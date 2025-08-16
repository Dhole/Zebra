const std = @import("std");

const _ram = @import("ram.zig");
const Ram = _ram.Ram;

const _gpu = @import("gpu.zig");

const ADDR_IO_PORTS: u32 = 0x1f80_1000;

const ChanCtl = packed struct {
    const TransferDirection = enum(u1) {
        to_main_ram = 0,
        from_main_ram = 1,
    };
    const AddressStep = enum(u1) {
        forward = 0, // +4
        backward = 1, // -4
    };
    const SyncMode = enum(u2) {
        manual = 0,
        request = 1,
        linked_list = 2,
        reserved = 3,
    };
    // 0       Transfer Direction    (0=To Main RAM, 1=From Main RAM)
    transfer_direction: TransferDirection = TransferDirection.to_main_ram,
    // 1       Memory Address Step   (0=Forward;+4, 1=Backward;-4)
    memory_address_step: AddressStep = AddressStep.forward,
    // 2-7     Not used              (always zero)
    unused_0: u6 = 0,
    // 8       Chopping Enable       (0=Normal, 1=Chopping; run CPU during DMA gaps)
    chopping_enable: u1 = 0,
    // 9-10    SyncMode, Transfer Synchronisation/Mode (0-3):
    //           0  Start immediately and transfer all at once (used for CDROM, OTC)
    //           1  Sync blocks to DMA requests   (used for MDEC, SPU, and GPU-data)
    //           2  Linked-List mode              (used for GPU-command-lists)
    //           3  Reserved                      (not used)
    sync_mode: SyncMode = SyncMode.manual,
    // 11-15   Not used              (always zero)
    unused_1: u5 = 0,
    // 16-18   Chopping DMA Window Size (1 SHL N words)
    chopping_dma_window_size: u3 = 0,
    // 19      Not used              (always zero)
    unused_2: u1 = 0,
    // 20-22   Chopping CPU Window Size (1 SHL N clks)
    chopping_cpu_window_size: u3 = 0,
    // 23      Not used              (always zero)
    unused_3: u1 = 0,
    // 24      Start/Busy            (0=Stopped/Completed, 1=Start/Enable/Busy)
    enable: bool = false,
    // 25-27   Not used              (always zero)
    unused_4: u3 = 0,
    // 28      Start/Trigger         (0=Normal, 1=Manual Start; use for SyncMode=0)
    trigger: bool = false,
    // 29      Unknown (R/W) Pause?  (0=No, 1=Pause?)     (For SyncMode=0 only?)
    unknown_0: u1 = 0,
    // 30      Unknown (R/W)
    unknown_1: u1 = 0,
    // 31      Not used              (always zero)
    unused_5: u1 = 0,

    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }

    const Self = @This();

    fn init() Self {
        return Self{};
    }

    fn set(self: *Self, v: u32) void {
        self.* = @bitCast(v);
    }

    fn get(self: *Self) u32 {
        return @bitCast(self.*);
    }

    fn active(self: *Self) bool {
        const start = if (self.sync_mode == SyncMode.manual) self.trigger else true;
        return self.enable and start;
    }

    // Set channel status to "completed" state
    fn set_done(self: *Self) void {
        self.enable = false;
        self.trigger = false;

        // TODO: Need to set the correct value for the other fields (in
        // particular interrupts)
    }
};

// 1F801080h+N*10h - D#_MADR - DMA base address (Channel 0..6) (R/W)
const BaseAddr = packed struct {
    // 0-23  Memory Address where the DMA will start reading from/writing to
    address: u24 = 0,
    // 24-31 Not used (always zero)
    unused: u8 = 0,

    const Self = @This();

    fn init() Self {
        return Self{};
    }

    fn set(self: *Self, v: u32) void {
        self.address = @as(Self, @bitCast(v)).address;
    }

    fn get(self: *Self) u32 {
        return @bitCast(self.*);
    }
};

// 1F801084h+N*10h - D#_BCR - DMA Block Control (Channel 0..6) (R/W)
const BlkCtl = packed union {
    // For SyncMode=0 (ie. for OTC and CDROM):
    const Mode0 = packed struct {
        //   0-15  BC    Number of words (0001h..FFFFh) (or 0=10000h words)
        bc: u16,
        //   16-31 0     Not used (usually 0 for OTC, or 1 ("one block") for CDROM)
        unused: u16,
    };
    // For SyncMode=1 (ie. for MDEC, SPU, and GPU-vram-data):
    const Mode1 = packed struct {
        //   0-15  BS    Blocksize (words) ;for GPU/SPU max 10h, for MDEC max 20h
        bs: u16,
        //   16-31 BA    Amount of blocks  ;ie. total length = BS*BA words
        ba: u16,
    };
    // For SyncMode=2 (ie. for GPU-command-lists):
    const Mode2 = packed struct {
        //   0-31  0     Not used (should be zero) (transfer ends at END-CODE in list)
        unused: u32,
    };

    mode_0: Mode0,
    mode_1: Mode1,
    mode_2: Mode2,

    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }

    const Self = @This();

    fn init() Self {
        return @bitCast(@as(u32, 0));
    }

    fn set(self: *Self, v: u32) void {
        self.* = @bitCast(v);
    }

    fn get(self: *Self) u32 {
        return @bitCast(self.*);
    }

    fn sync_manual_num_words(self: *Self) usize {
        const n = self.mode_0.bc;
        return if (n == 0) 0x10000 else n;
    }

    fn sync_request_blocksize(self: *Self) usize {
        return self.mode_1.bs;
    }

    fn sync_request_amount_blocks(self: *Self) usize {
        return self.mode_1.ba;
    }
};

const Chan = struct {
    base_addr: BaseAddr,
    blk_ctl: BlkCtl,
    chan_ctl: ChanCtl,

    const Self = @This();

    fn init() Self {
        return Self{
            .base_addr = BaseAddr.init(),
            .blk_ctl = BlkCtl.init(),
            .chan_ctl = ChanCtl.init(),
        };
    }

    // DMA transfer size in words or null for linked list mode.
    fn transfer_size(self: *Self) ?usize {
        return switch (self.chan_ctl.sync_mode) {
            ChanCtl.SyncMode.manual => self.blk_ctl.sync_manual_num_words(),
            ChanCtl.SyncMode.request => self.blk_ctl.sync_request_blocksize() * self.blk_ctl.sync_request_amount_blocks(),
            // In linked list mode the size is not known ahead of time: we stop
            // when we encounter the "end of list" marker (0xffffff).
            else => null,
        };
    }
};

const Interrupt = struct {
    v: u32,

    // 1f8010f4 - DICR - DMA Interrupt Register (R/W)
    // 0-5   Unknown  (read/write-able)
    // 6-14  Not used (always zero)
    const FORCE_IRQ: u8 = 15; //    15    Force IRQ (sets bit31)                         (0=None, 1=Force Bit31=1)
    const IRQ_EN_FLAGS: u8 = 16; // 16-22 IRQ Enable setting bit24-30 upon DMA0..DMA6    (0=None, 1=Enable)
    const IRQ_EN_SIG: u8 = 23; //   23    IRQ Enable setting bit31 when bit24-30=nonzero (0=None, 1=Enable)
    const IRQ_FLAGS: u8 = 24; //    24-30 IRQ Flags for DMA0..DMA6    (Write 1 to reset) (0=None, 1=IRQ)
    const IRQ_SIG: u8 = 31; //      31    IRQ Signal (0-to-1 triggers 1f801070.bit3)     (0=None, 1=IRQ) (R)

    const Self = @This();

    fn init() Self {
        return Self{ .v = 0 };
    }

    fn set(self: *Self, v: u32) void {
        self.v = v;
    }

    fn irq_flags(self: *Self) u8 {
        return (self.interrupt >> IRQ_FLAGS) & 0b111_1111;
    }

    fn irq_en_flags(self: *Self) u8 {
        return (self.interrupt >> IRQ_EN_FLAGS) & 0b111_1111;
    }

    fn force_irq(self: *Self) bool {
        return (self.interrupt & FORCE_IRQ) != 0;
    }

    fn irq_en_sig(self: *Self) bool {
        return (self.interrupt & IRQ_EN_SIG) != 0;
    }
};

const Control = packed struct {
    // 0-2   DMA0, MDECin  Priority      (0..7; 0=Highest, 7=Lowest)
    dma0_mdecin_prio: u3,
    // 3     DMA0, MDECin  Master Enable (0=Disable, 1=Enable)
    dma0_mdecin_master_enable: bool,
    // 4-6   DMA1, MDECout Priority      (0..7; 0=Highest, 7=Lowest)
    dma1_mdecout_prio: u3,
    // 7     DMA1, MDECout Master Enable (0=Disable, 1=Enable)
    dma1_mdecout_master_enable: bool,
    // 8-10  DMA2, GPU     Priority      (0..7; 0=Highest, 7=Lowest)
    dma2_gpu_prio: u3,
    // 11    DMA2, GPU     Master Enable (0=Disable, 1=Enable)
    dma2_gpu_master_enable: bool,
    // 12-14 DMA3, CDROM   Priority      (0..7; 0=Highest, 7=Lowest)
    dma3_cdrom_prio: u3,
    // 15    DMA3, CDROM   Master Enable (0=Disable, 1=Enable)
    dma3_cdrom_master_enable: bool,
    // 16-18 DMA4, SPU     Priority      (0..7; 0=Highest, 7=Lowest)
    dma4_spu_prio: u3,
    // 19    DMA4, SPU     Master Enable (0=Disable, 1=Enable)
    dma4_spu_master_enable: bool,
    // 20-22 DMA5, PIO     Priority      (0..7; 0=Highest, 7=Lowest)
    dma5_pio_prio: u3,
    // 23    DMA5, PIO     Master Enable (0=Disable, 1=Enable)
    dma5_pio_master_enable: bool,
    // 24-26 DMA6, OTC     Priority      (0..7; 0=Highest, 7=Lowest)
    dma6_otc_prio: u3,
    // 27    DMA6, OTC     Master Enable (0=Disable, 1=Enable)
    dma6_otc_master_enable: bool,
    // 28-30 Unknown, Priority Offset or so? (R/W)
    unknown_0: u3,
    // 31    Unknown, no effect? (R/W)
    unknown_1: u1,

    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }

    const Self = @This();

    fn init() Self {
        var v: Self = undefined;
        v.set(0x07654321);
        return v;
    }

    fn set(self: *Self, v: u32) void {
        self.* = @bitCast(v);
    }

    fn get(self: *Self) u32 {
        return @bitCast(self.*);
    }
};

pub fn Dma(comptime Renderer: type) type {
    const Gpu = _gpu.Gpu(Renderer);

    return struct {
        pub const ADDR_START: u16 = 0x0080;
        pub const ADDR_END: u16 = 0x00ff;

        const Port = enum(u8) {
            mdec_in = 0,
            mdec_out = 1,
            gpu = 2,
            cdrom = 3,
            spu = 4,
            pio = 5,
            otc = 6,
            _,
        };

        const REG_DMA_START: u16 = 0x0080;
        // 1f80108x DMA0 channel 0 - MDECin (RAM to MDEC)
        // 1f80109x DMA1 channel 1 - MDECout (MDEC to RAM)
        // 1f8010ax DMA2 channel 2 - GPU (lists + image data)
        // 1f8010bx DMA3 channel 3 - CDROM
        // 1f8010cx DMA4 channel 4 - SPU
        // 1f8010dx DMA5 channel 5 - PIO (Expansion Port)
        // 1f8010ex DMA6 channel 6 - OTC (reverse clear OT) (GPU related)
        const REG_DMA_END: u16 = 0x00ef;
        const REG_DPCR: u16 = 0x00f0; // 1f8010f0 DPCR - DMA Control register
        const REG_DICR: u16 = 0x00f4; // 1f8010f4 DICR - DMA Interrupt register
        // 1f8010f8 unknown
        // 1f8010fc unknown

        channels: [7]Chan, // 1f801080 - 1f8010ec - DMA Channels registers
        control: Control, // 1f8010f0 DPCR - DMA Control register
        interrupt: Interrupt, // 1f8010f4 DICR - DMA Interrupt register

        // Port devices
        ram: Ram,
        gpu: *Gpu,

        const Self = @This();

        pub fn init(ram: Ram, gpu: *Gpu) Self {
            return Self{
                .channels = .{Chan.init()} ** 7,
                .control = Control.init(),
                .interrupt = Interrupt.init(),
                .ram = ram,
                .gpu = gpu,
            };
        }

        fn irq(self: *Self) bool {
            const channel_irq = self.interrupt.irq_flags() & self.interrupt.irq_en_flags();
            return self.interrupt.force_irq() or
                (self.interrupt.irq_en_sig() and (channel_irq != 0));
        }

        pub fn read_u32(self: *Self, addr: u16) u32 {
            return switch (addr) {
                REG_DMA_START...REG_DMA_END => blk: {
                    const offset = addr - REG_DMA_START;
                    const port = offset >> 4;
                    const reg = offset & 0x000f;
                    break :blk switch (reg) {
                        0x0 => self.channels[port].base_addr.get(),
                        0x4 => self.channels[port].blk_ctl.get(),
                        0x8 => self.channels[port].chan_ctl.get(),
                        else => unreachable,
                    };
                },
                REG_DPCR => self.control.get(),
                REG_DICR => self.interrupt.v,
                else => unreachable,
            };
        }
        pub fn write_u32(self: *Self, addr: u16, v: u32) void {
            switch (addr) {
                REG_DMA_START...REG_DMA_END => {
                    const offset = addr - REG_DMA_START;
                    const port = offset >> 4;
                    const reg = offset & 0x000f;
                    var channel = &self.channels[port];
                    switch (reg) {
                        0x0 => channel.base_addr.set(v),
                        0x4 => channel.blk_ctl.set(v),
                        0x8 => channel.chan_ctl.set(v),
                        else => unreachable,
                    }
                    if (channel.chan_ctl.active()) {
                        self.dma_transfer(@enumFromInt(port), channel);
                    }
                },
                REG_DPCR => self.control.set(v),
                REG_DICR => self.interrupt.set(v),
                else => unreachable,
            }
        }

        // Execute a DMA transfer for a port
        fn dma_transfer(self: *Self, port: Port, channel: *Chan) void {
            // DMA transfer has been started, for now let's process everything in
            // one pass (i.e. no chopping or priority handling)
            switch (channel.chan_ctl.sync_mode) {
                ChanCtl.SyncMode.linked_list => self.dma_linked_list(port, channel),
                else => self.dma_block(port, channel),
            }
        }

        fn dma_block(self: *Self, port: Port, channel: *Chan) void {
            const increment: i32 = switch (channel.chan_ctl.memory_address_step) {
                ChanCtl.AddressStep.forward => 4,
                ChanCtl.AddressStep.backward => -4,
            };
            var addr = channel.base_addr.get();

            // Transfer size in words
            // NOTE: We never call this in Linked-List mode, which would return null in transfer_size()
            var remsz = channel.transfer_size() orelse unreachable;
            std.debug.print("DBG dma_block size={}\n", .{remsz});

            while (remsz > 0) {
                // Mednafen mask the address this way
                const cur_addr = addr & 0x1f_fffc;

                switch (channel.chan_ctl.transfer_direction) {
                    ChanCtl.TransferDirection.to_main_ram => {
                        const src_word = switch (port) {
                            // Clear ordering table
                            Port.otc => switch (remsz) {
                                // Last entry (increment = -4) contains the end of table marker
                                1 => 0xff_ffff,
                                // Pointer to the previous entry
                                else => (addr -% 4) & 0x1f_fffc,
                            },
                            else => std.debug.panic("TODO: DMA source-port {}", .{port}),
                        };
                        self.ram.write(u32, cur_addr, src_word);
                    },
                    ChanCtl.TransferDirection.from_main_ram => {
                        const src_word = self.ram.read(u32, cur_addr);
                        switch (port) {
                            Port.gpu => self.gpu.gp0(src_word),
                            else => std.debug.panic("TODO: DMA destination-port {}", .{port}),
                        }
                    },
                }

                addr +%= @bitCast(increment);
                remsz -= 1;
            }

            channel.chan_ctl.set_done();
        }

        fn dma_linked_list(self: *Self, port: Port, channel: *Chan) void {
            var addr = channel.base_addr.get() & 0x1f_fffc;

            if (channel.chan_ctl.transfer_direction == ChanCtl.TransferDirection.to_main_ram) {
                std.debug.panic("Invalid DMA direction for linked-list mode", .{});
            }

            // I don't know if the DMA even supports linked-list mode for anything besides the GPU
            if (port != Port.gpu) {
                std.debug.panic("Attempted linked-list DMA on port {}", .{port});
            }

            while (true) {
                // In linked-list mode each entry starts with a "header" word.  The
                // high byte contains the number of words in the "packet" (not
                // counting the header word).
                const header = self.ram.read(u32, addr);
                var remsz = header >> 24;

                while (remsz > 0) {
                    addr = (addr +% 4) & 0x1f_fffc;
                    const command = self.ram.read(u32, addr);
                    self.gpu.gp0(command);
                    remsz -= 1;
                }

                // The end of list marker is usually 0xffffff but mednafen only
                // checks for the MSB so maybe that's what the hardware does?
                // Since this bit is not part of any valid address it makes some
                // sense.  TODO: Test that at some point
                if (header & 0x80_0000 != 0) {
                    break;
                }
                addr = header & 0x1f_fffc;
            }

            channel.chan_ctl.set_done();
        }
    };
}
