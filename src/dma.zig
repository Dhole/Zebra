const std = @import("std");

const _ram = @import("ram.zig");
const Ram = _ram.Ram;

const _gpu = @import("gpu.zig");
const Gpu = _gpu.Gpu;

const ADDR_IO_PORTS: u32 = 0x1f80_1000;

const ChanCtl = struct {
    v: u32,

    // 1f801088+N*10 - D#_CHCR - DMA Channel Control (Channel 0..6) (R/W)
    const TRANS_DIR: u8 = 0; // 0       Transfer Direction    (0=To Main RAM, 1=From Main RAM)
    const Direction = enum(u1) {
        to_ram = 0,
        from_ram = 1,
    };
    const MEM_ADDR_STEP: u8 = 1; // 1       Memory Address Step   (0=Forward;+4, 1=Backward;-4)
    const Step = enum(u1) {
        inc = 0,
        dec = 1,
    };

    // 2-7     Not used              (always zero)
    const CHOP_EN = 8; // 8       Chopping Enable       (0=Normal, 1=Chopping; run CPU during DMA gaps)
    const SYNC_MODE = 9; // 9-10    SyncMode, Transfer Synchronisation/Mode (0-3):
    const Sync = enum(u2) {
        manual = 0, //      Start immediately and transfer all at once (used for CDROM, OTC)
        request = 1, //     Sync blocks to DMA requests   (used for MDEC, SPU, and GPU-data)
        linked_list = 2, // Linked-List mode              (used for GPU-command-lists)
        reserved = 3, //    Reserved                      (not used)
    };
    // 11-15   Not used              (always zero)
    const CHOP_DMA_WIN_SIZE = 16; // 16-18   Chopping DMA Window Size (1 SHL N words)
    // 19      Not used              (always zero)
    const CHOP_CPU_WIN_SIZE = 20; // 20-22   Chopping CPU Window Size (1 SHL N clks)
    // 23      Not used              (always zero)
    const START_BUSY = 24; // 24      Start/Busy            (0=Stopped/Completed, 1=Start/Enable/Busy)
    // 25-27   Not used              (always zero)
    const START_TRIG = 28; // 28      Start/Trigger         (0=Normal, 1=Manual Start; use for SyncMode=0)
    // 29      Unknown (R/W) Pause?  (0=No, 1=Pause?)     (For SyncMode=0 only?)
    // 30      Unknown (R/W)
    // 31      Not used              (always zero)

    const Self = @This();

    fn init() Self {
        return Self{ .v = 0 };
    }

    fn set(self: *Self, v: u32) void {
        self.v = v;
    }

    fn trans_dir_to_ram(self: *Self) bool {
        return ((self.v >> TRANS_DIR) & 0b1) == 0;
    }

    fn mem_addr_step_inc(self: *Self) bool {
        return ((self.v >> MEM_ADDR_STEP) & 0b1) == 0;
    }

    fn sync_mode(self: *Self) Sync {
        return @enumFromInt(self.v >> SYNC_MODE & 0b11);
    }

    fn enable(self: *Self) bool {
        return ((self.v >> START_BUSY) & 0b1) == 1;
    }
    fn set_enable_false(self: *Self) void {
        const mask: u32 = @intCast(1 << START_BUSY);
        self.v &= ~mask;
    }

    fn trigger(self: *Self) bool {
        return ((self.v >> START_TRIG) & 0b1) == 1;
    }
    fn set_trigger_false(self: *Self) void {
        const mask: u32 = @intCast(1 << START_TRIG);
        self.v &= ~mask;
    }

    fn active(self: *Self) bool {
        const start = if (self.sync_mode() == Sync.manual) self.trigger() else true;
        return self.enable() and start;
    }

    // Set channel status to "completed" state
    fn set_done(self: *Self) void {
        self.set_enable_false();
        self.set_trigger_false();

        // TODO: Need to set the correct value for the other fields (in
        // particular interrupts)
    }
};

const BaseAddr = struct {
    v: u32,

    // 1F801080h+N*10h - D#_MADR - DMA base address (Channel 0..6) (R/W)
    // 0-23  Memory Address where the DMA will start reading from/writing to
    // 24-31 Not used (always zero)

    const Self = @This();

    fn init() Self {
        return Self{ .v = 0 };
    }

    fn set(self: *Self, v: u32) void {
        self.v = v & 0xff_ffff;
    }
};

const BlkCtl = struct {
    v: u32,

    // 1F801084h+N*10h - D#_BCR - DMA Block Control (Channel 0..6) (R/W)
    // For SyncMode=0 (ie. for OTC and CDROM):
    //   0-15  BC    Number of words (0001h..FFFFh) (or 0=10000h words)
    //   16-31 0     Not used (usually 0 for OTC, or 1 ("one block") for CDROM)
    // For SyncMode=1 (ie. for MDEC, SPU, and GPU-vram-data):
    //   0-15  BS    Blocksize (words) ;for GPU/SPU max 10h, for MDEC max 20h
    //   16-31 BA    Amount of blocks  ;ie. total length = BS*BA words
    // For SyncMode=2 (ie. for GPU-command-lists):
    //   0-31  0     Not used (should be zero) (transfer ends at END-CODE in list)

    const Self = @This();

    fn init() Self {
        return Self{ .v = 0 };
    }

    fn set(self: *Self, v: u32) void {
        self.v = v;
    }

    fn sync_manual_num_words(self: *Self) usize {
        const n = self.v & 0xffff_ffff;
        return if (n == 0) 0x10000 else n;
    }

    fn sync_request_blocksize(self: *Self) usize {
        return self.v & 0xffff_ffff;
    }

    fn sync_request_amount_blocks(self: *Self) usize {
        return self.v >> 16;
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
        return switch (self.chan_ctl.sync_mode()) {
            ChanCtl.Sync.manual => self.blk_ctl.sync_manual_num_words(),
            ChanCtl.Sync.request => self.blk_ctl.sync_request_blocksize() * self.blk_ctl.sync_request_amount_blocks(),
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

const Control = struct {
    v: u32,

    // 1f8010f0 - DPCR - DMA Control Register (R/W)
    const CTL_DMA0_PRIORITY: u8 = 0; //       0-2   DMA0, MDECin  Priority      (0..7; 0=Highest, 7=Lowest)
    const CTL_DMA0_MASTER_ENABLE: u8 = 3; //  3     DMA0, MDECin  Master Enable (0=Disable, 1=Enable)
    const CTL_DMA1_PRIORITY: u8 = 4; //       4-6   DMA1, MDECout Priority      (0..7; 0=Highest, 7=Lowest)
    const CTL_DMA1_MASTER_ENABLE: u8 = 7; //  7     DMA1, MDECout Master Enable (0=Disable, 1=Enable)
    const CTL_DMA2_PRIORITY: u8 = 8; //       8-10  DMA2, GPU     Priority      (0..7; 0=Highest, 7=Lowest)
    const CTL_DMA2_MASTER_ENABLE: u8 = 11; // 11    DMA2, GPU     Master Enable (0=Disable, 1=Enable)
    const CTL_DMA3_PRIORITY: u8 = 12; //      12-14 DMA3, CDROM   Priority      (0..7; 0=Highest, 7=Lowest)
    const CTL_DMA3_MASTER_ENABLE: u8 = 15; // 15    DMA3, CDROM   Master Enable (0=Disable, 1=Enable)
    const CTL_DMA4_PRIORITY: u8 = 16; //      16-18 DMA4, SPU     Priority      (0..7; 0=Highest, 7=Lowest)
    const CTL_DMA4_MASTER_ENABLE: u8 = 19; // 19    DMA4, SPU     Master Enable (0=Disable, 1=Enable)
    const CTL_DMA5_PRIORITY: u8 = 20; //      20-22 DMA5, PIO     Priority      (0..7; 0=Highest, 7=Lowest)
    const CTL_DMA5_MASTER_ENABLE: u8 = 23; // 23    DMA5, PIO     Master Enable (0=Disable, 1=Enable)
    const CTL_DMA6_PRIORITY: u8 = 24; //      24-26 DMA6, OTC     Priority      (0..7; 0=Highest, 7=Lowest)
    const CTL_DMA6_MASTER_ENABLE: u8 = 27; // 27    DMA6, OTC     Master Enable (0=Disable, 1=Enable)
    // 28-30 Unknown, Priority Offset or so? (R/W)
    // 31    Unknown, no effect? (R/W)

    const Self = @This();

    fn init() Self {
        return Self{ .v = 0x07654321 };
    }

    fn set(self: *Self, v: u32) void {
        self.v = v;
    }
};

pub const Dma = struct {
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
                    0x0 => self.channels[port].base_addr.v,
                    0x4 => self.channels[port].blk_ctl.v,
                    0x8 => self.channels[port].chan_ctl.v,
                    else => unreachable,
                };
            },
            REG_DPCR => self.control.v,
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
        switch (channel.chan_ctl.sync_mode()) {
            ChanCtl.Sync.linked_list => self.dma_linked_list(port, channel),
            else => self.dma_block(port, channel),
        }
    }

    fn dma_block(self: *Self, port: Port, channel: *Chan) void {
        const increment: i32 = if (channel.chan_ctl.mem_addr_step_inc()) 4 else -4;
        var addr = channel.base_addr.v;

        // Transfer size in words
        // NOTE: We never call this in Linked-List mode, which would return null in transfer_size()
        var remsz = channel.transfer_size() orelse unreachable;

        while (remsz > 0) {
            // Mednafen mask the address this way
            const cur_addr = addr & 0x1f_fffc;

            if (channel.chan_ctl.trans_dir_to_ram()) {
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
            } else {
                const src_word = self.ram.read(u32, cur_addr);
                switch (port) {
                    Port.gpu => self.gpu.gp0(src_word),
                    else => std.debug.panic("TODO: DMA destination-port {}", .{port}),
                }
            }

            addr +%= @bitCast(increment);
            remsz -= 1;
        }

        channel.chan_ctl.set_done();
    }

    fn dma_linked_list(self: *Self, port: Port, channel: *Chan) void {
        var addr = channel.base_addr.v & 0x1f_fffc;

        if (channel.chan_ctl.trans_dir_to_ram()) {
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
