const std = @import("std");
const log = std.log;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const atomic = std.atomic;
const AtomicOrder = std.builtin.AtomicOrder;

const root = @import("root.zig");
const Inst = root.Inst;
const RegIdx = root.RegIdx;
const InstArgs = root.InstArgs;
const Op = root.Op;
const buf_read = root.buf_read;

const decoder = @import("decoder.zig");
const decode = decoder.decode;

const disasm = @import("disasm.zig");
const fmt_reg = disasm.fmt_reg;
const fmt_inst = disasm.fmt_inst;
const print_disasm = disasm.print_disasm;

const _ram = @import("ram.zig");
const Ram = _ram.Ram;
const SIZE_RAM = _ram.SIZE_RAM;

const _dma = @import("dma.zig");

const _gpu = @import("gpu.zig");

pub const SIZE_BIOS: usize = 512 * 1024; // 512 KiB
pub const SIZE_EXP_REG1: usize = 8 * 1024; // 8 KiB
pub const SIZE_SCRATCH: usize = 1024; // 1 KiB
pub const SIZE_IO_PORTS: usize = 8 * 1024; // 8 KiB
pub const SIZE_EXP_REG2: usize = 8 * 1024; // 8 KiB
pub const SIZE_EXP_REG3: usize = 2 * 1024 * 1024; // 2 MiB
pub const SIZE_CACHECTL: usize = 512; // 0.5 KiB
// Physical addresses
pub const ADDR_RAM: u32 = 0x0000_0000;
pub const ADDR_EXP_REG1: u32 = 0x1f00_0000;
pub const ADDR_SCRATCH: u32 = 0x1f80_0000;
pub const ADDR_IO_PORTS: u32 = 0x1f80_1000;
pub const ADDR_EXP_REG2: u32 = 0x1f80_2000;
pub const ADDR_EXP_REG3: u32 = 0x1fa0_0000;
pub const ADDR_BIOS: u32 = 0x1fc0_0000;
pub const ADDR_CACHECTL: u32 = 0xfffe_0000;

pub const VADDR_KUSEG: u32 = 0x0000_0000;
pub const VADDR_KSEG0: u32 = 0x8000_0000;
pub const VADDR_KSEG1: u32 = 0xa000_0000;
pub const VADDR_RESET: u32 = VADDR_KSEG1 + ADDR_BIOS;

pub const REGION_MASK = [_]u32{
    // KUSEG: 2048 MB vaddr corresponds to paddr
    0xffff_ffff, 0xffff_ffff, 0xffff_ffff, 0xffff_ffff,
    // KSEG0:  512 MB
    0x1fff_ffff,
    // KSEG1:  512 MB
    0x1fff_ffff,
    // KSEG2: 1024 MB vaddr corresponds to paddr
    0xffff_ffff, 0xffff_ffff,
};

fn addr_region(addr: u32) usize {
    return addr >> 29;
}

fn vaddr_to_paddr(vaddr: u32) struct { usize, u32 } {
    const region = addr_region(vaddr);
    const paddr = vaddr & REGION_MASK[region];
    return .{ region, paddr };
}

pub const Cfg = struct {
    dbg: bool = false,
};

const Dbg = struct {
    trace_inst: bool = false,
    trace_io: bool = false,
    breaks: ArrayList(u32),
    _break: atomic.Value(bool) = atomic.Value(bool).init(false),
    _first_step: bool = true,

    fn init(allocator: Allocator) Dbg {
        return Dbg{
            .breaks = ArrayList(u32).init(allocator),
        };
    }

    fn deinit(self: *Dbg) void {
        self.breaks.deinit();
    }
};

//  0     IEc Current Interrupt Enable  (0=Disable, 1=Enable) ;rfe pops IUp here
//  1     KUc Current Kernal/User Mode  (0=Kernel, 1=User)    ;rfe pops KUp here
//  2     IEp Previous Interrupt Disable                      ;rfe pops IUo here
//  3     KUp Previous Kernal/User Mode                       ;rfe pops KUo here
//  4     IEo Old Interrupt Disable                       ;left unchanged by rfe
//  5     KUo Old Kernal/User Mode                        ;left unchanged by rfe
//  6-7   -   Not used (zero)
//  8-15  Im  8 bit interrupt mask fields. When set the corresponding
//            interrupts are allowed to cause an exception.
const COP0_SR_ISC = 16; // Isolate Cache (0=No, 1=Isolate)
//              When isolated, all load and store operations are targetted
//              to the Data cache, and never the main memory.
//              (Used by PSX Kernel, in combination with Port FFFE0130h)
//  17    Swc Swapped cache mode (0=Normal, 1=Swapped)
//              Instruction cache will act as Data cache and vice versa.
//              Use only with Isc to access & invalidate Instr. cache entries.
//              (Not used by PSX Kernel)
//  18    PZ  When set cache parity bits are written as 0.
//  19    CM  Shows the result of the last load operation with the D-cache
//            isolated. It gets set if the cache really contained data
//            for the addressed memory location.
//  20    PE  Cache parity error (Does not cause exception)
//  21    TS  TLB shutdown. Gets set if a programm address simultaneously
//            matches 2 TLB entries.
//            (initial value on reset allows to detect extended CPU version?)
const COP0_SR_BEV = 22; // BEV Boot exception vectors in RAM/ROM (0=RAM/KSEG0, 1=ROM/KSEG1)
//  23-24 -   Not used (zero)
//  25    RE  Reverse endianness   (0=Normal endianness, 1=Reverse endianness)
//              Reverses the byte order in which data is stored in
//              memory. (lo-hi -> hi-lo)
//              (Has affect only to User mode, not to Kernal mode) (?)
//              (The bit doesn't exist in PSX ?)
//  26-27 -   Not used (zero)
//  28    CU0 COP0 Enable (0=Enable only in Kernal Mode, 1=Kernal and User Mode)
//  29    CU1 COP1 Enable (0=Disable, 1=Enable) (none such in PSX)
//  30    CU2 COP2 Enable (0=Disable, 1=Enable) (GTE in PSX)
//  31    CU3 COP3 Enable (0=Disable, 1=Enable) (none such in PSX)

//  0-1   -      Not used (zero)
//  2-6   Excode Describes what kind of exception occured:
//                 00h INT     Interrupt
//                 01h MOD     Tlb modification (none such in PSX)
//                 02h TLBL    Tlb load         (none such in PSX)
//                 03h TLBS    Tlb store        (none such in PSX)
//                 04h AdEL    Address error, Data load or Instruction fetch
//                 05h AdES    Address error, Data store
//                             The address errors occur when attempting to read
//                             outside of KUseg in user mode and when the address
//                             is misaligned. (See also: BadVaddr register)
//                 06h IBE     Bus error on Instruction fetch
//                 07h DBE     Bus error on Data load/store
//                 08h Syscall Generated unconditionally by syscall instruction
//                 09h BP      Breakpoint - break instruction
//                 0Ah RI      Reserved instruction
//                 0Bh CpU     Coprocessor unusable
//                 0Ch Ov      Arithmetic overflow
//                 0Dh-1Fh     Not used
//  7     -      Not used (zero)
//  8-15  Ip     Interrupt pending field. Bit 8 and 9 are R/W, and
//               contain the last value written to them. As long
//               as any of the bits are set they will cause an
//               interrupt if the corresponding bit is set in IM.
//  16-27 -      Not used (zero)
//  28-29 CE     Opcode Bit26-27 (aka coprocessor number in case of COP opcodes)
//  30    -      Not used (zero) / Undoc: When BD=1, Branch condition (0=False)
const COP0_CAUSE_BD = 31; // BD Branch Delay (set when last exception points to the branch
//               instruction instead of the instruction in the branch delay
//               slot, where the exception occurred)

const Exception = enum(u8) {
    int = 0x00, // INT     Interrupt
    mod = 0x01, // MOD     Tlb modification (none such in PSX)
    tlbl = 0x02, // TLBL    Tlb load         (none such in PSX)
    tlbs = 0x03, // TLBS    Tlb store        (none such in PSX)
    adel = 0x04, // AdEL    Address error, Data load or Instruction fetch
    ades = 0x05, // AdES    Address error, Data store
    //                          The address errors occur when attempting to read
    //                          outside of KUseg in user mode and when the address
    //                          is misaligned. (See also: BadVaddr register)
    ibe = 0x06, // IBE     Bus error on Instruction fetch
    dbe = 0x07, // DBE     Bus error on Data load/store
    syscall = 0x08, // Syscall Generated unconditionally by syscall instruction
    bp = 0x09, // BP      Breakpoint - break instruction
    ri = 0x0a, // RI      Reserved instruction
    cpu = 0x0b, // CpU     Coprocessor unusable
    ov = 0x0c, // Ov      Arithmetic overflow
};

const Cop0 = struct {
    // COP0 Register Summary
    const Reg = enum(u8) {
        bpc = 3, // Breakpoint on execute (R/W)
        bda = 5, // Breakpoint on data access (R/W)
        jumpdest = 6, // Randomly memorized jump address (R)
        dcic = 7, // Breakpoint control (R/W)
        bad_vaddr = 8, // Bad Virtual Address (R)
        bdam = 9, // Data Access breakpoint mask (R/W)
        bpcm = 11, // Execute breakpoint mask (R/W)
        sr = 12, // System status register (R/W)
        cause = 13, // Describes the most recently recognised exception (R)
        epc = 14, // Return Address from Trap (R)
        prid = 15, // Processor ID (R)
        _,
    };

    status_reg: u32, // 12=SR
    cause: u32, // 13=CAUSE
    exception_pc: u32, // 14=EPC
    //

    const Self = @This();

    fn init() Self {
        return Self{
            .status_reg = 0,
            .cause = 0,
            .exception_pc = 0,
        };
    }

    fn set_r(self: *Self, idx: RegIdx, v: u32) void {
        const reg: Reg = @enumFromInt(idx[0]);
        switch (reg) {
            Reg.bpc, Reg.bda, Reg.jumpdest, Reg.dcic, Reg.bdam, Reg.bpcm => {
                if (v != 0) {
                    std.debug.panic("TODO: cop0 write reg {} {x:0>8}", .{ idx[0], v });
                }
            },
            Reg.sr => self.status_reg = v,
            Reg.cause => std.debug.print("WARN: write to cop0.cause", .{}),
            Reg.epc => std.debug.print("WARN: write to cop0.epc", .{}),
            else => std.debug.panic("TODO: cop0 set_r {} {x:0>8}", .{ idx[0], v }),
        }
    }

    fn r(self: *Self, idx: RegIdx) u32 {
        const reg: Reg = @enumFromInt(idx[0]);
        return switch (reg) {
            Reg.sr => self.status_reg,
            Reg.cause => self.cause,
            Reg.epc => self.exception_pc,
            else => std.debug.panic("TODO: cop0 r {}", .{idx[0]}),
        };
    }
};

pub fn Cpu(comptime DbgWriter: type, comptime Renderer: type, comptime cfg: Cfg) type {
    return struct {
        const Gpu = _gpu.Gpu(Renderer);
        const Dma = _dma.Dma(Renderer);

        dbg_w: DbgWriter,
        comptime cfg: Cfg = cfg,
        dbg: Dbg,
        regs: [32]u32, // Registers
        cop0: Cop0,
        dma: Dma,
        gpu: *Gpu,
        renderer: Renderer,
        pc: u32 = 0, // Current Program Counter
        next_pc: u32, // Next Program Counter
        next_next_pc: u32, // Next next Program Counter
        branch: bool = false, // Last instruction branched
        delay_slot: bool = false, // Current instruction is in a branch delay slot
        hi: u32, // Multiplication 64 bit high result or division remainder
        lo: u32, // Multiplication 64 bit low result or division quotient
        delay_dst_r: RegIdx = .{0}, // register to update from a load delay
        delay_dst_v: u32 = 0, // value from a load delay
        bios: []const u8,
        ram: Ram,
        allocator: Allocator,

        const Self = @This();

        pub fn init(
            allocator: Allocator,
            bios: []const u8,
            dbg_writer: DbgWriter,
        ) !Self {
            if (bios.len != SIZE_BIOS) {
                return error.BiosInvalidSize;
            }
            const ram = try Ram.init(allocator);
            const gpu = try allocator.create(Gpu);
            const renderer = try Renderer.init();
            gpu.* = Gpu.init(renderer);
            const bios_copy = try allocator.alloc(u8, SIZE_BIOS);
            std.mem.copyForwards(u8, bios_copy, bios);
            const pc = VADDR_RESET;
            const self = Self{
                .dbg_w = dbg_writer,
                .cfg = cfg,
                .dbg = Dbg.init(allocator),
                .regs = .{0} ** 32,
                .cop0 = Cop0.init(),
                .dma = Dma.init(ram, gpu),
                .gpu = gpu,
                .renderer = renderer,
                .next_pc = pc,
                .next_next_pc = pc +% 4,
                .lo = 0,
                .hi = 0,
                .bios = bios_copy,
                .ram = ram,
                .allocator = allocator,
            };
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.renderer.deinit();
            self.ram.deinit();
            self.allocator.destroy(self.gpu);
            self.allocator.free(self.bios);
            self.dbg.deinit();
        }

        pub fn r(self: *const Self, idx: RegIdx) u32 {
            return self.regs[idx[0]];
        }

        pub fn set_r(self: *Self, idx: RegIdx, v: u32) void {
            self.regs[idx[0]] = v;
            self.regs[0] = 0;
        }

        pub fn format_regs(self: *const Self, writer: anytype) !void {
            for (0..8) |row| {
                for (0..5) |column| {
                    if (column == 0) {
                        switch (row) {
                            0 => try writer.print("pc: {x:0>8}", .{self.pc}),
                            1 => try writer.print("lo: {x:0>8}", .{self.lo}),
                            2 => try writer.print("hi: {x:0>8}", .{self.hi}),
                            else => try writer.print("            ", .{}),
                        }
                    } else {
                        const idx: RegIdx = .{@intCast((column - 1) * 8 + row)};
                        try writer.print("   {d:0>2} {}: {x:0>8}", .{ idx[0], fmt_reg(idx), self.r(idx) });
                    }
                }
                try writer.print("\n", .{});
            }
        }

        // Run/Continue execution until a breakpoint
        pub fn dbg_run(self: *Self) void {
            if (!self.cfg.dbg) {
                @panic("dbg = false");
            }
            self.dbg._first_step = true;
            defer self.dbg._first_step = true;
            while (true) {
                self.step();
                if (self.dbg._break.load(AtomicOrder.unordered)) {
                    self.dbg_w.print("Breakpoint at {x:0>8}\n", .{self.pc}) catch @panic("write");
                    self.dbg._break.store(false, AtomicOrder.unordered);
                    return;
                }
                self.dbg._first_step = false;
            }
        }

        pub fn step(self: *Self) void {
            // Fetch next instruction
            const inst_raw = self.read(true, u32, self.next_pc) catch {
                self.exception(Exception.adel);
                return;
            };
            self.pc = self.next_pc;
            const maybe_inst = decode(inst_raw);
            if (self.cfg.dbg) {
                if (self.dbg.trace_inst) {
                    self.dbg_trace(maybe_inst, inst_raw) catch @panic("write");
                }
                if (!self.dbg._first_step) {
                    for (self.dbg.breaks.items) |addr| {
                        if (addr == self.pc) {
                            self.dbg._break.store(true, AtomicOrder.unordered);
                            return;
                        }
                    }
                }
            }
            if (maybe_inst) |inst| {
                self.exec(inst);
            } else {
                self.exception(Exception.ri);
            }
        }

        fn dbg_trace(self: *Self, maybe_inst: ?Inst, inst_raw: u32) !void {
            const inst = maybe_inst orelse {
                try self.dbg_w.print("{x:0>8}: {x:0>8} ???\n", .{ self.pc, inst_raw });
                return;
            };
            switch (inst) {
                .j, .jal, .mfc0 => {
                    // duckstation compatible
                    try print_disasm(self.dbg_w, self.pc, inst_raw, inst);
                    try self.dbg_w.print("\n", .{});
                    return;
                },
                else => {},
            }
            var disasm_str = [_]u8{' '} ** 48;
            var s = std.io.fixedBufferStream(disasm_str[0..]);
            try print_disasm(s.writer(), self.pc, inst_raw, inst);
            try self.dbg_w.print("{s} ; ", .{disasm_str});
            // NOTE: Using 0x prefix in values to be duckstation compatible
            switch (inst) {
                // rd, rs, rt
                .add, .addu, .sub, .subu, .slt, .sltu, .@"and", .@"or" => |a| try self.dbg_w.print(
                    "{}=0x{x:0>8}, {}=0x{x:0>8}, {}=0x{x:0>8}",
                    .{ fmt_reg(a.rd), self.r(a.rd), fmt_reg(a.rs), self.r(a.rs), fmt_reg(a.rt), self.r(a.rt) },
                ),
                // rt, rs
                .addi, .addiu, .slti, .sltiu, .andi, .ori, .xori => |a| try self.dbg_w.print(
                    "{}=0x{x:0>8}, {}=0x{x:0>8}",
                    .{ fmt_reg(a.rt), self.r(a.rt), fmt_reg(a.rs), self.r(a.rs) },
                ),
                // rs, rt
                .bne, .beq => |a| try self.dbg_w.print(
                    "{}=0x{x:0>8}, {}=0x{x:0>8}",
                    .{ fmt_reg(a.rs), self.r(a.rs), fmt_reg(a.rt), self.r(a.rt) },
                ),
                // rs, rt
                .div, .divu => |a| try self.dbg_w.print(
                    "{}=0x{x:0>8}, {}=0x{x:0>8}",
                    .{ fmt_reg(a.rs), self.r(a.rs), fmt_reg(a.rt), self.r(a.rt) },
                ),
                // rd, rt
                .sll, .srl, .sra => |a| try self.dbg_w.print(
                    "{}=0x{x:0>8}, {}=0x{x:0>8}",
                    .{ fmt_reg(a.rd), self.r(a.rd), fmt_reg(a.rt), self.r(a.rt) },
                ),
                // rs
                .bgtz, .bgez, .bltz, .blez => |a| try self.dbg_w.print("{}=0x{x:0>8}", .{ fmt_reg(a.rs), self.r(a.rs) }),
                // rs
                .jr => |a| try self.dbg_w.print("{}=0x{x:0>8}", .{ fmt_reg(a.rs), self.r(a.rs) }),
                // r31
                .jal => |_| try self.dbg_w.print("{}=0x{x:0>8}", .{ fmt_reg(.{31}), self.r(.{31}) }),
                // rd, rs
                .jalr => |a| try self.dbg_w.print(
                    "{}=0x{x:0>8}, {}=0x{x:0>8}",
                    .{ fmt_reg(a.rd), self.r(a.rd), fmt_reg(a.rs), self.r(a.rs) },
                ),
                // rt
                .lui => |a| try self.dbg_w.print("{}=0x{x:0>8}", .{ fmt_reg(a.rt), self.r(a.rt) }),
                // rd
                .mtc0, .mtc2 => |a| try self.dbg_w.print("{}=0x{x:0>8}", .{ fmt_reg(a.rt), self.r(a.rt) }),
                // rd, lo
                .mflo => |a| try self.dbg_w.print(
                    "{}=0x{x:0>8}, lo=0x{x:0>8}",
                    .{ fmt_reg(a.rd), self.r(a.rd), self.lo },
                ),
                // rd, hi
                .mfhi => |a| try self.dbg_w.print(
                    "{}=0x{x:0>8}, hi=0x{x:0>8}",
                    .{ fmt_reg(a.rd), self.r(a.rd), self.hi },
                ),
                // rs, lo
                .mtlo => |a| try self.dbg_w.print(
                    "lo=0x{x:0>8}, {}=0x{x:0>8}",
                    .{ self.lo, fmt_reg(a.rs), self.r(a.rs) },
                ),
                // rs, hi
                .mthi => |a| try self.dbg_w.print(
                    "hi=0x{x:0>8}, {}=0x{x:0>8}",
                    .{ self.hi, fmt_reg(a.rs), self.r(a.rs) },
                ),
                //
                //
                .j, .mfc0, .mfc2 => {},
                .lw => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    const v = try self.read(false, u32, addr);
                    try self.dbg_w.print(
                        "{}=0x{x:0>8}, addr=0x{x:0>8}[0x{x:0>8}]",
                        .{ fmt_reg(a.rt), self.r(a.rt), addr, v },
                    );
                },
                .lh, .lhu => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    const v = try self.read(false, u16, addr);
                    try self.dbg_w.print(
                        "{}=0x{x:0>8}, addr=0x{x:0>8}[0x{x:0>4}]",
                        .{ fmt_reg(a.rt), self.r(a.rt), addr, v },
                    );
                },
                .lb, .lbu => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    const v = try self.read(false, u8, addr);
                    try self.dbg_w.print(
                        "{}=0x{x:0>8}, addr=0x{x:0>8}[0x{x:0>2}]",
                        .{ fmt_reg(a.rt), self.r(a.rt), addr, v },
                    );
                },
                .sw, .sh, .sb => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    try self.dbg_w.print(
                        "{}=0x{x:0>8}, addr=0x{x:0>8}",
                        .{ fmt_reg(a.rt), self.r(a.rt), addr },
                    );
                },
                .sys => |a| {
                    _ = a;
                },
                .rfe => |a| {
                    _ = a;
                },
                .lwl => |_| try self.dbg_w.print("TODO", .{}),
                .lwr => |_| try self.dbg_w.print("TODO", .{}),
                .swl => |_| try self.dbg_w.print("TODO", .{}),
                .swr => |_| try self.dbg_w.print("TODO", .{}),
                .xor => |_| try self.dbg_w.print("TODO", .{}),
                .nor => |_| try self.dbg_w.print("TODO", .{}),
                .sllv => |_| try self.dbg_w.print("TODO", .{}),
                .srlv => |_| try self.dbg_w.print("TODO", .{}),
                .srav => |_| try self.dbg_w.print("TODO", .{}),
                .mult => |_| try self.dbg_w.print("TODO", .{}),
                .multu => |_| try self.dbg_w.print("TODO", .{}),
                .bltzal => |_| try self.dbg_w.print("TODO", .{}),
                .brk => |_| try self.dbg_w.print("TODO", .{}),
                .lwc0 => |_| try self.dbg_w.print("TODO", .{}),
                .swc0 => |_| try self.dbg_w.print("TODO", .{}),
                .lwc2 => |_| try self.dbg_w.print("TODO", .{}),
                .swc2 => |_| try self.dbg_w.print("TODO", .{}),
                .ctc2 => |_| try self.dbg_w.print("TODO", .{}),
                .cfc2 => |_| try self.dbg_w.print("TODO", .{}),
                .cop1 => |_| try self.dbg_w.print("TODO", .{}),
                .cop3 => |_| try self.dbg_w.print("TODO", .{}),
                .bgezal => |_| try self.dbg_w.print("TODO", .{}),
                // else => std.debug.panic("TODO dbg_trace {?}", .{inst}),
            }
            try self.dbg_w.print("\n", .{});
        }

        pub fn exec(self: *Self, inst: Inst) void {
            self.delay_slot = self.branch;
            self.branch = false;
            self.next_pc = self.next_next_pc;
            self.next_next_pc = self.next_pc +% 4;
            const dst_r, const dst_v, const delay_dst_r, const delay_dst_v = self.exec_no_write_regs(inst);
            self.set_r(dst_r, dst_v);
            self.set_r(self.delay_dst_r, self.delay_dst_v);
            self.delay_dst_r = delay_dst_r;
            self.delay_dst_v = delay_dst_v;
        }

        // execute instruction and return the reg that needs to been written by the operation
        pub fn exec_no_write_regs(self: *Self, inst: Inst) struct { RegIdx, u32, RegIdx, u32 } {
            var dst_r: RegIdx = .{0};
            var dst_v: u32 = 0;
            var delay_dst_r: RegIdx = .{0};
            var delay_dst_v: u32 = 0;
            switch (inst) {
                // ALU
                .lui => |a| {
                    const imm: u16 = @bitCast(a.imm);
                    dst_r = a.rt;
                    dst_v = @as(u32, imm) << 16;
                },
                .addu => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) +% self.r(a.rt);
                },
                .add => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    const rt: i32 = @bitCast(self.r(a.rt));
                    const res, const ov = @addWithOverflow(rs, rt);
                    if (ov == 0) {
                        dst_r = a.rd;
                        dst_v = @bitCast(res);
                    } else {
                        self.exception(Exception.ov);
                    }
                },
                .addiu => |a| {
                    const imm: u32 = @bitCast(@as(i32, a.imm));
                    dst_r = a.rt;
                    dst_v = self.r(a.rs) +% imm;
                },
                .addi => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    const res, const ov = @addWithOverflow(rs, @as(i32, a.imm));
                    if (ov == 0) {
                        dst_r = a.rt;
                        dst_v = @bitCast(res);
                    } else {
                        self.exception(Exception.ov);
                    }
                },
                .subu => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) -% self.r(a.rt);
                },
                .sub => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    const rt: i32 = @bitCast(self.r(a.rt));
                    const res, const ov = @subWithOverflow(rs, rt);
                    if (ov == 0) {
                        dst_r = a.rd;
                        dst_v = @bitCast(res);
                    } else {
                        self.exception(Exception.ov);
                    }
                },
                .slt => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    const rt: i32 = @bitCast(self.r(a.rt));
                    dst_r = a.rd;
                    dst_v = if (rs < rt) 1 else 0;
                },
                .sltu => |a| {
                    dst_r = a.rd;
                    dst_v = if (self.r(a.rs) < self.r(a.rt)) 1 else 0;
                },
                .slti => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    dst_r = a.rt;
                    dst_v = if (rs < @as(i32, a.imm)) 1 else 0;
                },
                .sltiu => |a| {
                    const imm: u32 = @bitCast(@as(i32, a.imm));
                    dst_r = a.rt;
                    dst_v = if (self.r(a.rs) < imm) 1 else 0;
                },
                .@"and" => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) & self.r(a.rt);
                },
                .andi => |a| {
                    const imm: u16 = @bitCast(a.imm);
                    dst_r = a.rt;
                    dst_v = self.r(a.rs) & @as(u32, imm);
                },
                .@"or" => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) | self.r(a.rt);
                },
                .ori => |a| {
                    const imm: u16 = @bitCast(a.imm);
                    dst_r = a.rt;
                    dst_v = self.r(a.rs) | @as(u32, imm);
                },
                .xor => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) ^ self.r(a.rt);
                },
                .xori => |a| {
                    const imm: u16 = @bitCast(a.imm);
                    dst_r = a.rt;
                    dst_v = self.r(a.rs) ^ @as(u32, imm);
                },
                .nor => |a| {
                    dst_r = a.rd;
                    dst_v = ~(self.r(a.rs) | self.r(a.rt));
                },
                .mult => |a| {
                    const rs_i32: i32 = @bitCast(self.r(a.rs));
                    const rt_i32: i32 = @bitCast(self.r(a.rt));
                    const rs: i64 = @as(i64, rs_i32);
                    const rt: i64 = @as(i64, rt_i32);
                    const res: u64 = @bitCast(rs * rt);
                    self.hi = @intCast(res >> 32);
                    self.lo = @intCast(res & 0xffff_ffff);
                },
                .multu => |a| {
                    const rs: u64 = @as(u64, self.r(a.rs));
                    const rt: u64 = @as(u64, self.r(a.rt));
                    const res = rs * rt;
                    self.hi = @intCast(res >> 32);
                    self.lo = @intCast(res & 0xffff_ffff);
                },
                .div => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    const rt: i32 = @bitCast(self.r(a.rt));
                    if (rt == 0) {
                        self.hi = @bitCast(rs);
                        if (rs >= 0) {
                            self.lo = 0xffff_ffff; // -1
                        } else {
                            self.lo = 0x0000_0001; // +1
                        }
                    } else if (rs == 0x8000_0000 and rt == 0xffff_ffff) {
                        self.hi = 0;
                        self.lo = 0x8000_0000;
                    } else {
                        self.hi = @bitCast(@rem(rs, rt));
                        self.lo = @bitCast(@divTrunc(rs, rt));
                    }
                },
                .divu => |a| {
                    const rs = self.r(a.rs);
                    const rt = self.r(a.rt);
                    if (rt == 0) {
                        self.hi = @bitCast(rs);
                        self.lo = 0xffff_ffff;
                    } else {
                        self.hi = @bitCast(rs % rt);
                        self.lo = @bitCast(rs / rt);
                    }
                },
                // Shifts
                .sll => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rt) << @intCast(a.imm);
                },
                .sllv => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rt) << @intCast(self.r(a.rs) & 0b11111);
                },
                .srl => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rt) >> @intCast(a.imm);
                },
                .srlv => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rt) >> @intCast(self.r(a.rs) & 0b11111);
                },
                .sra => |a| {
                    dst_r = a.rd;
                    const rt: i32 = @bitCast(self.r(a.rt));
                    dst_v = @bitCast(rt >> @intCast(a.imm));
                },
                .srav => |a| {
                    dst_r = a.rd;
                    const rt: i32 = @bitCast(self.r(a.rt));
                    dst_v = @bitCast(rt >> @intCast(self.r(a.rs) & 0b11111));
                },
                // Load & Store
                .lw => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    if (self.read(false, u32, self.r(a.rs) +% offset)) |v_lw| {
                        delay_dst_r = a.rt;
                        delay_dst_v = v_lw;
                    } else |_| {
                        self.exception(Exception.adel);
                    }
                },
                .lh => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    if (self.read(false, u16, self.r(a.rs) +% offset)) |v_lw_u16| {
                        const v_lw_i16: i16 = @bitCast(v_lw_u16);
                        const v_lw: u32 = @bitCast(@as(i32, v_lw_i16));
                        delay_dst_r = a.rt;
                        delay_dst_v = v_lw;
                    } else |_| {
                        self.exception(Exception.adel);
                    }
                },
                .lhu => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    if (self.read(false, u16, self.r(a.rs) +% offset)) |v_lw_u16| {
                        const v_lw = @as(u32, v_lw_u16);
                        delay_dst_r = a.rt;
                        delay_dst_v = v_lw;
                    } else |_| {
                        self.exception(Exception.adel);
                    }
                },
                .lb => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    if (self.read(false, u8, self.r(a.rs) +% offset)) |v_lw_u8| {
                        const v_lw_i8: i8 = @bitCast(v_lw_u8);
                        const v_lw: u32 = @bitCast(@as(i32, v_lw_i8));
                        delay_dst_r = a.rt;
                        delay_dst_v = v_lw;
                    } else |_| {
                        self.exception(Exception.adel);
                    }
                },
                .lbu => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    if (self.read(false, u8, self.r(a.rs) +% offset)) |v_lw_u8| {
                        const v_lw = @as(u32, v_lw_u8);
                        delay_dst_r = a.rt;
                        delay_dst_v = v_lw;
                    } else |_| {
                        self.exception(Exception.adel);
                    }
                },
                .lwl => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    // Reat the word at an aligned address
                    const word = self.read(false, u32, addr & ~@as(u32, 0b11)) catch unreachable;
                    // Get the register value that will be updated.  Read from
                    // the delay slot so that lwl and lwr work in a chain.
                    const cur_v = if (a.rt[0] == self.delay_dst_r[0]) self.delay_dst_v else self.r(a.rt);
                    const v = switch (@as(u2, @truncate(addr))) {
                        0 => (cur_v & 0x00ff_ffff) | (word << 24),
                        1 => (cur_v & 0x0000_ffff) | (word << 16),
                        2 => (cur_v & 0x0000_00ff) | (word << 8),
                        3 => (cur_v & 0x0000_0000) | (word << 0),
                    };
                    delay_dst_r = a.rt;
                    delay_dst_v = v;
                },
                .lwr => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    // Reat the word at an aligned address
                    const word = self.read(false, u32, addr & ~@as(u32, 0b11)) catch unreachable;
                    // Get the register value that will be updated.  Read from
                    // the delay slot so that lwl and lwr work in a chain.
                    const cur_v = if (a.rt[0] == self.delay_dst_r[0]) self.delay_dst_v else self.r(a.rt);
                    const v = switch (@as(u2, @truncate(addr))) {
                        0 => (cur_v & 0x0000_0000) | (word >> 0),
                        1 => (cur_v & 0xff00_0000) | (word >> 8),
                        2 => (cur_v & 0xffff_0000) | (word >> 16),
                        3 => (cur_v & 0xffff_ff00) | (word >> 24),
                    };
                    delay_dst_r = a.rt;
                    delay_dst_v = v;
                },
                .sw => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.write(u32, self.r(a.rs) +% offset, self.r(a.rt)) catch {
                        self.exception(Exception.ades);
                    };
                },
                .sh => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.write(u16, self.r(a.rs) +% offset, @intCast(self.r(a.rt) & 0xffff)) catch {
                        self.exception(Exception.ades);
                    };
                },
                .sb => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.write(u8, self.r(a.rs) +% offset, @intCast(self.r(a.rt) & 0xff)) catch {
                        self.exception(Exception.ades);
                    };
                },
                .swl => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    const addr_aligned = addr & ~@as(u32, 0b11);
                    // Reat the word at an aligned address
                    const cur_word = self.read(false, u32, addr_aligned) catch unreachable;
                    const v = self.r(a.rt);
                    const word = switch (addr & 0b11) {
                        0 => (cur_word & 0xffff_ff00) | (v >> 24),
                        1 => (cur_word & 0xffff_0000) | (v >> 16),
                        2 => (cur_word & 0xff00_0000) | (v >> 8),
                        3 => (cur_word & 0x0000_0000) | (v >> 0),
                        else => unreachable,
                    };
                    self.write(u32, addr_aligned, word) catch {
                        self.exception(Exception.ades);
                    };
                },
                .swr => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    const addr_aligned = addr & ~@as(u32, 0b11);
                    // Reat the word at an aligned address
                    const cur_word = self.read(false, u32, addr_aligned) catch unreachable;
                    const v = self.r(a.rt);
                    const word = switch (addr & 0b11) {
                        0 => (cur_word & 0x0000_0000) | (v << 0),
                        1 => (cur_word & 0x0000_00ff) | (v << 8),
                        2 => (cur_word & 0x0000_ffff) | (v << 16),
                        3 => (cur_word & 0x00ff_ffff) | (v << 24),
                        else => unreachable,
                    };
                    self.write(u32, addr_aligned, word) catch {
                        self.exception(Exception.ades);
                    };
                },
                // Jump & Branch
                .j => |a| {
                    self.next_next_pc = (self.pc & 0xf0000000) + a.imm * 4;
                    self.branch = true;
                },
                .jr => |a| {
                    self.next_next_pc = self.r(a.rs);
                    self.branch = true;
                },
                .jal => |a| {
                    self.set_r(.{31}, self.pc +% 8);
                    self.next_next_pc = (self.pc & 0xf0000000) + a.imm * 4;
                    self.branch = true;
                },
                .jalr => |a| {
                    self.set_r(a.rd, self.pc +% 8);
                    self.next_next_pc = self.r(a.rs);
                    self.branch = true;
                },
                .bne => |a| {
                    self.branch_cmp(self.r(a.rs) != self.r(a.rt), a.imm);
                },
                .beq => |a| {
                    self.branch_cmp(self.r(a.rs) == self.r(a.rt), a.imm);
                },
                .bgtz => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    self.branch_cmp(rs > 0, a.imm);
                },
                .bgez => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    self.branch_cmp(rs >= 0, a.imm);
                },
                .bgezal => |a| {
                    self.set_r(.{31}, self.pc +% 8);
                    const rs: i32 = @bitCast(self.r(a.rs));
                    self.branch_cmp(rs >= 0, a.imm);
                },
                .bltz => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    self.branch_cmp(rs < 0, a.imm);
                },
                .blez => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    self.branch_cmp(rs <= 0, a.imm);
                },
                .bltzal => |a| {
                    self.set_r(.{31}, self.pc +% 8);
                    const rs: i32 = @bitCast(self.r(a.rs));
                    self.branch_cmp(rs < 0, a.imm);
                },
                // Coprocessor
                .mtc0 => |a| {
                    if (a.imm != 0) {
                        std.debug.panic("TODO: mtc0 with sel={} at pc={x:0>8}", .{ a.imm, self.pc });
                    }
                    self.cop0.set_r(a.rd, self.r(a.rt));
                },
                .mfc0 => |a| {
                    if (a.imm != 0) {
                        std.debug.panic("TODO: mfc0 with sel={} at pc={x:0>8}", .{ a.imm, self.pc });
                    }
                    const v_c0 = self.cop0.r(a.rd);

                    delay_dst_r = a.rt;
                    delay_dst_v = v_c0;
                },
                .lwc0 => |a| {
                    _ = a; // unused
                    self.exception(Exception.cpu);
                },
                .swc0 => |a| {
                    _ = a; // unused
                    self.exception(Exception.cpu);
                },
                .lwc2 => |a| {
                    // Load Word To Coprocessor 2
                    _ = a; // unused
                    std.debug.panic("TODO: lwc2 at pc={x:0>8}", .{self.pc});
                },
                .swc2 => |a| {
                    // Store Word From Coprocessor 2
                    _ = a; // unused
                    std.debug.panic("TODO: swc2 at pc={x:0>8}", .{self.pc});
                },
                .mtc2 => |a| {
                    // Move To Coprocessor 2
                    _ = a; // unused
                    std.debug.panic("TODO: mtc2 at pc={x:0>8}", .{self.pc});
                },
                .mfc2 => |a| {
                    // Move From Coprocessor 2
                    _ = a; // unused
                    std.debug.panic("TODO: mfc2 at pc={x:0>8}", .{self.pc});
                },
                .ctc2 => |a| {
                    // Move Control Word To Coprocessor 2
                    _ = a; // unused
                    std.debug.panic("TODO: ctc2 at pc={x:0>8}", .{self.pc});
                },
                .cfc2 => |a| {
                    // Move Control Word From Coprocessor 2
                    _ = a; // unused
                    std.debug.panic("TODO: cfc2 at pc={x:0>8}", .{self.pc});
                },
                .cop1 => |a| {
                    _ = a; // unused
                    self.exception(Exception.cpu);
                },
                .cop3 => |a| {
                    _ = a; // unused
                    self.exception(Exception.cpu);
                },
                // Other
                .rfe => |a| {
                    _ = a;
                    const mask_int_enable_user: u32 = 0b0000_0000_0001_1111;
                    const mode = self.cop0.status_reg & mask_int_enable_user;
                    self.cop0.status_reg &= ~mask_int_enable_user;
                    self.cop0.status_reg |= (mode >> 2);
                },
                .mflo => |a| {
                    dst_r = a.rd;
                    dst_v = self.lo;
                },
                .mfhi => |a| {
                    dst_r = a.rd;
                    dst_v = self.hi;
                },
                .mtlo => |a| {
                    self.lo = self.r(a.rs);
                },
                .mthi => |a| {
                    self.hi = self.r(a.rs);
                },
                .sys => |a| {
                    _ = a; // unused
                    self.exception(Exception.syscall);
                },
                .brk => |a| {
                    _ = a; // unused
                    self.exception(Exception.bp);
                },
                // else => std.debug.panic("TODO: inst {?} at pc={x:0>8}", .{ inst, self.pc }),
            }
            return .{ dst_r, dst_v, delay_dst_r, delay_dst_v };
        }

        fn branch_cmp(self: *Self, cmp_result: bool, imm: i16) void {
            if (cmp_result) {
                const offset: u32 = @bitCast(@as(i32, imm * 4));
                self.next_next_pc = self.pc +% 4 +% offset;
                self.branch = true;
            }
        }

        fn exception(self: *Self, cause: Exception) void {
            // Exception handler address depends on BEV
            const handler: u32 = if (self.cop0.status_reg & (1 << COP0_SR_BEV) != 0)
                0xbfc00180
            else
                0x80000080;

            // Shift bits [5:0] of SR two places to the left.
            // Those bits are three pairs of Interrupt Enable/User
            // Mode bits behaving like a stack 3 entries deep.
            // Entering an exception pushes a pair of zeroes
            // by left shifting the stack which disables
            // interrupts and puts the CPU in kernel mode.
            // The original third entry is discarded (it’s up
            // to the kernel to handle more than two recursive
            // exception levels).
            const mask_int_enable_user: u32 = 0b0000_0000_0001_1111;
            const mode = self.cop0.status_reg & mask_int_enable_user;
            self.cop0.status_reg &= ~mask_int_enable_user;
            self.cop0.status_reg |= (mode << 2) & mask_int_enable_user;

            self.cop0.cause = @intFromEnum(cause) << 2;

            // Save current instruction in EPC
            self.cop0.exception_pc = self.pc;
            if (self.delay_slot) {
                self.cop0.exception_pc = self.pc -% 4;
                self.cop0.cause |= 1 << COP0_CAUSE_BD;
            }

            // Jump directly to the exception handler
            self.next_pc = handler;
            self.next_next_pc = self.next_pc +% 4;
        }

        fn check_alignment(comptime T: type, addr: u32) bool {
            switch (T) {
                u8 => return true,
                u16 => {
                    if (addr % 2 != 0) {
                        std.debug.print("unaligned u16 memory access at {x:0>8}\n", .{addr});
                        return false;
                    } else {
                        return true;
                    }
                },
                u32 => {
                    if (addr % 4 != 0) {
                        std.debug.print("unaligned u32 memory access at {x:0>8}\n", .{addr});
                        return false;
                    } else {
                        return true;
                    }
                },
                else => @compileError("invalid T"),
            }
        }

        fn bios_read(self: *Self, comptime T: type, addr: u32) T {
            return buf_read(T, self.bios, addr);
        }

        pub fn read(self: *Self, comptime code: bool, comptime T: type, addr: u32) !T {
            if (!code) {
                if (self.cop0.status_reg & (1 << COP0_SR_ISC) != 0) {
                    std.debug.print("TODO: read {s} Isolate Cache addr {x:0>8} (ignored)\n", .{ @typeName(T), addr });
                    return 0;
                }
            }
            if (!Self.check_alignment(T, addr)) {
                return error.unaligned;
            }
            const region, const paddr = vaddr_to_paddr(addr);
            _ = region;
            if (ADDR_RAM <= paddr and paddr < SIZE_RAM) {
                return self.ram.read(T, paddr - ADDR_RAM);
            } else if (ADDR_BIOS <= paddr and paddr < ADDR_BIOS + SIZE_BIOS) {
                return self.bios_read(T, paddr - ADDR_BIOS);
            } else if (ADDR_EXP_REG1 <= paddr and paddr < ADDR_EXP_REG1 + SIZE_EXP_REG1) {
                std.debug.print("TODO: read {s} at EXP_REG1 {x:0>8}\n", .{ @typeName(T), addr });
                return switch (T) {
                    u8 => 0xff,
                    u16 => 0xffff,
                    u32 => 0xffff_ffff,
                    else => @compileError("invalid T"),
                };
            } else if (ADDR_IO_PORTS <= paddr and paddr < ADDR_IO_PORTS + SIZE_IO_PORTS) {
                return self.io_regs_read(T, @intCast(paddr - ADDR_IO_PORTS));
            } else if (ADDR_CACHECTL <= paddr and paddr < ADDR_CACHECTL + SIZE_CACHECTL) {
                std.debug.print("TODO: read {s} at CACHECTL {x:0>8}\n", .{ @typeName(T), addr });
                return 0;
            } else {
                std.debug.panic("TODO: read paddr {x:0>8}", .{paddr});
            }
        }

        pub fn write(self: *Self, comptime T: type, addr: u32, v: T) !void {
            if (self.cop0.status_reg & (1 << COP0_SR_ISC) != 0) {
                std.debug.print("TODO: write {s} Isolate Cache addr {x:0>8} value {x:0>8} (ignored)\n", .{ @typeName(T), addr, v });
                return;
            }
            if (!Self.check_alignment(T, addr)) {
                return error.unaligned;
            }
            const region, const paddr = vaddr_to_paddr(addr);
            _ = region;
            if (ADDR_RAM <= paddr and paddr < ADDR_RAM + SIZE_RAM) {
                self.ram.write(T, paddr - ADDR_RAM, v);
            } else if (ADDR_IO_PORTS <= paddr and paddr < ADDR_IO_PORTS + SIZE_IO_PORTS) {
                self.io_regs_write(T, @intCast(paddr - ADDR_IO_PORTS), v);
            } else if (ADDR_CACHECTL <= paddr and paddr < ADDR_CACHECTL + SIZE_CACHECTL) {
                std.debug.print("TODO: write {s} at CACHECTL {x:0>8} value {x:0>8}\n", .{ @typeName(T), addr, v });
                // TODO
                // @panic("TODO: CACHECTL");
            } else {
                std.debug.panic("TODO: write addr {x:0>8}", .{addr});
            }
        }

        fn io_regs_read(self: *Self, comptime T: type, addr: u16) T {
            if (self.cfg.dbg) {
                if (self.dbg.trace_io) {
                    std.debug.print("io read {s} at {x:0>8}\n", .{ @typeName(T), addr + ADDR_IO_PORTS });
                }
            }
            switch (T) {
                u8 => std.debug.panic("TODO: io_regs_read u8 addr {x:0>8}\n", .{addr + ADDR_IO_PORTS}),
                u16 => switch (addr) {
                    ADDR_I_STAT => {
                        std.debug.print("TODO: io_regs_read u16 ADDR_I_STAT\n", .{});
                    },
                    ADDR_I_MASK => {
                        std.debug.print("TODO: io_regs_read u16 ADDR_I_MASK\n", .{});
                    },
                    ADDR_SPU_VOICE_START...ADDR_SPU_VOICE_END => {
                        std.debug.print("TODO: io_regs_read u16 ADDR_SPU_VOICE_START...ADDR_SPU_VOICE_END {x:0>8}\n", .{addr + ADDR_IO_PORTS});
                    },
                    ADDR_SPU_CTL_START...ADDR_SPU_CTL_END => {
                        std.debug.print("TODO: io_regs_read u16 ADDR_SPU_CTL_REG_START...ADDR_SPU_CTL_REG_END {x:0>8}\n", .{addr + ADDR_IO_PORTS});
                    },
                    else => std.debug.panic("TODO: io_regs_read u16 addr {x:0>8}\n", .{addr + ADDR_IO_PORTS}),
                },
                u32 => switch (addr) {
                    ADDR_I_MASK => {
                        std.debug.print("TODO: io_regs_read u32 ADDR_I_MASK\n", .{});
                    },
                    ADDR_TIMERS_START...ADDR_TIMERS_END => {
                        std.debug.print("TODO: io_regs_read u32 ADDR_TIMERS_START...ADDR_TIMERS_END {x:0>8}\n", .{addr + ADDR_IO_PORTS});
                    },
                    Dma.ADDR_START...Dma.ADDR_END => {
                        return self.dma.read_u32(addr);
                    },
                    Gpu.ADDR_START...Gpu.ADDR_END => {
                        return self.gpu.read_u32(addr);
                    },
                    else => std.debug.panic("TODO: io_regs_read u32 addr {x:0>8}\n", .{addr + ADDR_IO_PORTS}),
                },
                else => @compileError("invalid T"),
            }
            return 0;
        }

        fn io_regs_write(self: *Self, comptime T: type, addr: u16, v: T) void {
            if (self.cfg.dbg) {
                if (self.dbg.trace_io) {
                    std.debug.print("io write {s} {x:0>8} at {x:0>8}\n", .{ @typeName(T), v, addr + ADDR_IO_PORTS });
                }
            }
            switch (T) {
                u8 => switch (addr) {
                    DTL_H2000_PSX_POST => {
                        std.debug.print("TODO: io_regs_write u8 DTL_H2000_PSX_POST value {x:0>8}\n", .{v});
                    },
                    else => std.debug.panic("TODO: io_regs_write u8 addr {x:0>8} value {x:0>2}\n", .{ addr + ADDR_IO_PORTS, v }),
                },
                u16 => switch (addr) {
                    ADDR_I_MASK => {
                        std.debug.print("TODO: io_regs_write u16 ADDR_I_MASK value {x:0>8}\n", .{v});
                    },
                    ADDR_TIMERS_START...ADDR_TIMERS_END => {
                        std.debug.print("TODO: io_regs_write u16 ADDR_TIMERS_START...ADDR_TIMERS_END {x:0>8} value {x:0>8}\n", .{ addr + ADDR_IO_PORTS, v });
                    },
                    ADDR_SPU_VOICE_START...ADDR_SPU_VOICE_END => {
                        std.debug.print("TODO: io_regs_write u16 ADDR_SPU_VOICE_START...ADDR_SPU_VOICE_END {x:0>8} value {x:0>8}\n", .{ addr + ADDR_IO_PORTS, v });
                    },
                    ADDR_SPU_CTL_START...ADDR_SPU_CTL_END => {
                        std.debug.print("TODO: io_regs_write u16 ADDR_SPU_CTL_START...ADDR_SPU_CTL_END {x:0>8} value {x:0>8}\n", .{ addr + ADDR_IO_PORTS, v });
                    },
                    else => std.debug.panic("TODO: io_regs_write u16 addr {x:0>8} value {x:0>4}\n", .{ addr + ADDR_IO_PORTS, v }),
                },
                u32 => switch (addr) {
                    Dma.ADDR_START...Dma.ADDR_END => {
                        self.dma.write_u32(addr, v);
                    },
                    ADDR_TIMERS_START...ADDR_TIMERS_END => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_TIMERS_START...ADDR_TIMERS_END {x:0>8} value {x:0>8}\n", .{ addr + ADDR_IO_PORTS, v });
                    },
                    ADDR_I_STAT => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_I_STAT value {x:0>8}\n", .{v});
                    },
                    ADDR_I_MASK => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_I_MASK value {x:0>8}\n", .{v});
                    },
                    ADDR_EXP1_BASE => {
                        if (v != 0x1f000000) {
                            std.debug.panic("Bad expansion 1 base address: {x:0>8}", .{v});
                        }
                    },
                    ADDR_EXP2_BASE => {
                        if (v != 0x1f802000) {
                            std.debug.panic("Bad expansion 1 base address: {x:0>8}", .{v});
                        }
                    },
                    ADDR_EXP1_DELAY => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_EXP1_DELAY value {x:0>8}\n", .{v});
                    },
                    ADDR_EXP3_DELAY => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_EXP3_DELAY value {x:0>8}\n", .{v});
                    },
                    ADDR_BIOS_ROM => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_BIOS_ROM value {x:0>8}\n", .{v});
                    },
                    ADDR_SPU_DELAY => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_SPU_DELAY value {x:0>8}\n", .{v});
                    },
                    ADDR_CDROM_DELAY => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_CDROM_DELAY value {x:0>8}\n", .{v});
                    },
                    ADDR_EXP2_DELAY => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_EXP2_DELAY value {x:0>8}\n", .{v});
                    },
                    ADDR_COM_DELAY => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_COM_DELAY value {x:0>8}\n", .{v});
                    },
                    ADDR_RAM_SIZE => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_RAM_SIZE value {x:0>8}\n", .{v});
                    },
                    ADDR_SPU_MAIN_VOL_LR => {
                        std.debug.print("TODO: io_regs_write u32 ADDR_SPU_MAIN_VOL_LR value {x:0>8}\n", .{v});
                    },
                    Gpu.ADDR_START...Gpu.ADDR_END => {
                        self.gpu.write_u32(addr, v);
                    },
                    else => std.debug.panic("TODO: io_regs_write u32 addr {x:0>8} value {x:0>8}\n", .{ addr + ADDR_IO_PORTS, v }),
                },
                else => @compileError("invalid T"),
            }
        }
    };
}

//
// IO Regs
//

// Memory Control 1

const ADDR_EXP1_BASE: u32 = 0x0000; // 1f801000 4 Expansion 1 Base Address (usually 1F000000h)
const ADDR_EXP2_BASE: u32 = 0x0004; // 1f801004 4 Expansion 2 Base Address (usually 1F802000h)
const ADDR_EXP1_DELAY: u32 = 0x0008; // 1f801008 4 Expansion 1 Delay/Size (usually 0013243Fh; 512Kbytes 8bit-bus)
const ADDR_EXP3_DELAY: u32 = 0x000c; // 1f80100c 4 Expansion 3 Delay/Size (usually 00003022h; 1 byte)
const ADDR_BIOS_ROM: u32 = 0x0010; // 1f801010 4 BIOS ROM Delay/Size (usually 0013243Fh; 512Kbytes 8bit-bus)
const ADDR_SPU_DELAY: u32 = 0x0014; // 1f801014 4 SPU_DELAY Delay/Size (usually 200931E1h)
const ADDR_CDROM_DELAY: u32 = 0x0018; // 1f801018 4 CDROM_DELAY Delay/Size (usually 00020843h or 00020943h)
const ADDR_EXP2_DELAY: u32 = 0x001c; // 1f80101c 4 Expansion 2 Delay/Size (usually 00070777h; 128-bytes 8bit-bus)
const ADDR_COM_DELAY: u32 = 0x0020; // 1f801020 4 COM_DELAY / COMMON_DELAY (00031125h or 0000132Ch or 00001325h)

// Peripheral I/O Ports

//   1F801040h 1/4  JOY_DATA Joypad/Memory Card Data (R/W)
//   1F801044h 4    JOY_STAT Joypad/Memory Card Status (R)
//   1F801048h 2    JOY_MODE Joypad/Memory Card Mode (R/W)
//   1F80104Ah 2    JOY_CTRL Joypad/Memory Card Control (R/W)
//   1F80104Eh 2    JOY_BAUD Joypad/Memory Card Baudrate (R/W)
//   1F801050h 1/4  SIO_DATA Serial Port Data (R/W)
//   1F801054h 4    SIO_STAT Serial Port Status (R)
//   1F801058h 2    SIO_MODE Serial Port Mode (R/W)
//   1F80105Ah 2    SIO_CTRL Serial Port Control (R/W)
//   1F80105Ch 2    SIO_MISC Serial Port Internal Register (R/W)
//   1F80105Eh 2    SIO_BAUD Serial Port Baudrate (R/W)

// Memory Control 2

const ADDR_RAM_SIZE: u32 = 0x0060; // 1f801060 4/2 RAM_SIZE (usually 00000B88h; 2MB RAM mirrored in first 8MB)

// Interrupt Control

const ADDR_I_STAT: u32 = 0x0070; // 1f801070 2 I_STAT - Interrupt status register
const ADDR_I_MASK: u32 = 0x0074; // 1f801074 2 I_MASK - Interrupt mask register

// Timers (aka Root counters)

const ADDR_TIMERS_START: u32 = 0x0100;
const ADDR_TIMERS_END: u32 = 0x0129;
const ADDR_TIM0_CUR_CNT: u32 = 0x0100; //   1f801100 2 Timer 0 Current Counter Value (R/W)  ;\
const ADDR_TIM0_CNT_MODE: u32 = 0x0104; //  1f801104 2 Timer 0 Counter Mode          (R/W)  ; Dotclock
const ADDR_TIM0_CNT_TAR: u32 = 0x0108; //   1f801108 2 Timer 0 Counter Target Value  (R/W)  ;/
const ADDR_TIM1_CUR_CNT: u32 = 0x0110; //   1f801110 2 Timer 1 Current Counter Value (R/W)  ;\
const ADDR_TIM1_CNT_MODE: u32 = 0x0114; //  1f801114 2 Timer 1 Counter Mode          (R/W)  ; Horizontal Retrace
const ADDR_TIM1_CNT_TAR: u32 = 0x0118; //   1f801118 2 Timer 1 Counter Target Value  (R/W)  ;/
const ADDR_TIM2_CUR_CNT: u32 = 0x0120; //   1f801120 2 Timer 2 Current Counter Value (R/W)  ;\
const ADDR_TIM2_CNT_MODE: u32 = 0x0124; //  1f801124 2 Timer 2 Counter Mode          (R/W)  ; 1/8 system clock
const ADDR_TIM2_CNT_TAR: u32 = 0x0128; //   1f801128 2 Timer 2 Counter Target Value  (R/W)  ;/

// CDROM Registers (Address.Read/Write.Index)

//   1F801800h.x.x   1   CD Index/Status Register (Bit0-1 R/W, Bit2-7 Read Only)
//   1F801801h.R.x   1   CD Response Fifo (R) (usually with Index1)
//   1F801802h.R.x   1/2 CD Data Fifo - 8bit/16bit (R) (usually with Index0..1)
//   1F801803h.R.0   1   CD Interrupt Enable Register (R)
//   1F801803h.R.1   1   CD Interrupt Flag Register (R/W)
//   1F801803h.R.2   1   CD Interrupt Enable Register (R) (Mirror)
//   1F801803h.R.3   1   CD Interrupt Flag Register (R/W) (Mirror)
//   1F801801h.W.0   1   CD Command Register (W)
//   1F801802h.W.0   1   CD Parameter Fifo (W)
//   1F801803h.W.0   1   CD Request Register (W)
//   1F801801h.W.1   1   Unknown/unused
//   1F801802h.W.1   1   CD Interrupt Enable Register (W)
//   1F801803h.W.1   1   CD Interrupt Flag Register (R/W)
//   1F801801h.W.2   1   Unknown/unused
//   1F801802h.W.2   1   CD Audio Volume for Left-CD-Out to Left-SPU-Input (W)
//   1F801803h.W.2   1   CD Audio Volume for Left-CD-Out to Right-SPU-Input (W)
//   1F801801h.W.3   1   CD Audio Volume for Right-CD-Out to Right-SPU-Input (W)
//   1F801802h.W.3   1   CD Audio Volume for Right-CD-Out to Left-SPU-Input (W)
//   1F801803h.W.3   1   CD Audio Volume Apply Changes (by writing bit5=1)

// MDEC Registers

//   1F801820h.Write 4   MDEC Command/Parameter Register (W)
//   1F801820h.Read  4   MDEC Data/Response Register (R)
//   1F801824h.Write 4   MDEC Control/Reset Register (W)
//   1F801824h.Read  4   MDEC Status Register (R)

// SPU Voice 0..23 Registers

const ADDR_SPU_VOICE_START: u32 = 0x0c00;
const ADDR_SPU_VOICE_END: u32 = 0x0d7f;
//   0F801C00h+N*10h 4   Voice 0..23 Volume Left/Right
//   1F801C04h+N*10h 2   Voice 0..23 ADPCM Sample Rate
//   1F801C06h+N*10h 2   Voice 0..23 ADPCM Start Address
//   1F801C08h+N*10h 4   Voice 0..23 ADSR Attack/Decay/Sustain/Release
//   1F801C0Ch+N*10h 2   Voice 0..23 ADSR Current Volume
//   1F801C0Eh+N*10h 2   Voice 0..23 ADPCM Repeat Address

// SPU Control Registers

const ADDR_SPU_CTL_START: u32 = 0x0d80;
const ADDR_SPU_CTL_END: u32 = 0x0dbf;

const ADDR_SPU_MAIN_VOL_LR: u32 = 0x0d80; // 1f801d80 4 Main Volume Left/Right
const ADDR_SPU_REV_VOL_LR: u32 = 0x0d84; // 1f801d84 4 Reverb Output Volume Left/Right
//   1F801D88h 4  Voice 0..23 Key ON (Start Attack/Decay/Sustain) (W)
const ADDR_SPU_VOICE_KEY_OFF: u32 = 0x0d8c; // 1F801D8Ch 4 Voice 0..23 Key OFF (Start Release) (W)
const ADDR_SPU_VOICE_CHAN_FM: u32 = 0x0d90; // 1F801D90h 4 Voice 0..23 Channel FM (pitch lfo) mode (R/W)
const ADDR_SPU_VOICE_NOISE_MODE: u32 = 0x0d94; // 1F801D94h 4 Voice 0..23 Channel Noise mode (R/W)
const ADDR_SPU_VOICE_REV_MODE: u32 = 0x0d98; // 1F801D98h 4 Voice 0..23 Channel Reverb mode (R/W)
const ADDR_SPU_VOICE_CHAN_STATUS: u32 = 0x0d9c; // 1F801D9Ch 4 Voice 0..23 Channel ON/OFF (status) (R)
//   1F801DA0h 2  Unknown? (R) or (W)
//   1F801DA2h 2  Sound RAM Reverb Work Area Start Address
//   1F801DA4h 2  Sound RAM IRQ Address
const ADDR_SPU_SND_RAM_DTA: u32 = 0x0da6; //   1F801DA6h 2  Sound RAM Data Transfer Address
const ADDR_SPU_SND_RAM_DTF: u32 = 0x0da8; //   1F801DA8h 2  Sound RAM Data Transfer Fifo
const ADDR_SPU_CNT: u32 = 0x0daa; // 1F801DAA 2 SPU Control Register (SPUCNT)
const ADDR_SPU_SND_RAM_DTC: u32 = 0x0dac; //   1F801DACh 2  Sound RAM Data Transfer Control
const ADDR_SPU_STAT: u32 = 0x0dae; // 1F801DAE 2 SPU Status Register (SPUSTAT) (R)
const ADDR_SPU_CD_VOL_LR: u32 = 0x0db0; //   1F801DB0h 4  CD Volume Left/Right
const ADDR_SPU_EXT_VOL_LR: u32 = 0x0db4; //   1F801DB4h 4  Extern Volume Left/Right
//   1F801DB8h 4  Current Main Volume Left/Right
//   1F801DBCh 4  Unknown? (R/W)

// SPU Reverb Configuration Area

//   1F801DC0h 2  dAPF1  Reverb APF Offset 1
//   1F801DC2h 2  dAPF2  Reverb APF Offset 2
//   1F801DC4h 2  vIIR   Reverb Reflection Volume 1
//   1F801DC6h 2  vCOMB1 Reverb Comb Volume 1
//   1F801DC8h 2  vCOMB2 Reverb Comb Volume 2
//   1F801DCAh 2  vCOMB3 Reverb Comb Volume 3
//   1F801DCCh 2  vCOMB4 Reverb Comb Volume 4
//   1F801DCEh 2  vWALL  Reverb Reflection Volume 2
//   1F801DD0h 2  vAPF1  Reverb APF Volume 1
//   1F801DD2h 2  vAPF2  Reverb APF Volume 2
//   1F801DD4h 4  mSAME  Reverb Same Side Reflection Address 1 Left/Right
//   1F801DD8h 4  mCOMB1 Reverb Comb Address 1 Left/Right
//   1F801DDCh 4  mCOMB2 Reverb Comb Address 2 Left/Right
//   1F801DE0h 4  dSAME  Reverb Same Side Reflection Address 2 Left/Right
//   1F801DE4h 4  mDIFF  Reverb Different Side Reflection Address 1 Left/Right
//   1F801DE8h 4  mCOMB3 Reverb Comb Address 3 Left/Right
//   1F801DECh 4  mCOMB4 Reverb Comb Address 4 Left/Right
//   1F801DF0h 4  dDIFF  Reverb Different Side Reflection Address 2 Left/Right
//   1F801DF4h 4  mAPF1  Reverb APF Address 1 Left/Right
//   1F801DF8h 4  mAPF2  Reverb APF Address 2 Left/Right
//   1F801DFCh 4  vIN    Reverb Input Volume Left/Right

// SPU Internal Registers

//   1F801E00h+N*04h  4 Voice 0..23 Current Volume Left/Right
//   1F801E60h      20h Unknown? (R/W)
//   1F801E80h     180h Unknown? (Read: FFh-filled) (Unused or Write only?)

// Expansion Region 2 (default 128 bytes, max 8 KBytes)

//   1F802000h      80h Expansion Region (8bit data bus, crashes on 16bit access?)

// Expansion Region 2 - Dual Serial Port (for TTY Debug Terminal)

//   1F802020h/1st    DUART Mode Register 1.A (R/W)
//   1F802020h/2nd    DUART Mode Register 2.A (R/W)
//   1F802021h/Read   DUART Status Register A (R)
//   1F802021h/Write  DUART Clock Select Register A (W)
//   1F802022h/Read   DUART Toggle Baud Rate Generator Test Mode (Read=Strobe)
//   1F802022h/Write  DUART Command Register A (W)
//   1F802023h/Read   DUART Rx Holding Register A (FIFO) (R)
//   1F802023h/Write  DUART Tx Holding Register A (W)
//   1F802024h/Read   DUART Input Port Change Register (R)
//   1F802024h/Write  DUART Aux. Control Register (W)
//   1F802025h/Read   DUART Interrupt Status Register (R)
//   1F802025h/Write  DUART Interrupt Mask Register (W)
//   1F802026h/Read   DUART Counter/Timer Current Value, Upper/Bit15-8 (R)
//   1F802026h/Write  DUART Counter/Timer Reload Value,  Upper/Bit15-8 (W)
//   1F802027h/Read   DUART Counter/Timer Current Value, Lower/Bit7-0 (R)
//   1F802027h/Write  DUART Counter/Timer Reload Value,  Lower/Bit7-0 (W)
//   1F802028h/1st    DUART Mode Register 1.B (R/W)
//   1F802028h/2nd    DUART Mode Register 2.B (R/W)
//   1F802029h/Read   DUART Status Register B (R)
//   1F802029h/Write  DUART Clock Select Register B (W)
//   1F80202Ah/Read   DUART Toggle 1X/16X Test Mode (Read=Strobe)
//   1F80202Ah/Write  DUART Command Register B (W)
//   1F80202Bh/Read   DUART Rx Holding Register B (FIFO) (R)
//   1F80202Bh/Write  DUART Tx Holding Register B (W)
//   1F80202Ch/None   DUART Reserved Register (neither R nor W)
//   1F80202Dh/Read   DUART Input Port (R)
//   1F80202Dh/Write  DUART Output Port Configuration Register (W)
//   1F80202Eh/Read   DUART Start Counter Command (Read=Strobe)
//   1F80202Eh/Write  DUART Set Output Port Bits Command (Set means Out=LOW)
//   1F80202Fh/Read   DUART Stop Counter Command (Read=Strobe)
//   1F80202Fh/Write  DUART Reset Output Port Bits Command (Reset means Out=HIGH)

// Expansion Region 2 - Int/Dip/Post

//   1F802000h 1 DTL-H2000: ATCONS STAT (R)
//   1F802002h 1 DTL-H2000: ATCONS DATA (R and W)
//   1F802004h 2 DTL-H2000: Whatever 16bit data ?
//   1F802030h 1/4 DTL-H2000: Secondary IRQ10 Flags
//   1F802032h 1 DTL-H2000: Whatever IRQ Control ?
const ADDR_DTL_H2000_BOOTMODE: u32 = 0x1040; // 1f802040 1 DTL-H2000: Bootmode "Dip switches" (R)
const DTL_H2000_PSX_POST: u32 = 0x1041; // 1f802041 1 PSX: POST (external 7 segment display, indicate BIOS boot status)
//   1F802042h 1 DTL-H2000: POST/LED (similar to POST) (other addr, 2-digit wide)   1F802070h 1 PS2: POST2 (similar to POST, but PS2 BIOS uses this address)

// Expansion Region 2 - Nocash Emulation Expansion

//   1F802060h Emu-Expansion ID1 "E" (R)
//   1F802061h Emu-Expansion ID2 "X" (R)
//   1F802062h Emu-Expansion ID3 "P" (R)
//   1F802063h Emu-Expansion Version (01h) (R)
//   1F802064h Emu-Expansion Enable1 "O" (R/W)
//   1F802065h Emu-Expansion Enable2 "N" (R/W)
//   1F802066h Emu-Expansion Halt (R)
//   1F802067h Emu-Expansion Turbo Mode Flags (R/W)

// Expansion Region 3 (default 1 byte, max 2 MBytes)

//   1FA00000h - Not used by BIOS or any PSX games
//   1FA00000h - POST3 (similar to POST, but PS2 BIOS uses this address)

// BIOS Region (default 512 Kbytes, max 4 MBytes)

//   1FC00000h 80000h   BIOS ROM (512Kbytes) (Reset Entrypoint at BFC00000h)

// Memory Control 3 (Cache Control)

//   FFFE0130h 4        Cache Control
