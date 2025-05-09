const std = @import("std");
const log = std.log;
const mem = std.mem;
const ArrayList = std.ArrayList;

const root = @import("root.zig");
const Inst = root.Inst;
const RegIdx = root.RegIdx;
const InstArgs = root.InstArgs;
const Op = root.Op;

const decoder = @import("decoder.zig");
const decode = decoder.decode;

const disasm = @import("disasm.zig");
const fmt_reg = disasm.fmt_reg;
const fmt_inst = disasm.fmt_inst;
const print_disasm = disasm.print_disasm;

pub const SIZE_RAM: usize = 4 * 512 * 1024; // 2 MiB
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

fn buf_read(comptime T: type, buf: []const u8, addr: u32) T {
    return switch (T) {
        u8 => buf[addr],
        u16 => @as(u16, buf[addr]) + @as(u16, buf[addr + 1]) * 0x100,
        u32 => @as(u32, buf[addr]) + @as(u32, buf[addr + 1]) * 0x100 +
            @as(u32, buf[addr + 2]) * 0x10000 + @as(u32, buf[addr + 3]) * 0x1000000,
        else => @compileError("invalid T"),
    };
}

fn buf_write(comptime T: type, buf: []u8, addr: u32, v: T) void {
    switch (T) {
        u8 => {
            buf[addr] = @intCast(v);
        },
        u16 => {
            buf[addr + 0] = @intCast((v & 0x00ff) >> 0);
            buf[addr + 1] = @intCast((v & 0xff00) >> 8);
        },
        u32 => {
            buf[addr + 0] = @intCast((v & 0x000000ff) >> 0);
            buf[addr + 1] = @intCast((v & 0x0000ff00) >> 8);
            buf[addr + 2] = @intCast((v & 0x00ff0000) >> 16);
            buf[addr + 3] = @intCast((v & 0xff000000) >> 24);
        },
        else => @compileError("invalid T"),
    }
}

pub const Cfg = struct {
    dbg: bool = false,
};

const Dbg = struct {
    trace_inst: bool = false,
    trace_io: bool = false,
    breaks: ArrayList(u32),
    _break: bool = false,
    _first_step: bool = true,

    fn init(allocator: mem.Allocator) Dbg {
        return Dbg{
            .breaks = ArrayList(u32).init(allocator),
        };
    }

    fn deinit(self: *Dbg) void {
        self.breaks.deinit();
    }
};

const COP0_SR_ISC = 16; // Isolate Cache (0=No, 1=Isolate)

const Cop0 = struct {
    // COP0 Register Summary
    const REG_BPC = 3; // Breakpoint on execute (R/W)
    const REG_BDA = 5; // Breakpoint on data access (R/W)
    const REG_JUMPDEST = 6; // Randomly memorized jump address (R)
    const REG_DCIC = 7; // Breakpoint control (R/W)
    const REG_BadVaddr = 8; // Bad Virtual Address (R)
    const REG_BDAM = 9; // Data Access breakpoint mask (R/W)
    const REG_BPCM = 11; // Execute breakpoint mask (R/W)
    const REG_SR = 12; // System status register (R/W)
    const REG_CAUSE = 13; // Describes the most recently recognised exception (R)
    const REG_EPC = 14; // Return Address from Trap (R)
    const REG_PRID = 15; // Processor ID (R)

    status_reg: u32, // SR

    const Self = @This();

    fn init() Self {
        return Self{
            .status_reg = 0,
        };
    }

    fn set_r(self: *Self, idx: RegIdx, v: u32) void {
        switch (idx[0]) {
            REG_BPC, REG_BDA, REG_JUMPDEST, REG_DCIC, REG_BDAM, REG_BPCM, REG_CAUSE => {
                if (v != 0) {
                    std.debug.panic("TODO: cop0 write reg {} {x:0>8}", .{ idx[0], v });
                }
            },
            REG_SR => self.status_reg = v,
            else => std.debug.panic("TODO: cop0 set_r {} {x:0>8}", .{ idx[0], v }),
        }
    }

    fn r(self: *Self, idx: RegIdx) u32 {
        return switch (idx[0]) {
            REG_SR => self.status_reg,
            else => std.debug.panic("TODO: cop0 r {}", .{idx[0]}),
        };
    }
};

pub fn Cpu(comptime dbg_writer_type: type, comptime cfg: Cfg) type {
    return struct {
        dbg_w: dbg_writer_type,
        comptime cfg: Cfg = cfg,
        dbg: Dbg,
        regs: [32]u32, // Registers
        cop0: Cop0,
        pc: u32, // Program Counter
        hi: u32, // Multiplication 64 bit high result or division remainder
        lo: u32, // Multiplication 64 bit low result or division quotient
        delay_dst_r: RegIdx = .{0}, // register to update from a load delay
        delay_dst_v: u32 = 0, // value from a load delay
        bios: []const u8,
        ram: []u8,
        allocator: mem.Allocator,

        const Self = @This();

        pub fn init(
            allocator: mem.Allocator,
            bios: []const u8,
            dbg_writer: dbg_writer_type,
        ) !Self {
            if (bios.len != SIZE_BIOS) {
                return error.BiosInvalidSize;
            }
            const ram = try allocator.alloc(u8, SIZE_RAM);
            @memset(ram, 0);
            const bios_copy = try allocator.alloc(u8, SIZE_BIOS);
            std.mem.copyForwards(u8, bios_copy, bios);
            const self = Self{
                .dbg_w = dbg_writer,
                .cfg = cfg,
                .dbg = Dbg.init(allocator),
                .regs = [_]u32{0} ** 32,
                .cop0 = Cop0.init(),
                .pc = VADDR_RESET,
                .lo = 0,
                .hi = 0,
                .bios = bios_copy,
                .ram = ram,
                .allocator = allocator,
            };
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.ram);
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
                if (self.dbg._break) {
                    self.dbg_w.print("Breakpoint at {x:0>8}\n", .{self.pc}) catch @panic("write");
                    self.dbg._break = false;
                    return;
                }
                self.dbg._first_step = false;
            }
        }

        pub fn step(self: *Self) void {
            // Fetch next instruction
            const inst_raw = self.read(true, u32, self.pc);
            const inst = decode(inst_raw) orelse std.debug.panic("TODO: unknown inst {x:0>8}", .{inst_raw});
            if (self.cfg.dbg) {
                if (self.dbg.trace_inst) {
                    self.dbg_trace(inst, inst_raw) catch @panic("write");
                }
                if (!self.dbg._first_step) {
                    for (self.dbg.breaks.items) |addr| {
                        if (addr == self.pc) {
                            self.dbg._break = true;
                            return;
                        }
                    }
                }
            }
            const dst_r, const dst_v, const delay_dst_r, const delay_dst_v = self.exec_no_write_regs(inst);
            self.set_r(dst_r, dst_v);
            self.set_r(self.delay_dst_r, self.delay_dst_v);
            self.delay_dst_r = delay_dst_r;
            self.delay_dst_v = delay_dst_v;
        }

        fn dbg_trace(self: *Self, inst: Inst, inst_raw: u32) !void {
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
                //
                .j, .mfc0, .mfc2 => {},
                .lw => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    const v = self.read(false, u32, addr);
                    try self.dbg_w.print(
                        "{}=0x{x:0>8}, addr=0x{x:0>8}[0x{x:0>8}]",
                        .{ fmt_reg(a.rt), self.r(a.rt), addr, v },
                    );
                },
                .lh => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    const v = self.read(false, u16, addr);
                    try self.dbg_w.print(
                        "{}=0x{x:0>8}, addr=0x{x:0>8}[0x{x:0>4}]",
                        .{ fmt_reg(a.rt), self.r(a.rt), addr, v },
                    );
                },
                .lb, .lbu => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const addr = self.r(a.rs) +% offset;
                    const v = self.read(false, u8, addr);
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
                else => std.debug.panic("TODO dbg_trace {?}", .{inst}),
            }
            try self.dbg_w.print("\n", .{});
        }

        pub fn exec(self: *Self, inst: Inst) void {
            const dst_r, const dst_v = self.exec_no_write_regs(inst);
            self.set_r(dst_r, dst_v);
        }

        // fn exec_load_delay_slot(self: *Self, rt: RegIdx, v: u32) struct { RegIdx, u32 } {
        //     // TODO: decode next instruction and keep it in Self instead of fetching it again here.
        //     const next_inst_raw = self.read(true, u32, self.pc);
        //     const MASK_LOAD: u32 = 0b111000_00000_000000000000000000000;
        //     const VALU_LOAD: u32 = 0b100000_00000_000000000000000000000;
        //     const MASK_LDCP: u32 = 0b111100_11101_000000000000000000000;
        //     const VALU_LDCP: u32 = 0b010000_00000_000000000000000000000;
        //     // If the next instruction is a load, don't execute it via the load
        //     // delay slot and let the next step handle it so that we don't
        //     // delay register updates for more than 1 step.
        //     if (((next_inst_raw & MASK_LOAD) == VALU_LOAD) or
        //         ((next_inst_raw & MASK_LDCP) == VALU_LDCP))
        //     {
        //         // TODO: panic if the next instruction reads register rt
        //         return .{ rt, v };
        //     } else {
        //         // Execute instruction in the load delay slot
        //         const dst_r, const dst_v = self._step();
        //         self.set_r(rt, v);
        //         return .{ dst_r, dst_v };
        //     }
        // }

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
                    self.pc +%= 4;
                },
                .addu => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) +% self.r(a.rt);
                    self.pc +%= 4;
                },
                .add => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    const rt: i32 = @bitCast(self.r(a.rt));
                    const res, const ov = @addWithOverflow(rs, rt);
                    if (ov == 1) {
                        std.debug.panic("TODO: add overflow trap", .{});
                    }
                    dst_r = a.rd;
                    dst_v = @bitCast(res);
                    self.pc +%= 4;
                },
                .addiu => |a| {
                    const imm: u32 = @bitCast(@as(i32, a.imm));
                    dst_r = a.rt;
                    dst_v = self.r(a.rs) +% imm;
                    self.pc +%= 4;
                },
                .addi => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    const res, const ov = @addWithOverflow(rs, @as(i32, a.imm));
                    if (ov == 1) {
                        std.debug.panic("TODO: addi overflow trap", .{});
                    }
                    dst_r = a.rt;
                    dst_v = @bitCast(res);
                    self.pc +%= 4;
                },
                .subu => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) -% self.r(a.rt);
                    self.pc +%= 4;
                },
                .slt => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    const rt: i32 = @bitCast(self.r(a.rt));
                    dst_r = a.rd;
                    dst_v = if (rs < rt) 1 else 0;
                    self.pc +%= 4;
                },
                .sltu => |a| {
                    dst_r = a.rd;
                    dst_v = if (self.r(a.rs) < self.r(a.rt)) 1 else 0;
                    self.pc +%= 4;
                },
                .slti => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    dst_r = a.rt;
                    dst_v = if (rs < @as(i32, a.imm)) 1 else 0;
                    self.pc +%= 4;
                },
                .sltiu => |a| {
                    const imm: u32 = @bitCast(@as(i32, a.imm));
                    dst_r = a.rt;
                    dst_v = if (self.r(a.rs) < imm) 1 else 0;
                    self.pc +%= 4;
                },
                .@"and" => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) & self.r(a.rt);
                    self.pc +%= 4;
                },
                .andi => |a| {
                    const imm: u16 = @bitCast(a.imm);
                    dst_r = a.rt;
                    dst_v = self.r(a.rs) & @as(u32, imm);
                    self.pc +%= 4;
                },
                .@"or" => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rs) | self.r(a.rt);
                    self.pc +%= 4;
                },
                .ori => |a| {
                    const imm: u16 = @bitCast(a.imm);
                    dst_r = a.rt;
                    dst_v = self.r(a.rs) | @as(u32, imm);
                    self.pc +%= 4;
                },
                // Shifts
                .sll => |a| {
                    dst_r = a.rd;
                    dst_v = self.r(a.rt) << @intCast(a.imm);
                    self.pc +%= 4;
                },
                .sra => |a| {
                    dst_r = a.rd;
                    const rt: i32 = @bitCast(self.r(a.rt));
                    dst_v = @bitCast(rt >> @intCast(a.imm));
                    self.pc +%= 4;
                },
                // Load & Store
                .lw => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const v_lw = self.read(false, u32, self.r(a.rs) +% offset);
                    self.pc +%= 4;
                    delay_dst_r = a.rt;
                    delay_dst_v = v_lw;
                },
                .lb => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const v_lw_i8: i8 = @bitCast(self.read(false, u8, self.r(a.rs) +% offset));
                    const v_lw: u32 = @bitCast(@as(i32, v_lw_i8));
                    self.pc +%= 4;
                    delay_dst_r = a.rt;
                    delay_dst_v = v_lw;
                },
                .lbu => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    const v_lw = @as(u32, self.read(false, u8, self.r(a.rs) +% offset));
                    self.pc +%= 4;
                    delay_dst_r = a.rt;
                    delay_dst_v = v_lw;
                },
                .sw => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.write(u32, self.r(a.rs) +% offset, self.r(a.rt));
                    self.pc +%= 4;
                },
                .sh => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.write(u16, self.r(a.rs) +% offset, @intCast(self.r(a.rt) & 0xffff));
                    self.pc +%= 4;
                },
                .sb => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.write(u8, self.r(a.rs) +% offset, @intCast(self.r(a.rt) & 0xff));
                    self.pc +%= 4;
                },
                // Jump & Branch
                .j => |a| {
                    const new_pc = (self.pc & 0xf0000000) + a.imm * 4;
                    self.pc +%= 4;
                    // Execute instruction in the branch delay slot
                    _ = self.step();
                    self.pc = new_pc;
                },
                .jr => |a| {
                    const new_pc = self.r(a.rs);
                    self.pc +%= 4;
                    // Execute instruction in the branch delay slot
                    self.step();
                    self.pc = new_pc;
                },
                .jal => |a| {
                    const new_pc = (self.pc & 0xf0000000) + a.imm * 4;
                    self.set_r(.{31}, self.pc + 8);
                    self.pc +%= 4;
                    // Execute instruction in the branch delay slot
                    _ = self.step();
                    self.pc = new_pc;
                },
                .jalr => |a| {
                    const new_pc = self.r(a.rs);
                    self.set_r(a.rd, self.pc + 8);
                    self.pc +%= 4;
                    // Execute instruction in the branch delay slot
                    _ = self.step();
                    self.pc = new_pc;
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
                .bltz => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    self.branch_cmp(rs < 0, a.imm);
                },
                .blez => |a| {
                    const rs: i32 = @bitCast(self.r(a.rs));
                    self.branch_cmp(rs <= 0, a.imm);
                },
                // Other
                .rfe => |a| {
                    _ = a;
                    std.debug.print("TODO: pc={x:0>8} rfe\n", .{self.pc});
                    self.pc +%= 4;
                },
                .mtc0 => |a| {
                    if (a.imm != 0) {
                        std.debug.panic("TODO: mtc0 with sel={} at pc={x:0>8}", .{ a.imm, self.pc });
                    }
                    self.cop0.set_r(a.rd, self.r(a.rt));
                    self.pc +%= 4;
                },
                .mfc0 => |a| {
                    if (a.imm != 0) {
                        std.debug.panic("TODO: mfc0 with sel={} at pc={x:0>8}", .{ a.imm, self.pc });
                    }
                    const v_c0 = self.cop0.r(a.rd);
                    self.pc +%= 4;

                    delay_dst_r = a.rt;
                    delay_dst_v = v_c0;
                },
                .bc0f => |a| {
                    _ = a;
                    self.pc +%= 4;
                    // TODO
                },
                else => std.debug.panic("TODO: inst {?}", .{inst}),
            }
            return .{ dst_r, dst_v, delay_dst_r, delay_dst_v };
        }

        fn branch_cmp(self: *Self, cmp_result: bool, imm: i16) void {
            self.pc +%= 4;
            if (cmp_result) {
                const pc = self.pc;
                // Execute instruction in the branch delay slot.  Before that
                // apply the delay register update if any.
                self.set_r(self.delay_dst_r, self.delay_dst_v);
                self.delay_dst_r = .{0};
                self.delay_dst_v = 0;
                self.step();
                const offset: u32 = @bitCast(@as(i32, imm * 4));
                self.pc = pc +% offset;
            }
        }

        fn check_alignment(comptime T: type, addr: u32) void {
            return switch (T) {
                u8 => return,
                u16 => {
                    if (addr % 2 != 0) {
                        std.debug.panic("unaligned u16 memory access at {x:0>8}", .{addr});
                    }
                },
                u32 => {
                    if (addr % 4 != 0) {
                        std.debug.panic("unaligned u32 memory access at {x:0>8}", .{addr});
                    }
                },
                else => @compileError("invalid T"),
            };
        }

        fn ram_read(self: *Self, comptime T: type, addr: u32) T {
            return buf_read(T, self.ram, addr);
        }

        fn ram_write(self: *Self, comptime T: type, addr: u32, v: T) void {
            buf_write(T, self.ram, addr, v);
        }

        fn bios_read(self: *Self, comptime T: type, addr: u32) T {
            return buf_read(T, self.bios, addr);
        }

        pub fn read(self: *Self, comptime code: bool, comptime T: type, addr: u32) T {
            if (!code) {
                if (self.cop0.status_reg & (1 << COP0_SR_ISC) != 0) {
                    std.debug.print("TODO: read {s} Isolate Cache addr {x:0>8} (ignored)\n", .{ @typeName(T), addr });
                    return 0;
                }
            }
            Self.check_alignment(T, addr);
            const region, const paddr = vaddr_to_paddr(addr);
            _ = region;
            if (ADDR_RAM <= paddr and paddr < SIZE_RAM) {
                return self.ram_read(T, paddr - ADDR_RAM);
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
                return self.io_regs_read(T, paddr - ADDR_IO_PORTS);
            } else if (ADDR_CACHECTL <= paddr and paddr < ADDR_CACHECTL + SIZE_CACHECTL) {
                std.debug.print("TODO: read {s} at CACHECTL {x:0>8}\n", .{ @typeName(T), addr });
                return 0;
            } else {
                std.debug.panic("TODO: read paddr {x:0>8}", .{paddr});
            }
        }

        pub fn write(self: *Self, comptime T: type, addr: u32, v: T) void {
            if (self.cop0.status_reg & (1 << COP0_SR_ISC) != 0) {
                std.debug.print("TODO: write {s} Isolate Cache addr {x:0>8} value {x:0>8} (ignored)\n", .{ @typeName(T), addr, v });
                return;
            }
            Self.check_alignment(T, addr);
            const region, const paddr = vaddr_to_paddr(addr);
            _ = region;
            if (ADDR_RAM <= paddr and paddr < ADDR_RAM + SIZE_RAM) {
                self.ram_write(T, paddr - ADDR_RAM, v);
            } else if (ADDR_IO_PORTS <= paddr and paddr < ADDR_IO_PORTS + SIZE_IO_PORTS) {
                self.io_regs_write(T, paddr - ADDR_IO_PORTS, v);
            } else if (ADDR_CACHECTL <= paddr and paddr < ADDR_CACHECTL + SIZE_CACHECTL) {
                std.debug.print("TODO: write {s} at CACHECTL {x:0>8} value {x:0>8}\n", .{ @typeName(T), addr, v });
                // TODO
                // @panic("TODO: CACHECTL");
            } else {
                std.debug.panic("TODO: write addr {x:0>8}", .{addr});
            }
        }

        fn io_regs_read(self: *Self, comptime T: type, addr: u32) T {
            if (self.cfg.dbg) {
                if (self.dbg.trace_io) {
                    std.debug.print("io read {s} at {x:0>8}\n", .{ @typeName(T), addr + ADDR_IO_PORTS });
                }
            }
            switch (T) {
                u8 => std.debug.panic("TODO: io_regs_read u8 addr {x:0>8}\n", .{addr}),
                u16 => std.debug.panic("TODO: io_regs_read u16 addr {x:0>8}\n", .{addr}),
                u32 => switch (addr) {
                    I_STAT_ADDR => {
                        std.debug.panic("TODO: io_regs_read u32 I_STAT_ADDR\n", .{});
                    },
                    I_MASK_ADDR => {
                        std.debug.print("TODO: io_regs_read u32 I_MASK_ADDR\n", .{});
                        return 0;
                    },
                    else => std.debug.panic("TODO: io_regs_read u32 addr {x:0>8}\n", .{addr}),
                },
                else => @compileError("invalid T"),
            }
        }

        fn io_regs_write(self: *Self, comptime T: type, addr: u32, v: T) void {
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
                    else => std.debug.panic("TODO: io_regs_write u8 addr {x:0>8} value {x:0>2}\n", .{ addr, v }),
                },
                u16 => switch (addr) {
                    MAIN_VOL_LR_ADDR => {
                        std.debug.print("TODO: io_regs_write u16 MAIN_VOL_LR_ADDR value {x:0>8}\n", .{v});
                    },
                    MAIN_VOL_LR_ADDR + 2 => {
                        std.debug.print("TODO: io_regs_write u16 MAIN_VOL_LR_ADDR+2 value {x:0>8}\n", .{v});
                    },
                    REV_VOL_LR_ADDR => {
                        std.debug.print("TODO: io_regs_write u16 REV_VOL_LR_ADDR value {x:0>8}\n", .{v});
                    },
                    REV_VOL_LR_ADDR + 2 => {
                        std.debug.print("TODO: io_regs_write u16 REV_VOL_LR_ADDR+2 value {x:0>8}\n", .{v});
                    },
                    TIM0_CUR_CNT_VAL => {
                        std.debug.print("TODO: io_regs_write u16 TIM0_CUR_CNT_VAL value {x:0>8}\n", .{v});
                    },
                    TIM0_CUR_CNT_VAL + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM0_CUR_CNT_VAL+2 value {x:0>8}\n", .{v});
                    },
                    TIM0_CNT_MODE_ADDR => {
                        std.debug.print("TODO: io_regs_write u16 TIM0_CNT_MODE_ADDR value {x:0>8}\n", .{v});
                    },
                    TIM0_CNT_MODE_ADDR + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM0_CNT_MODE_ADDR+2 value {x:0>8}\n", .{v});
                    },
                    TIM0_CNT_TAR_VAL => {
                        std.debug.print("TODO: io_regs_write u16 TIM0_CNT_TAR_VAL value {x:0>8}\n", .{v});
                    },
                    TIM0_CNT_TAR_VAL + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM0_CNT_TAR_VAL+2 value {x:0>8}\n", .{v});
                    },
                    TIM1_CUR_CNT_VAL => {
                        std.debug.print("TODO: io_regs_write u16 TIM1_CUR_CNT_VAL value {x:0>8}\n", .{v});
                    },
                    TIM1_CUR_CNT_VAL + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM1_CUR_CNT_VAL+2 value {x:0>8}\n", .{v});
                    },
                    TIM1_CNT_MODE_ADDR => {
                        std.debug.print("TODO: io_regs_write u16 TIM1_CNT_MODE_ADDR value {x:0>8}\n", .{v});
                    },
                    TIM1_CNT_MODE_ADDR + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM1_CNT_MODE_ADDR+2 value {x:0>8}\n", .{v});
                    },
                    TIM1_CNT_TAR_VAL => {
                        std.debug.print("TODO: io_regs_write u16 TIM1_CNT_TAR_VAL value {x:0>8}\n", .{v});
                    },
                    TIM1_CNT_TAR_VAL + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM1_CNT_TAR_VAL+2 value {x:0>8}\n", .{v});
                    },
                    TIM2_CUR_CNT_VAL => {
                        std.debug.print("TODO: io_regs_write u16 TIM2_CUR_CNT_VAL value {x:0>8}\n", .{v});
                    },
                    TIM2_CUR_CNT_VAL + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM2_CUR_CNT_VAL+2 value {x:0>8}\n", .{v});
                    },
                    TIM2_CNT_MODE_ADDR => {
                        std.debug.print("TODO: io_regs_write u16 TIM2_CNT_MODE_ADDR value {x:0>8}\n", .{v});
                    },
                    TIM2_CNT_MODE_ADDR + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM2_CNT_MODE_ADDR+2 value {x:0>8}\n", .{v});
                    },
                    TIM2_CNT_TAR_VAL => {
                        std.debug.print("TODO: io_regs_write u16 TIM2_CNT_TAR_VAL value {x:0>8}\n", .{v});
                    },
                    TIM2_CNT_TAR_VAL + 2 => {
                        std.debug.print("TODO: io_regs_write u16 TIM2_CNT_TAR_VAL+2 value {x:0>8}\n", .{v});
                    },
                    else => std.debug.panic("TODO: io_regs_write u16 addr {x:0>8} value {x:0>4}\n", .{ addr, v }),
                },
                u32 => switch (addr) {
                    I_STAT_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 I_STAT_ADDR value {x:0>8}\n", .{v});
                    },
                    I_MASK_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 I_MASK_ADDR value {x:0>8}\n", .{v});
                    },
                    EXP1_BASE_ADDR => {
                        if (v != 0x1f000000) {
                            std.debug.panic("Bad expansion 1 base address: {x:0>8}", .{v});
                        }
                    },
                    EXP2_BASE_ADDR => {
                        if (v != 0x1f802000) {
                            std.debug.panic("Bad expansion 1 base address: {x:0>8}", .{v});
                        }
                    },
                    EXP1_DELAY_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 EXP1_DELAY_ADDR value {x:0>8}\n", .{v});
                    },
                    EXP3_DELAY_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 EXP3_DELAY_ADDR value {x:0>8}\n", .{v});
                    },
                    BIOS_ROM_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 BIOS_ROM_ADDR value {x:0>8}\n", .{v});
                    },
                    SPU_DELAY_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 SPU_DELAY_ADDR value {x:0>8}\n", .{v});
                    },
                    CDROM_DELAY_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 CDROM_DELAY_ADDR value {x:0>8}\n", .{v});
                    },
                    EXP2_DELAY_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 EXP2_DELAY_ADDR value {x:0>8}\n", .{v});
                    },
                    COM_DELAY_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 COM_DELAY_ADDR value {x:0>8}\n", .{v});
                    },
                    RAM_SIZE_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 RAM_SIZE_ADDR value {x:0>8}\n", .{v});
                    },
                    MAIN_VOL_LR_ADDR => {
                        std.debug.print("TODO: io_regs_write u32 MAIN_VOL_LR_ADDR value {x:0>8}\n", .{v});
                    },
                    else => std.debug.panic("TODO: io_regs_write u32 addr {x:0>8} value {x:0>8}\n", .{ addr, v }),
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

const EXP1_BASE_ADDR: u32 = 0x0000; // 1f801000 4 Expansion 1 Base Address (usually 1F000000h)
const EXP2_BASE_ADDR: u32 = 0x0004; // 1f801004 4 Expansion 2 Base Address (usually 1F802000h)
const EXP1_DELAY_ADDR: u32 = 0x0008; // 1f801008 4 Expansion 1 Delay/Size (usually 0013243Fh; 512Kbytes 8bit-bus)
const EXP3_DELAY_ADDR: u32 = 0x000c; // 1f80100c 4 Expansion 3 Delay/Size (usually 00003022h; 1 byte)
const BIOS_ROM_ADDR: u32 = 0x0010; // 1f801010 4 BIOS ROM Delay/Size (usually 0013243Fh; 512Kbytes 8bit-bus)
const SPU_DELAY_ADDR: u32 = 0x0014; // 1f801014 4 SPU_DELAY Delay/Size (usually 200931E1h)
const CDROM_DELAY_ADDR: u32 = 0x0018; // 1f801018 4 CDROM_DELAY Delay/Size (usually 00020843h or 00020943h)
const EXP2_DELAY_ADDR: u32 = 0x001c; // 1f80101c 4 Expansion 2 Delay/Size (usually 00070777h; 128-bytes 8bit-bus)
const COM_DELAY_ADDR: u32 = 0x0020; // 1f801020 4 COM_DELAY / COMMON_DELAY (00031125h or 0000132Ch or 00001325h)

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

const RAM_SIZE_ADDR: u32 = 0x0060; // 1f801060 4/2 RAM_SIZE (usually 00000B88h; 2MB RAM mirrored in first 8MB)

// Interrupt Control

const I_STAT_ADDR: u32 = 0x0070; // 1f801070 2 I_STAT - Interrupt status register
const I_MASK_ADDR: u32 = 0x0074; // 1f801074 2 I_MASK - Interrupt mask register

// DMA Registers

//   1F80108xh      DMA0 channel 0 - MDECin
//   1F80109xh      DMA1 channel 1 - MDECout
//   1F8010Axh      DMA2 channel 2 - GPU (lists + image data)
//   1F8010Bxh      DMA3 channel 3 - CDROM
//   1F8010Cxh      DMA4 channel 4 - SPU
//   1F8010Dxh      DMA5 channel 5 - PIO (Expansion Port)
//   1F8010Exh      DMA6 channel 6 - OTC (reverse clear OT) (GPU related)
//   1F8010F0h      DPCR - DMA Control register
//   1F8010F4h      DICR - DMA Interrupt register
//   1F8010F8h      unknown
//   1F8010FCh      unknown

// Timers (aka Root counters)

const TIM0_CUR_CNT_VAL: u32 = 0x0100; //   1f801100 2 Timer 0 Current Counter Value (R/W)  ;\
const TIM0_CNT_MODE_ADDR: u32 = 0x0104; // 1f801104 2 Timer 0 Counter Mode          (R/W)  ; Dotclock
const TIM0_CNT_TAR_VAL: u32 = 0x0108; //   1f801108 2 Timer 0 Counter Target Value  (R/W)  ;/
const TIM1_CUR_CNT_VAL: u32 = 0x0110; //   1f801110 2 Timer 1 Current Counter Value (R/W)  ;\
const TIM1_CNT_MODE_ADDR: u32 = 0x0114; // 1f801114 2 Timer 1 Counter Mode          (R/W)  ; Horizontal Retrace
const TIM1_CNT_TAR_VAL: u32 = 0x0118; //   1f801118 2 Timer 1 Counter Target Value  (R/W)  ;/
const TIM2_CUR_CNT_VAL: u32 = 0x0120; //   1f801120 2 Timer 2 Current Counter Value (R/W)  ;\
const TIM2_CNT_MODE_ADDR: u32 = 0x0124; // 1f801124 2 Timer 2 Counter Mode          (R/W)  ; 1/8 system clock
const TIM2_CNT_TAR_VAL: u32 = 0x0128; //   1f801128 2 Timer 2 Counter Target Value  (R/W)  ;/

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

// GPU Registers

//   1F801810h.Write 4   GP0 Send GP0 Commands/Packets (Rendering and VRAM Access)
//   1F801814h.Write 4   GP1 Send GP1 Commands (Display Control)
//   1F801810h.Read  4   GPUREAD Read responses to GP0(C0h) and GP1(10h) commands
//   1F801814h.Read  4   GPUSTAT Read GPU Status Register

// MDEC Registers

//   1F801820h.Write 4   MDEC Command/Parameter Register (W)
//   1F801820h.Read  4   MDEC Data/Response Register (R)
//   1F801824h.Write 4   MDEC Control/Reset Register (W)
//   1F801824h.Read  4   MDEC Status Register (R)

// SPU Voice 0..23 Registers

//   1F801C00h+N*10h 4   Voice 0..23 Volume Left/Right
//   1F801C04h+N*10h 2   Voice 0..23 ADPCM Sample Rate
//   1F801C06h+N*10h 2   Voice 0..23 ADPCM Start Address
//   1F801C08h+N*10h 4   Voice 0..23 ADSR Attack/Decay/Sustain/Release
//   1F801C0Ch+N*10h 2   Voice 0..23 ADSR Current Volume
//   1F801C0Eh+N*10h 2   Voice 0..23 ADPCM Repeat Address

// SPU Control Registers

const MAIN_VOL_LR_ADDR: u32 = 0x0d80; // 1f801d80 4 Main Volume Left/Right
const REV_VOL_LR_ADDR: u32 = 0x0d84; // 1f801d84 4 Reverb Output Volume Left/Right
//   1F801D88h 4  Voice 0..23 Key ON (Start Attack/Decay/Sustain) (W)
//   1F801D8Ch 4  Voice 0..23 Key OFF (Start Release) (W)
//   1F801D90h 4  Voice 0..23 Channel FM (pitch lfo) mode (R/W)
//   1F801D94h 4  Voice 0..23 Channel Noise mode (R/W)
//   1F801D98h 4  Voice 0..23 Channel Reverb mode (R/W)
//   1F801D9Ch 4  Voice 0..23 Channel ON/OFF (status) (R)
//   1F801DA0h 2  Unknown? (R) or (W)
//   1F801DA2h 2  Sound RAM Reverb Work Area Start Address
//   1F801DA4h 2  Sound RAM IRQ Address
//   1F801DA6h 2  Sound RAM Data Transfer Address
//   1F801DA8h 2  Sound RAM Data Transfer Fifo
//   1F801DAAh 2  SPU Control Register (SPUCNT)
//   1F801DACh 2  Sound RAM Data Transfer Control
//   1F801DAEh 2  SPU Status Register (SPUSTAT) (R)
//   1F801DB0h 4  CD Volume Left/Right
//   1F801DB4h 4  Extern Volume Left/Right
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
const DTL_H2000_BOOTMODE_ADDR: u32 = 0x1040; // 1f802040 1 DTL-H2000: Bootmode "Dip switches" (R)
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
