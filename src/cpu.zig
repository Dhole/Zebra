const std = @import("std");
const log = std.log;
const mem = std.mem;
const ArrayList = std.ArrayList;

const root = @import("root.zig");
const Inst = root.Inst;
const InstArgs = root.InstArgs;
const Op = root.Op;

const decoder = @import("decoder.zig");
const decode = decoder.decode;

const disasm = @import("disasm.zig");
const FmtReg = disasm.FmtReg;
const FmtInst = disasm.FmtInst;
const print_disasm = disasm.print_disasm;

const MEM_SIZE: usize = 4 * 512 * 1024; // 2 MiB
pub const BIOS_SIZE: usize = 512 * 1024; // 512 KiB
const SCRATCH_SIZE: usize = 1024; // 1 KiB
const HWREGS_SIZE: usize = 8 * 1024; // 8 KiB
const CACHECTL_SIZE: usize = 512; // 0.5 KiB
const ADDR_BIOS: u32 = 0xbfc0_0000;
const ADDR_KUSEG: u32 = 0x0000_0000;
const ADDR_KSEG0: u32 = 0x8000_0000;
const ADDR_KSEG1: u32 = 0xa000_0000;
const ADDR_KSEG2: u32 = 0xfffe_0000;
const ADDR_RESET: u32 = ADDR_BIOS;
const ADDR_SCRATCH: u32 = 0x1f80_0000;
const ADDR_HWREGS: u32 = 0x1f80_1000;

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

pub fn Cpu(comptime dbg_writer_type: type, comptime cfg: Cfg) type {
    return struct {
        dbg_w: dbg_writer_type,
        comptime cfg: Cfg = cfg,
        dbg: Dbg,
        r: [32]u32, // Registers
        pc: u32, // Program Counter
        hi: u32, // Multiplication 64 bit high result or division remainder
        lo: u32, // Multiplication 64 bit low result or division quotient
        bios: []const u8,
        mem: []u8,
        allocator: mem.Allocator,

        const Self = @This();

        pub fn init(
            allocator: mem.Allocator,
            bios: []const u8,
            dbg_writer: dbg_writer_type,
        ) !Self {
            if (bios.len != BIOS_SIZE) {
                return error.BiosInvalidSize;
            }
            const memory = try allocator.alloc(u8, MEM_SIZE);
            @memset(memory, 0);
            const bios_copy = try allocator.alloc(u8, BIOS_SIZE);
            std.mem.copyForwards(u8, bios_copy, bios);
            const self = Self{
                .dbg_w = dbg_writer,
                .cfg = cfg,
                .dbg = Dbg.init(allocator),
                .r = [_]u32{0} ** 32,
                .pc = ADDR_RESET,
                .lo = 0,
                .hi = 0,
                .bios = bios_copy,
                .mem = memory,
                .allocator = allocator,
            };
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.mem);
            self.allocator.free(self.bios);
            self.dbg.deinit();
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
                        const v: u8 = @intCast((column - 1) * 8 + row);
                        try writer.print("   {d:0>2} {}: {x:0>8}", .{ v, FmtReg{ .v = v }, self.r[v] });
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
            const inst_raw = self.read(u32, self.pc);
            const inst = decode(inst_raw) orelse @panic("TODO: unknown inst");
            if (self.cfg.dbg) {
                if (self.dbg.trace_inst) {
                    print_disasm(self.dbg_w, self.pc, inst_raw, inst) catch @panic("write");
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
            self.exec(inst);
        }

        fn exec(self: *Self, inst: Inst) void {
            switch (inst) {
                .lui => |a| {
                    const imm: u16 = @bitCast(a.imm);
                    self.r[a.rt] = @as(u32, imm) << 16;
                    self.pc += 4;
                },
                .ori => |a| {
                    const imm: u16 = @bitCast(a.imm);
                    self.r[a.rt] = self.r[a.rs] | @as(u32, imm);
                    self.pc += 4;
                },
                .sw => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.write(u32, self.r[a.rs] +% offset, self.r[a.rt]);
                    self.pc += 4;
                },
                .sll => |a| {
                    self.r[a.rd] = self.r[a.rt] << @intCast(a.imm);
                    self.pc += 4;
                },
                .addiu => |a| {
                    const imm: u32 = @bitCast(@as(i32, a.imm));
                    self.r[a.rt] = self.r[a.rs] +% imm;
                    self.pc += 4;
                },
                .j => |a| {
                    const new_pc = (self.pc & 0xf0000000) + a.imm * 4;
                    self.pc += 4;
                    // Execute instruction in the branch delay slot
                    self.step();
                    self.pc = new_pc;
                },
                .jal => |a| {
                    const new_pc = (self.pc & 0xf0000000) + a.imm * 4;
                    self.r[31] = self.pc + 8;
                    self.pc += 4;
                    // Execute instruction in the branch delay slot
                    self.step();
                    self.pc = new_pc;
                },
                .@"or" => |a| {
                    self.r[a.rd] = self.r[a.rs] | self.r[a.rt];
                    self.pc += 4;
                },
                .cfc0 => |a| {
                    // TODO
                    // self.r[a.rt] = self.cop0.ctl_reg[a.rd]
                    _ = a;
                    self.pc += 4;
                },
                .bne => |a| {
                    self.pc += 4;
                    if (self.r[a.rs] != self.r[a.rt]) {
                        const pc = self.pc;
                        // Execute instruction in the branch delay slot
                        self.step();
                        const offset: u32 = @bitCast(@as(i32, a.imm * 4));
                        self.pc = pc +% offset;
                    }
                },
                .addi => |a| {
                    // TODO: overflow trap
                    const imm: u32 = @bitCast(@as(i32, a.imm));
                    self.r[a.rt] = self.r[a.rs] +% imm;
                    self.pc += 4;
                },
                .lw => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.r[a.rt] = self.read(u32, self.r[a.rs] +% offset);
                    self.pc += 4;
                },
                .sltu => |a| {
                    if (self.r[a.rs] < self.r[a.rt]) {
                        self.r[a.rd] = 1;
                    } else {
                        self.r[a.rd] = 0;
                    }
                    self.pc += 4;
                },
                .addu => |a| {
                    self.r[a.rd] = self.r[a.rs] +% self.r[a.rt];
                    self.pc += 4;
                },
                .sh => |a| {
                    const offset: u32 = @bitCast(@as(i32, a.imm));
                    self.write(u16, self.r[a.rs] +% offset, @intCast(self.r[a.rt] & 0xffff));
                    self.pc += 4;
                },
                .andi => |a| {
                    _ = a;
                    @panic("TODO");
                },
                else => @panic("TODO"),
            }
        }

        fn mem_read(self: *Self, comptime T: type, addr: u32) T {
            return buf_read(T, self.mem, addr);
        }

        fn mem_write(self: *Self, comptime T: type, addr: u32, v: T) void {
            buf_write(T, self.mem, addr, v);
        }

        // fn mem_write_u32(self: *Self, addr: u32, v: u32) void {
        //     self.mem[addr + 0] = @intCast((v & 0x000000ff) >> 0);
        //     self.mem[addr + 1] = @intCast((v & 0x0000ff00) >> 8);
        //     self.mem[addr + 2] = @intCast((v & 0x00ff0000) >> 16);
        //     self.mem[addr + 3] = @intCast((v & 0xff000000) >> 24);
        // }

        fn bios_read(self: *Self, comptime T: type, addr: u32) T {
            return buf_read(T, self.bios, addr);
        }

        // fn bios_read_u16(self: *Self, addr: u32) u16 {
        //     return @as(u32, self.bios[addr]) + @as(u32, self.bios[addr + 1]) * 0x100;
        // }

        // fn bios_read_u32(self: *Self, addr: u32) u32 {
        //     return @as(u32, self.bios[addr]) + @as(u32, self.bios[addr + 1]) * 0x100 +
        //         @as(u32, self.bios[addr + 2]) * 0x10000 + @as(u32, self.bios[addr + 3]) * 0x1000000;
        // }

        pub fn read(self: *Self, comptime T: type, addr: u32) T {
            if (ADDR_KUSEG <= addr and addr < ADDR_KUSEG + MEM_SIZE) {
                return self.mem_read(T, addr - ADDR_KUSEG);
            } else if (ADDR_KSEG0 <= addr and addr < ADDR_KSEG0 + MEM_SIZE) {
                return self.mem_read(T, addr - ADDR_KSEG0);
            } else if (ADDR_KSEG1 <= addr and addr < ADDR_KSEG1 + MEM_SIZE) {
                return self.mem_read(T, addr - ADDR_KSEG1);
            } else if (ADDR_BIOS <= addr and addr < ADDR_BIOS + BIOS_SIZE) {
                return self.bios_read(T, addr - ADDR_BIOS);
            } else if (ADDR_HWREGS <= addr and addr < ADDR_HWREGS + HWREGS_SIZE) {
                @panic("TODO: HWREGS");
            } else {
                @panic("TODO");
            }
        }

        fn write(self: *Self, comptime T: type, addr: u32, v: T) void {
            if (ADDR_KUSEG <= addr and addr < ADDR_KUSEG + MEM_SIZE) {
                self.mem_write(T, addr - ADDR_KUSEG, v);
            } else if (ADDR_KSEG0 <= addr and addr < ADDR_KSEG0 + MEM_SIZE) {
                self.mem_write(T, addr - ADDR_KSEG0, v);
            } else if (ADDR_KSEG1 <= addr and addr < ADDR_KSEG1 + MEM_SIZE) {
                self.mem_write(T, addr - ADDR_KSEG1, v);
            } else if (ADDR_HWREGS <= addr and addr < ADDR_HWREGS + HWREGS_SIZE) {
                // TODO
                // @panic("TODO: HWREGS");
            } else if (ADDR_KSEG2 <= addr and addr < ADDR_KSEG2 + CACHECTL_SIZE) {
                // TODO
                // @panic("TODO: CACHECTL");
            } else {
                @panic("TODO");
            }
        }

        // fn write_u32(self: *Self, addr: u32, v: u32) void {
        //     if (ADDR_KUSEG <= addr and addr < ADDR_KUSEG + MEM_SIZE) {
        //         self.mem_write_u32(addr - ADDR_KUSEG, v);
        //     } else if (ADDR_KSEG0 <= addr and addr < ADDR_KSEG0 + MEM_SIZE) {
        //         self.mem_write_u32(addr - ADDR_KSEG0, v);
        //     } else if (ADDR_KSEG1 <= addr and addr < ADDR_KSEG1 + MEM_SIZE) {
        //         self.mem_write_u32(addr - ADDR_KSEG1, v);
        //     } else if (ADDR_HWREGS <= addr and addr < ADDR_HWREGS + HWREGS_SIZE) {
        //         // TODO
        //         // @panic("TODO: HWREGS");
        //     } else if (ADDR_KSEG2 <= addr and addr < ADDR_KSEG2 + CACHECTL_SIZE) {
        //         // TODO
        //         // @panic("TODO: CACHECTL");
        //     } else {
        //         @panic("TODO");
        //     }
        // }

        // pub fn read_u32(self: *Self, addr: u32) u32 {
        //     if (ADDR_KUSEG <= addr and addr < ADDR_KUSEG + MEM_SIZE) {
        //         return self.mem_read_u32(addr - ADDR_KUSEG);
        //     } else if (ADDR_KSEG0 <= addr and addr < ADDR_KSEG0 + MEM_SIZE) {
        //         return self.mem_read_u32(addr - ADDR_KSEG0);
        //     } else if (ADDR_KSEG1 <= addr and addr < ADDR_KSEG1 + MEM_SIZE) {
        //         return self.mem_read_u32(addr - ADDR_KSEG1);
        //     } else if (ADDR_BIOS <= addr and addr < ADDR_BIOS + BIOS_SIZE) {
        //         return self.bios_read_u32(addr - ADDR_BIOS);
        //     } else {
        //         @panic("TODO");
        //     }
        // }
    };
}
