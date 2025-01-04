const std = @import("std");
const log = std.log;
const mem = std.mem;

const root = @import("root.zig");
const Inst = root.Inst;
const InstArgs = root.InstArgs;
const Op = root.Op;

const decoder = @import("decoder.zig");
const decode = decoder.decode;

const disasm = @import("disasm.zig");
const FmtReg = disasm.FmtReg;

const MEM_SIZE: usize = 4 * 512 * 1024; // 2 MiB
pub const BIOS_SIZE: usize = 512 * 1024; // 512 KiB
const SCRATCH_SIZE: usize = 1024; // 1 KiB
const HWREGS_SIZE: usize = 8 * 1024; // 8 KiB
const ADDR_BIOS: u32 = 0xbfc0_0000;
const ADDR_KUSEG: u32 = 0x0000_0000;
const ADDR_KSEG0: u32 = 0x8000_0000;
const ADDR_KSEG1: u32 = 0xa000_0000;
const ADDR_RESET: u32 = ADDR_BIOS;
const ADDR_SCRATCH: u32 = 0x1f80_0000;
const ADDR_HWREGS: u32 = 0x1f80_1000;

pub const Cpu = struct {
    r: [32]u32, // Registers
    pc: u32, // Program Counter
    hi: u32, // Multiplication 64 bit high result or division remainder
    lo: u32, // Multiplication 64 bit low result or division quotient
    bios: []const u8,
    mem: []u8,
    allocator: mem.Allocator,

    const Self = @This();

    pub fn init(allocator: mem.Allocator, bios: []const u8) !Self {
        if (bios.len != BIOS_SIZE) {
            return error.BiosInvalidSize;
        }
        const _mem = try allocator.alloc(u8, MEM_SIZE);
        @memset(_mem, 0);
        const _bios = try allocator.alloc(u8, BIOS_SIZE);
        std.mem.copyForwards(u8, _bios, bios);
        const self = Self{
            .r = [_]u32{0} ** 32,
            .pc = ADDR_RESET,
            .lo = 0,
            .hi = 0,
            .bios = _bios,
            .mem = _mem,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.mem);
        self.allocator.free(self.bios);
    }

    pub fn format(self: *const Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
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

    pub fn step(self: *Self) void {
        // Fetch next instruction
        const inst_raw = self.read_u32(self.pc);
        const inst = decode(inst_raw);
        self.exec(inst);
    }

    fn exec(self: *Self, inst: Inst) void {
        switch (inst) {
            .lui => |a| {
                self.r[a.rt] = @as(u32, @intCast(a.imm)) << 16;
                self.pc += 4;
            },
            .ori => |a| {
                self.r[a.rt] = self.r[a.rs] | @as(u32, @intCast(a.imm));
                self.pc += 4;
            },
            .sw => |a| {
                const offset: u32 = @intCast(@as(i32, a.imm));
                self.write_u32(self.r[a.rs] + offset, self.r[a.rt]);
                self.pc += 4;
            },
            .sll => |a| {
                self.r[a.rd] = self.r[a.rt] << @intCast(a.imm5);
                self.pc += 4;
            },
            .addiu => |a| {
                const imm: u32 = @intCast(@as(i32, a.imm));
                self.r[a.rt] = self.r[a.rs] + imm;
                self.pc += 4;
            },
            else => @panic("TODO"),
        }
    }

    fn mem_read_u16(self: *Self, addr: u32) u16 {
        return @as(u32, self.mem[addr]) + @as(u32, self.mem[addr + 1]) * 0x100;
    }

    fn mem_read_u32(self: *Self, addr: u32) u32 {
        return @as(u32, self.mem[addr]) + @as(u32, self.mem[addr + 1]) * 0x100 +
            @as(u32, self.mem[addr + 2]) * 0x10000 + @as(u32, self.mem[addr + 3]) * 0x1000000;
    }

    fn mem_write_u32(self: *Self, addr: u32, v: u32) void {
        self.mem[addr + 0] = @intCast((v & 0x000000ff) >> 0);
        self.mem[addr + 1] = @intCast((v & 0x0000ff00) >> 8);
        self.mem[addr + 2] = @intCast((v & 0x00ff0000) >> 16);
        self.mem[addr + 3] = @intCast((v & 0xff000000) >> 24);
    }

    fn bios_read_u16(self: *Self, addr: u32) u16 {
        return @as(u32, self.bios[addr]) + @as(u32, self.bios[addr + 1]) * 0x100;
    }

    fn bios_read_u32(self: *Self, addr: u32) u32 {
        return @as(u32, self.bios[addr]) + @as(u32, self.bios[addr + 1]) * 0x100 +
            @as(u32, self.bios[addr + 2]) * 0x10000 + @as(u32, self.bios[addr + 3]) * 0x1000000;
    }

    fn read_u16(self: *Self, addr: u32) u16 {
        if (ADDR_KUSEG <= addr and addr < ADDR_KUSEG + MEM_SIZE) {
            return self.mem_read_u16(addr - ADDR_KUSEG);
        } else if (ADDR_KSEG0 <= addr and addr < ADDR_KSEG0 + MEM_SIZE) {
            return self.mem_read_u16(addr - ADDR_KSEG0);
        } else if (ADDR_KSEG1 <= addr and addr < ADDR_KSEG1 + MEM_SIZE) {
            return self.mem_read_u16(addr - ADDR_KSEG1);
        } else if (ADDR_BIOS <= addr and addr < ADDR_BIOS + BIOS_SIZE) {
            return self.bios_read_u16(addr - ADDR_BIOS);
        } else if (ADDR_HWREGS <= addr and addr < ADDR_HWREGS + HWREGS_SIZE) {
            @panic("TODO: HWREGS");
        } else {
            @panic("TODO");
        }
    }

    fn write_u32(self: *Self, addr: u32, v: u32) void {
        if (ADDR_KUSEG <= addr and addr < ADDR_KUSEG + MEM_SIZE) {
            self.mem_write_u32(addr - ADDR_KUSEG, v);
        } else if (ADDR_KSEG0 <= addr and addr < ADDR_KSEG0 + MEM_SIZE) {
            self.mem_write_u32(addr - ADDR_KSEG0, v);
        } else if (ADDR_KSEG1 <= addr and addr < ADDR_KSEG1 + MEM_SIZE) {
            self.mem_write_u32(addr - ADDR_KSEG0, v);
        } else if (ADDR_HWREGS <= addr and addr < ADDR_HWREGS + HWREGS_SIZE) {
            // TODO
            // @panic("TODO: HWREGS");
        } else {
            @panic("TODO");
        }
    }

    pub fn read_u32(self: *Self, addr: u32) u32 {
        if (ADDR_KUSEG <= addr and addr < ADDR_KUSEG + MEM_SIZE) {
            return self.mem_read_u32(addr - ADDR_KUSEG);
        } else if (ADDR_KSEG0 <= addr and addr < ADDR_KSEG0 + MEM_SIZE) {
            return self.mem_read_u32(addr - ADDR_KSEG0);
        } else if (ADDR_KSEG1 <= addr and addr < ADDR_KSEG1 + MEM_SIZE) {
            return self.mem_read_u32(addr - ADDR_KSEG1);
        } else if (ADDR_BIOS <= addr and addr < ADDR_BIOS + BIOS_SIZE) {
            return self.bios_read_u32(addr - ADDR_BIOS);
        } else {
            @panic("TODO");
        }
    }
};
