const std = @import("std");

// GPU Registers

pub const Gpu = struct {
    pub const ADDR_START: u16 = 0x0810;
    pub const ADDR_END: u16 = 0x0817;

    const RegRead = enum(u16) {
        read = 0x0810, // 1F801810.Read  4 GPUREAD Read responses to GP0(C0h) and GP1(10h) commands
        stat = 0x0814, // 1F801814.Read  4 GPUSTAT Read GPU Status Register
        _,
    };
    const RegWrite = enum(u16) {
        gp0 = 0x0810, //  1F801810.Write 4 GP0 Send GP0 Commands/Packets (Rendering and VRAM Access)
        gp1 = 0x0814, //  1F801814.Write 4 GP1 Send GP1 Commands (Display Control)
        _,
    };

    const Self = @This();

    pub fn init() Self {
        return Self{};
    }

    pub fn read_u32(self: *Self, addr: u16) u32 {
        _ = self;
        const reg: RegRead = @enumFromInt(addr);
        switch (reg) {
            RegRead.read => {
                std.debug.print("TODO: gpu.read_u32 READ\n", .{});
                return 0;
            },
            RegRead.stat => {
                std.debug.print("TODO: gpu.read_u32 STAT\n", .{});
                // Set bit 28 to signal that the GPU is ready to receive DMA blocks
                return 0x1000_0000;
            },
            _ => unreachable,
        }
    }
    pub fn write_u32(self: *Self, addr: u16, v: u32) void {
        _ = self;
        const reg: RegWrite = @enumFromInt(addr);
        switch (reg) {
            RegWrite.gp0 => {
                std.debug.print("TODO: gpu.write_u32 GP0 value {x:0>8}\n", .{v});
            },
            RegWrite.gp1 => {
                std.debug.print("TODO: gpu.write_u32 GP1 value {x:0>8}\n", .{v});
            },
            _ => unreachable,
        }
    }
};
