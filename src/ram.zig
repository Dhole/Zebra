const std = @import("std");
const Allocator = std.mem.Allocator;

const root = @import("root.zig");
const buf_read = root.buf_read;
const buf_write = root.buf_write;

pub const SIZE_RAM: usize = 4 * 512 * 1024; // 2 MiB

pub const Ram = struct {
    allocator: Allocator,
    mem: []u8,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
    ) !Self {
        const mem = try allocator.alloc(u8, SIZE_RAM);
        @memset(mem, 0);
        return Self{
            .allocator = allocator,
            .mem = mem,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.mem);
    }

    pub fn read(self: *Self, comptime T: type, addr: u32) T {
        return buf_read(T, self.mem, addr);
    }

    pub fn write(self: *Self, comptime T: type, addr: u32, v: T) void {
        buf_write(T, self.mem, addr, v);
    }
};
