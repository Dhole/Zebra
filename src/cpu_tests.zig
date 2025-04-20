const std = @import("std");
const testing = std.testing;
const expectEqual = std.testing.expectEqual;

const _cpu = @import("cpu.zig");
const Cpu = _cpu.Cpu;
const Cfg = _cpu.Cfg;
const BIOS_SIZE = _cpu.BIOS_SIZE;
const ADDR_KUSEG = _cpu.ADDR_KUSEG;

const w = std.io.getStdOut().writer();
const cfg = Cfg{ .dbg = true };

fn cpu_init() !Cpu(@TypeOf(w), cfg) {
    const allocator = std.testing.allocator;

    const bios_file = try std.fs.cwd().openFile("bios/SCPH1001.BIN", .{});
    const bios = try bios_file.reader().readAllAlloc(allocator, BIOS_SIZE);
    defer allocator.free(bios);

    const cpu = try Cpu(@TypeOf(w), cfg).init(allocator, bios, w);
    return cpu;
}

test "op_lui" {
    // TODO
}

test "op_ori" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(2, 0x0000_ffff);
    cpu.exec(.{ .ori = .{ .rt = 1, .rs = 2, .imm = @bitCast(@as(u16, 0x0000)) } });
    try expectEqual(0x0000_ffff, cpu.r(1));

    cpu.set_r(2, 0x0000_0000);
    cpu.exec(.{ .ori = .{ .rt = 1, .rs = 2, .imm = @bitCast(@as(u16, 0xffff)) } });
    try expectEqual(0x0000_ffff, cpu.r(1));

    cpu.set_r(2, 0xffff_0000);
    cpu.exec(.{ .ori = .{ .rt = 1, .rs = 2, .imm = @bitCast(@as(u16, 0xffff)) } });
    try expectEqual(0xffff_ffff, cpu.r(1));
}

test "op_sw" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(2, ADDR_KUSEG);
    cpu.set_r(1, 0x1234_5678);
    cpu.exec(.{ .sw = .{ .rt = 1, .rs = 2, .imm = 0x0004 } });
    try expectEqual(0x1234_5678, cpu.read(u32, ADDR_KUSEG + 4));

    cpu.set_r(2, ADDR_KUSEG + 16);
    cpu.set_r(1, 0x8765_4321);
    cpu.exec(.{ .sw = .{ .rt = 1, .rs = 2, .imm = -0x0004 } });
    try expectEqual(0x8765_4321, cpu.read(u32, ADDR_KUSEG + 16 - 4));
}

test "op_sll" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(2, 0x0000_ffff);
    cpu.exec(.{ .sll = .{ .rd = 1, .rt = 2, .imm = 4, .rs = 0 } });
    try expectEqual(0x000f_fff0, cpu.r(1));

    cpu.set_r(2, 0x1111_ffff);
    cpu.exec(.{ .sll = .{ .rd = 1, .rt = 2, .imm = 4, .rs = 0 } });
    try expectEqual(0x111f_fff0, cpu.r(1));
}

test "op_addiu" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(2, 0x000f_ffff);
    cpu.exec(.{ .addiu = .{ .rt = 1, .rs = 2, .imm = 0x1000 } });
    try expectEqual(0x0010_0fff, cpu.r(1));

    cpu.set_r(2, 0x000f_8fff);
    cpu.exec(.{ .addiu = .{ .rt = 1, .rs = 2, .imm = -0x8000 } });
    try expectEqual(0x000f_0fff, cpu.r(1));
}

test "op_j" {
    // TODO
}

test "op_jal" {
    // TODO
}

test "op_or" {
    // TODO
}

test "op_cfc0" {
    // TODO
}

test "op_bne" {
    // TODO
}

test "op_beq" {
    // TODO
}

test "op_addi" {
    // TODO
}

test "op_lw" {
    // TODO
}

test "op_sltu" {
    // TODO
}

test "op_addu" {
    // TODO
}

test "op_sh" {
    // TODO
}

test "op_andi" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(2, 0x0000_ffff);
    cpu.exec(.{ .andi = .{ .rt = 1, .rs = 2, .imm = @bitCast(@as(u16, 0x0000)) } });
    try expectEqual(0x0000_0000, cpu.r(1));

    cpu.set_r(2, 0x0000_0000);
    cpu.exec(.{ .andi = .{ .rt = 1, .rs = 2, .imm = @bitCast(@as(u16, 0x0000)) } });
    try expectEqual(0x0000_0000, cpu.r(1));

    cpu.set_r(2, 0xffff_ffff);
    cpu.exec(.{ .andi = .{ .rt = 1, .rs = 2, .imm = @bitCast(@as(u16, 0xffff)) } });
    try expectEqual(0x0000_ffff, cpu.r(1));
}

test "op_sb" {
    // TODO
}

test "op_jr" {
    // TODO
}

test "op_lb" {
    // TODO
}

test "op_mfc0" {
    // TODO
}

test "op_and" {
    // TODO
}

test "op_add" {
    // TODO
}

test "op_bc0f" {
    // TODO
}
