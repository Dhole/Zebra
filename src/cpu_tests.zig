const std = @import("std");
const testing = std.testing;
const expectEqual = std.testing.expectEqual;

const root = @import("root.zig");
const r = root.r;
const _cpu = @import("cpu.zig");
const Cpu = _cpu.Cpu;
const Cfg = _cpu.Cfg;
const SIZE_BIOS = _cpu.SIZE_BIOS;
const VADDR_KUSEG = _cpu.VADDR_KUSEG;

const w = std.io.getStdOut().writer();
const cfg = Cfg{ .dbg = true };

fn cpu_init() !Cpu(@TypeOf(w), cfg) {
    const allocator = std.testing.allocator;

    const bios_file = try std.fs.cwd().openFile("bios/SCPH1001.BIN", .{});
    const bios = try bios_file.reader().readAllAlloc(allocator, SIZE_BIOS);
    defer allocator.free(bios);

    const cpu = try Cpu(@TypeOf(w), cfg).init(allocator, bios, w);
    return cpu;
}

test "op_lui_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.exec(.{ .lui = .{ .rt = r(1), .rs = r(0), .imm = @bitCast(@as(u16, 0xffff)) } });
    try expectEqual(0xffff_0000, cpu.r(r(1)));
}

test "op_ori_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(r(2), 0x0000_ffff);
    cpu.exec(.{ .ori = .{ .rt = r(1), .rs = r(2), .imm = @bitCast(@as(u16, 0x0000)) } });
    try expectEqual(0x0000_ffff, cpu.r(r(1)));

    cpu.set_r(r(2), 0x0000_0000);
    cpu.exec(.{ .ori = .{ .rt = r(1), .rs = r(2), .imm = @bitCast(@as(u16, 0xffff)) } });
    try expectEqual(0x0000_ffff, cpu.r(r(1)));

    cpu.set_r(r(2), 0xffff_0000);
    cpu.exec(.{ .ori = .{ .rt = r(1), .rs = r(2), .imm = @bitCast(@as(u16, 0xffff)) } });
    try expectEqual(0xffff_ffff, cpu.r(r(1)));
}

test "op_sw_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(r(2), VADDR_KUSEG);
    cpu.set_r(r(1), 0x1234_5678);
    cpu.exec(.{ .sw = .{ .rt = r(1), .rs = r(2), .imm = 0x0004 } });
    try expectEqual(0x1234_5678, cpu.read(u32, VADDR_KUSEG + 4));

    cpu.set_r(r(2), VADDR_KUSEG + 16);
    cpu.set_r(r(1), 0x8765_4321);
    cpu.exec(.{ .sw = .{ .rt = r(1), .rs = r(2), .imm = -0x0004 } });
    try expectEqual(0x8765_4321, cpu.read(u32, VADDR_KUSEG + 16 - 4));
}

test "op_sll_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(r(2), 0x0000_ffff);
    cpu.exec(.{ .sll = .{ .rd = r(1), .rt = r(2), .imm = 4, .rs = r(0) } });
    try expectEqual(0x000f_fff0, cpu.r(r(1)));

    cpu.set_r(r(2), 0x1111_ffff);
    cpu.exec(.{ .sll = .{ .rd = r(1), .rt = r(2), .imm = 4, .rs = r(0) } });
    try expectEqual(0x111f_fff0, cpu.r(r(1)));
}

test "op_addiu_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(r(2), 0x000f_ffff);
    cpu.exec(.{ .addiu = .{ .rt = r(1), .rs = r(2), .imm = 0x1000 } });
    try expectEqual(0x0010_0fff, cpu.r(r(1)));

    cpu.set_r(r(2), 0x000f_8fff);
    cpu.exec(.{ .addiu = .{ .rt = r(1), .rs = r(2), .imm = -0x8000 } });
    try expectEqual(0x000f_0fff, cpu.r(r(1)));
}

test "op_j_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.pc = 0x0000_1234;
    cpu.write(u32, cpu.pc + 4, 0x0000_0000); // nop at the branch delay slot
    cpu.exec(.{ .j = .{ .imm = 0x0000_1000 } });
    try expectEqual(0x0000_4000, cpu.pc);

    cpu.pc = 0x0000_1234;
    cpu.write(u32, cpu.pc + 4, 0x24080b88); // `addiu r8, r0, 0xb88` at the branch delay slot
    cpu.exec(.{ .j = .{ .imm = 0x0000_1000 } });
    try expectEqual(0x0000_4000, cpu.pc);
    try expectEqual(0xb88, cpu.r(r(8)));
}

test "op_jal_" {
    // TODO
}

test "op_or_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(r(2), 0x0000_ffff);
    cpu.set_r(r(3), 0x0000_0000);
    cpu.exec(.{ .@"or" = .{ .rd = r(1), .rs = r(2), .rt = r(3), .imm = 0 } });
    try expectEqual(0x0000_ffff, cpu.r(r(1)));

    cpu.set_r(r(2), 0x0000_0000);
    cpu.set_r(r(3), 0x0000_ffff);
    cpu.exec(.{ .@"or" = .{ .rd = r(1), .rs = r(2), .rt = r(3), .imm = 0 } });
    try expectEqual(0x0000_ffff, cpu.r(r(1)));

    cpu.set_r(r(2), 0xffff_0000);
    cpu.set_r(r(3), 0x0000_ffff);
    cpu.exec(.{ .@"or" = .{ .rd = r(1), .rs = r(2), .rt = r(3), .imm = 0 } });
    try expectEqual(0xffff_ffff, cpu.r(r(1)));
}

test "op_cfc0_" {
    // TODO
}

test "op_bne_" {
    // TODO
}

test "op_beq_" {
    // TODO
}

test "op_addi_" {
    // TODO
}

test "op_lw_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.write(u32, 0x30, 0x1122_3344);
    cpu.write(u32, 0x50, 0x1234_5678);

    cpu.pc = 0;
    cpu.set_r(r(2), 0x40);
    cpu.write(u32, cpu.pc + 4, 0x0000_0000); // nop at the branch delay slot
    cpu.exec(.{ .lw = .{ .rt = r(1), .rs = r(2), .imm = 0x10 } });
    try expectEqual(0x1234_5678, cpu.r(r(1)));

    cpu.pc = 0;
    cpu.set_r(r(2), 0x40);
    cpu.write(u32, cpu.pc + 4, 0x0000_0000); // nop at the branch delay slot
    cpu.exec(.{ .lw = .{ .rt = r(1), .rs = r(2), .imm = -0x10 } });
    try expectEqual(0x1122_3344, cpu.r(r(1)));

    cpu.pc = 0;
    cpu.set_r(r(2), 0x40);
    cpu.write(u32, cpu.pc + 4, 0x24880b88); // `addiu r8, r4, 0xb88` at the load delay slot
    cpu.exec(.{ .lw = .{ .rt = r(4), .rs = r(2), .imm = 0x10 } });
    // The delay slot instruction reads r4 before the load writes to it (so it
    // reads v=0 instead of v=1234_5678)
    try expectEqual(0x1234_5678, cpu.r(r(4)));
    try expectEqual(0xb88, cpu.r(r(8)));

    cpu.pc = 0;
    cpu.set_r(r(2), 0x40);
    cpu.write(u32, cpu.pc + 4, 0x24080b88); // `addiu r8, r0, 0xb88` at the load delay slot
    cpu.exec(.{ .lw = .{ .rt = r(8), .rs = r(2), .imm = 0x10 } });
    // The dest register is overwritten by the load delay slot instruction
    try expectEqual(0xb88, cpu.r(r(8)));
}

test "op_sltu_" {
    // TODO
}

test "op_addu_" {
    // TODO
}

test "op_sh_" {
    // TODO
}

test "op_andi_" {
    var cpu = try cpu_init();
    defer cpu.deinit();

    cpu.set_r(r(2), 0x0000_ffff);
    cpu.exec(.{ .andi = .{ .rt = r(1), .rs = r(2), .imm = @bitCast(@as(u16, 0x0000)) } });
    try expectEqual(0x0000_0000, cpu.r(r(1)));

    cpu.set_r(r(2), 0x0000_0000);
    cpu.exec(.{ .andi = .{ .rt = r(1), .rs = r(2), .imm = @bitCast(@as(u16, 0x0000)) } });
    try expectEqual(0x0000_0000, cpu.r(r(1)));

    cpu.set_r(r(2), 0xffff_ffff);
    cpu.exec(.{ .andi = .{ .rt = r(1), .rs = r(2), .imm = @bitCast(@as(u16, 0xffff)) } });
    try expectEqual(0x0000_ffff, cpu.r(r(1)));
}

test "op_sb_" {
    // TODO
}

test "op_jr_" {
    // TODO
}

test "op_lb_" {
    // TODO
}

test "op_mfc0_" {
    // TODO
}

test "op_and_" {
    // TODO
}

test "op_add_" {
    // TODO
}

test "op_bc0f_" {
    // TODO
}
