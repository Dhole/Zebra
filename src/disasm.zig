const std = @import("std");

const decoder = @import("decoder.zig");
const decode = decoder.decode;
const root = @import("root.zig");
const Inst = root.Inst;
const InstArgs = root.InstArgs;
const Op = root.Op;
const ITypeArgs = root.ITypeArgs;
const RTypeArgs = root.RTypeArgs;
const JTypeArgs = root.JTypeArgs;

pub const FmtReg = struct {
    v: u8,
    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // zr: Constant Zero
        // at: Reserved for the assembler
        // v0-v1: Values for results and expression evaluation
        // a0-a3: Arguments
        // t0-t9: Temporaries (not preserved accross calls)
        // s0-s7: Saved (preserved accross calls)
        // k0-k1: Reserved for OS Kernel
        // gp: Global Pointer
        // sp: Stak Pointer
        // fp: Frame Pointer
        // ra: Return Address
        const name = switch (self.v) {
            0 => "zr",
            1 => "at",
            2 => "v0",
            3 => "v1",
            4 => "a0",
            5 => "a1",
            6 => "a2",
            7 => "a3",
            8 => "t0",
            9 => "t1",
            10 => "t2",
            11 => "t3",
            12 => "t4",
            13 => "t5",
            14 => "t6",
            15 => "t7",
            16 => "s0",
            17 => "s1",
            18 => "s2",
            19 => "s3",
            20 => "s4",
            21 => "s5",
            22 => "s6",
            23 => "s7",
            24 => "t8",
            25 => "t9",
            26 => "k0",
            27 => "k1",
            28 => "gp",
            29 => "sp",
            30 => "fp",
            31 => "ra",
            else => unreachable,
        };
        try writer.print("{s}", .{name});
        // try writer.print("r{d}", .{self.v});
    }
};

fn fmt_offset_reg(w: anytype, offset: i16, reg: u8) !void {
    if (offset == 0) {
        try w.print("({})", .{FmtReg{ .v = reg }});
    } else if (offset > 0) {
        try w.print("0x{x}({})", .{ offset, FmtReg{ .v = reg } });
    } else {
        try w.print("-0x{x}({})", .{ -offset, FmtReg{ .v = reg } });
    }
}

fn fmt_i_ld_st_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    try w.print("{s} {}, ", .{ op, FmtReg{ .v = a.rt } });
    try fmt_offset_reg(w, a.imm, a.rs);
}
fn fmt_i_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    try w.print("{s} {}, {}, 0x{x}", .{ op, FmtReg{ .v = a.rt }, FmtReg{ .v = a.rs }, a.imm });
}
fn fmt_i_signed_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    if (a.imm >= 0) {
        try w.print("{s} {}, {}, 0x{x}", .{ op, FmtReg{ .v = a.rt }, FmtReg{ .v = a.rs }, a.imm });
    } else {
        try w.print("{s} {}, {}, -0x{x}", .{ op, FmtReg{ .v = a.rt }, FmtReg{ .v = a.rs }, -a.imm });
    }
}
fn fmt_i_rt_imm_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    const imm: u16 = @bitCast(a.imm);
    try w.print("{s} {}, 0x{x}", .{ op, FmtReg{ .v = a.rt }, imm });
}
fn fmt_i_rs_imm_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    try w.print("{s} {}, 0x{x}", .{ op, FmtReg{ .v = a.rs }, a.imm });
}
fn fmt_i_rs_imm_signed_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    if (a.imm >= 0) {
        try w.print("{s} {}, 0x{x}", .{ op, FmtReg{ .v = a.rs }, a.imm });
    } else {
        try w.print("{s} {}, -0x{x}", .{ op, FmtReg{ .v = a.rs }, -a.imm });
    }
}
fn fmt_r_type(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}, {}", .{ op, FmtReg{ .v = a.rd }, FmtReg{ .v = a.rs }, FmtReg{ .v = a.rt } });
}
fn fmt_r_shift_type(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}, {}", .{ op, FmtReg{ .v = a.rd }, FmtReg{ .v = a.rt }, FmtReg{ .v = a.rs } });
}
fn fmt_r_rs_rt_type(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}", .{ op, FmtReg{ .v = a.rs }, FmtReg{ .v = a.rt } });
}
fn fmt_r_rd_rt_imm_type(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}, 0x{x}", .{ op, FmtReg{ .v = a.rd }, FmtReg{ .v = a.rt }, a.imm });
}
fn fmt_j_type(w: anytype, op: []const u8, pc: u32, a: JTypeArgs) !void {
    try w.print("{s} 0x{x}", .{ op, (pc & 0xf0000000) + a.imm * 4 });
}

pub const FmtInst = struct {
    v: Inst,
    pc: u32,
    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const inst = self.v;
        // Pseudo opcodes
        switch (inst) {
            .sll => |a| {
                if (a.rd == 0 and a.rt == 0 and a.imm == 0) {
                    try writer.print("nop", .{});
                    return;
                }
            },
            .@"or" => |a| {
                if (a.rt == 0) {
                    try writer.print("mov {}, {}", .{ FmtReg{ .v = a.rd }, FmtReg{ .v = a.rs } });
                    return;
                }
            },
            else => {},
        }

        try switch (inst) {
            .lb => |a| fmt_i_ld_st_type(writer, "lb", a),
            .lbu => |a| fmt_i_ld_st_type(writer, "lbu", a),
            .lh => |a| fmt_i_ld_st_type(writer, "lh", a),
            .lhu => |a| fmt_i_ld_st_type(writer, "lhu", a),
            .lw => |a| fmt_i_ld_st_type(writer, "lw", a),
            .lwl => |a| fmt_i_ld_st_type(writer, "lwl", a),
            .lwr => |a| fmt_i_ld_st_type(writer, "lwr", a),
            .sb => |a| fmt_i_ld_st_type(writer, "sb", a),
            .sh => |a| fmt_i_ld_st_type(writer, "sh", a),
            .sw => |a| fmt_i_ld_st_type(writer, "sw", a),
            .swl => |a| fmt_i_ld_st_type(writer, "swl", a),
            .swr => |a| fmt_i_ld_st_type(writer, "swr", a),
            .addi => |a| fmt_i_signed_type(writer, "addi", a),
            .addiu => |a| fmt_i_type(writer, "addiu", a),
            .slti => |a| fmt_i_signed_type(writer, "slti", a),
            .sltiu => |a| fmt_i_type(writer, "sltiu", a),
            .andi => |a| fmt_i_type(writer, "andi", a),
            .ori => |a| fmt_i_type(writer, "ori", a),
            .xori => |a| fmt_i_type(writer, "xori", a),
            .lui => |a| fmt_i_rt_imm_type(writer, "lui", a),
            .add => |a| fmt_r_type(writer, "add", a),
            .addu => |a| fmt_r_type(writer, "addu", a),
            .sub => |a| fmt_r_type(writer, "sub", a),
            .subu => |a| fmt_r_type(writer, "subu", a),
            .slt => |a| fmt_r_type(writer, "slt", a),
            .sltu => |a| fmt_r_type(writer, "sltu", a),
            .@"and" => |a| fmt_r_type(writer, "and", a),
            .@"or" => |a| fmt_r_type(writer, "or", a),
            .xor => |a| fmt_r_type(writer, "xor", a),
            .nor => |a| fmt_r_type(writer, "nor", a),
            .sll => |a| fmt_r_rd_rt_imm_type(writer, "sll", a),
            .srl => |a| fmt_r_rd_rt_imm_type(writer, "srl", a),
            .sra => |a| fmt_r_rd_rt_imm_type(writer, "sra", a),
            .sllv => |a| fmt_r_shift_type(writer, "sllv", a),
            .srlv => |a| fmt_r_shift_type(writer, "srlv", a),
            .srav => |a| fmt_r_shift_type(writer, "srav", a),
            .mult => |a| fmt_r_rs_rt_type(writer, "mult", a),
            .multu => |a| fmt_r_rs_rt_type(writer, "multu", a),
            .div => |a| fmt_r_rs_rt_type(writer, "div", a),
            .divu => |a| fmt_r_rs_rt_type(writer, "divu", a),
            .mfhi => |a| fmt_r_type(writer, "mfhi", a),
            .mflo => |a| fmt_r_type(writer, "mflo", a),
            .mthi => |a| fmt_r_type(writer, "mthi", a),
            .mtlo => |a| fmt_r_type(writer, "mtlo", a),
            .j => |a| fmt_j_type(writer, "j", self.pc, a),
            .jal => |a| fmt_j_type(writer, "jal", self.pc, a),
            .jr => |a| fmt_r_type(writer, "jr", a),
            .jalr => |a| fmt_r_type(writer, "jalr", a),
            .beq => |a| fmt_i_signed_type(writer, "beq", a),
            .bne => |a| fmt_i_signed_type(writer, "bne", a),
            .blez => |a| fmt_i_rs_imm_type(writer, "blez", a),
            .bgtz => |a| fmt_i_rs_imm_type(writer, "bgtz", a),
            .bltz => |a| fmt_i_rs_imm_type(writer, "bltz", a),
            .bgez => |a| fmt_i_rs_imm_type(writer, "bgez", a),
            .bltzal => |a| fmt_i_rs_imm_type(writer, "bltzal", a),
            .bgezal => |a| fmt_i_rs_imm_type(writer, "bgezal", a),
            .sys => |a| fmt_r_type(writer, "sys", a),
            .brk => |a| fmt_r_type(writer, "brk", a),
            .lwc0 => |a| fmt_i_ld_st_type(writer, "lwc0", a),
            .swc0 => |a| fmt_i_ld_st_type(writer, "swc0", a),
            .mtc0 => |a| fmt_r_type(writer, "mtc0", a),
            .mfc0 => |a| fmt_r_type(writer, "mfc0", a),
            .ctc0 => |a| fmt_r_type(writer, "ctc0", a),
            .cfc0 => |a| fmt_r_type(writer, "cfc0", a),
            .bc0f => |a| fmt_i_type(writer, "bc0f", a),
            .bc0t => |a| fmt_i_type(writer, "bc0t", a),
            .lwc2 => |a| fmt_i_ld_st_type(writer, "lwc2", a),
            .swc2 => |a| fmt_i_ld_st_type(writer, "swc2", a),
            .mtc2 => |a| fmt_r_type(writer, "mtc2", a),
            .mfc2 => |a| fmt_r_type(writer, "mfc2", a),
            .ctc2 => |a| fmt_r_type(writer, "ctc2", a),
            .cfc2 => |a| fmt_r_type(writer, "cfc2", a),
            .bc2f => |a| fmt_i_type(writer, "bc2f", a),
            .bc2t => |a| fmt_i_type(writer, "bc2t", a),
            .tlbr => |a| fmt_r_type(writer, "tlbr", a),
            .tlbwi => |a| fmt_r_type(writer, "tlbwi", a),
            .tlbwr => |a| fmt_r_type(writer, "tlbwr", a),
            .tlbp => |a| fmt_r_type(writer, "tlbp", a),
            .rfe => |a| fmt_r_type(writer, "rfe", a),
        };
    }
};
