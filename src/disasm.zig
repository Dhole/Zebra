const std = @import("std");

const decoder = @import("decoder.zig");
const decode = decoder.decode;
const root = @import("root.zig");
const RegIdx = root.RegIdx;
const Inst = root.Inst;
const InstArgs = root.InstArgs;
const Op = root.Op;
const ITypeArgs = root.ITypeArgs;
const RTypeArgs = root.RTypeArgs;
const JTypeArgs = root.JTypeArgs;

pub fn fmt_reg(idx: RegIdx) FmtReg {
    return .{ .idx = idx };
}

pub const FmtReg = struct {
    idx: RegIdx,
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
        const name = switch (self.idx[0]) {
            // 0 => "zr",
            0 => "zero", // duckstation compatible
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
        // try writer.print("r{d}", .{self.i});
    }
};

fn fmt_offset_reg(w: anytype, offset: i16, reg: RegIdx) !void {
    if (offset == 0) {
        // try w.print("({})", .{fmt_reg(reg)});
        // duckstation compatible
        try w.print("0x0({})", .{fmt_reg(reg)});
    } else if (offset > 0) {
        try w.print("0x{x}({})", .{ offset, fmt_reg(reg) });
    } else {
        try w.print("-0x{x}({})", .{ -offset, fmt_reg(reg) });
    }
}

fn fmt_i_ld_st_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    try w.print("{s} {}, ", .{ op, fmt_reg(a.rt) });
    try fmt_offset_reg(w, a.imm, a.rs);
}
fn fmt_i_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    // try w.print("{s} {}, {}, 0x{x}", .{ op, fmt_reg(a.rt), fmt_reg(a.rs), a.imm });
    // duckstation compatible
    const imm: u16 = @bitCast(a.imm);
    try w.print("{s} {}, {}, 0x{x:0>4}", .{ op, fmt_reg(a.rt), fmt_reg(a.rs), imm });
}
fn fmt_i_signed_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    // if (a.imm >= 0) {
    //     try w.print("{s} {}, {}, 0x{x}", .{ op, fmt_reg(a.rt), fmt_reg(a.rs), a.imm });
    // } else {
    //     try w.print("{s} {}, {}, -0x{x}", .{ op, fmt_reg(a.rt), fmt_reg(a.rs), -a.imm });
    // }
    // duckstation compatible
    try w.print("{s} {}, {}, {}", .{ op, fmt_reg(a.rt), fmt_reg(a.rs), a.imm });
}
fn fmt_i_branch_type(w: anytype, op: []const u8, pc: u32, a: ITypeArgs) !void {
    const offset: u32 = @bitCast(@as(i32, a.imm * 4));
    const dst = pc +% 4 +% offset;
    try w.print("{s} {}, {}, 0x{x:0>8}", .{ op, fmt_reg(a.rs), fmt_reg(a.rt), dst });
}
fn fmt_i_rt_imm_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    const imm: u16 = @bitCast(a.imm);
    // try w.print("{s} {}, 0x{x}", .{ op, fmt_reg(a.rt), imm });
    // duckstation compatible
    try w.print("{s} {}, 0x{x:0>4}", .{ op, fmt_reg(a.rt), imm });
}
fn fmt_i_rs_imm_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    try w.print("{s} {}, 0x{x}", .{ op, fmt_reg(a.rs), a.imm });
}
fn fmt_i_rs_imm_signed_type(w: anytype, op: []const u8, a: ITypeArgs) !void {
    if (a.imm >= 0) {
        try w.print("{s} {}, 0x{x}", .{ op, fmt_reg(a.rs), a.imm });
    } else {
        try w.print("{s} {}, -0x{x}", .{ op, fmt_reg(a.rs), -a.imm });
    }
}
fn fmt_i_rs_imm_branch_type(w: anytype, op: []const u8, pc: u32, a: ITypeArgs) !void {
    const offset: u32 = @bitCast(@as(i32, a.imm * 4));
    const dst = pc +% 4 +% offset;
    try w.print("{s} {}, 0x{x:0>8}", .{ op, fmt_reg(a.rs), dst });
}
fn fmt_r_type(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}, {}", .{ op, fmt_reg(a.rd), fmt_reg(a.rs), fmt_reg(a.rt) });
}
fn fmt_r_shift_type(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}, {}", .{ op, fmt_reg(a.rd), fmt_reg(a.rt), fmt_reg(a.rs) });
}
fn fmt_r_rs_rt_type(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}", .{ op, fmt_reg(a.rs), fmt_reg(a.rt) });
}
fn fmt_r_rd_rt_imm_type(w: anytype, op: []const u8, a: RTypeArgs) !void {
    // try w.print("{s} {}, {}, 0x{x}", .{ op, fmt_reg(a.rd), fmt_reg(a.rt), a.imm });
    // duckstation compatible
    try w.print("{s} {}, {}, {}", .{ op, fmt_reg(a.rd), fmt_reg(a.rt), a.imm });
}
fn fmt_j_type(w: anytype, op: []const u8, pc: u32, a: JTypeArgs) !void {
    try w.print("{s} 0x{x:0>8}", .{ op, (pc & 0xf0000000) + a.imm * 4 });
}
fn fmt_jr(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}", .{ op, fmt_reg(a.rs) });
}
fn fmt_jalr(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}", .{ op, fmt_reg(a.rd), fmt_reg(a.rs) });
}
fn fmt_mtc(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}", .{ op, fmt_reg(a.rt), a.rd[0] });
}
fn fmt_mfc(w: anytype, op: []const u8, a: RTypeArgs) !void {
    try w.print("{s} {}, {}", .{ op, fmt_reg(a.rt), a.rd[0] });
}

pub fn fmt_inst(inst: ?Inst, pc: u32) FmtInst {
    return .{ .inst = inst, .pc = pc };
}

pub const FmtInst = struct {
    inst: ?Inst,
    pc: u32,
    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const inst = self.inst orelse {
            try writer.print("???", .{});
            return;
        };
        // Pseudo opcodes (not duckstation compatible)
        // switch (inst) {
        //     .sll => |a| {
        //         if (a.rd[0] == 0 and a.rt[0] == 0 and a.imm == 0) {
        //             try writer.print("nop", .{});
        //             return;
        //         }
        //     },
        //     .@"or" => |a| {
        //         if (a.rt[0] == 0) {
        //             try writer.print("mov {}, {}", .{ fmt_reg(a.rd), fmt_reg(a.rs) });
        //             return;
        //         }
        //     },
        //     else => {},
        // }

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
            .addiu => |a| fmt_i_signed_type(writer, "addiu", a),
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
            .jr => |a| fmt_jr(writer, "jr", a),
            .jalr => |a| fmt_jalr(writer, "jalr", a),
            .beq => |a| fmt_i_branch_type(writer, "beq", self.pc, a),
            .bne => |a| fmt_i_branch_type(writer, "bne", self.pc, a),
            .blez => |a| fmt_i_rs_imm_branch_type(writer, "blez", self.pc, a),
            .bgtz => |a| fmt_i_rs_imm_branch_type(writer, "bgtz", self.pc, a),
            .bltz => |a| fmt_i_rs_imm_branch_type(writer, "bltz", self.pc, a),
            .bgez => |a| fmt_i_rs_imm_branch_type(writer, "bgez", self.pc, a),
            .bltzal => |a| fmt_i_rs_imm_branch_type(writer, "bltzal", self.pc, a),
            .bgezal => |a| fmt_i_rs_imm_branch_type(writer, "bgezal", self.pc, a),
            .sys => |a| fmt_r_type(writer, "sys", a),
            .brk => |a| fmt_r_type(writer, "brk", a),
            .lwc0 => |a| fmt_i_ld_st_type(writer, "lwc0", a),
            .swc0 => |a| fmt_i_ld_st_type(writer, "swc0", a),
            .mtc0 => |a| fmt_mtc(writer, "mtc0", a),
            .mfc0 => |a| fmt_mfc(writer, "mfc0", a),
            .ctc0 => |a| fmt_r_type(writer, "ctc0", a),
            .cfc0 => |a| fmt_r_type(writer, "cfc0", a),
            .bc0f => |a| fmt_i_type(writer, "bc0f", a),
            .bc0t => |a| fmt_i_type(writer, "bc0t", a),
            .lwc2 => |a| fmt_i_ld_st_type(writer, "lwc2", a),
            .swc2 => |a| fmt_i_ld_st_type(writer, "swc2", a),
            .mtc2 => |a| fmt_mtc(writer, "mtc2", a),
            .mfc2 => |a| fmt_mfc(writer, "mfc2", a),
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

pub fn print_disasm(writer: anytype, pc: u32, inst_raw: u32, inst: ?Inst) !void {
    writer.print("{x:0>8}: {x:0>8} {}", .{ pc, inst_raw, fmt_inst(inst, pc) }) catch @panic("write");
}
