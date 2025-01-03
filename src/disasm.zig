const std = @import("std");

const decoder = @import("decoder.zig");
const decode = decoder.decode;
const root = @import("root.zig");
const Inst = root.Inst;
const InstArgs = root.InstArgs;
const Op = root.Op;

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
    } else {
        try w.print("0x{x}({})", .{ offset, FmtReg{ .v = reg } });
    }
}

fn fmt_i_ld_st_type(w: anytype, _op: []const u8, args: InstArgs) !void {
    const a = args.i_type;
    try w.print("{s} {}, ", .{ _op, FmtReg{ .v = a.rt } });
    try fmt_offset_reg(w, a.imm, a.rs);
}
fn fmt_i_type(w: anytype, _op: []const u8, args: InstArgs) !void {
    const a = args.i_type;
    try w.print("{s} {}, {}, 0x{x}", .{ _op, FmtReg{ .v = a.rt }, FmtReg{ .v = a.rs }, a.imm });
}
fn fmt_i_rt_imm_type(w: anytype, _op: []const u8, args: InstArgs) !void {
    const a = args.i_type;
    try w.print("{s} {}, 0x{x}", .{ _op, FmtReg{ .v = a.rt }, a.imm });
}
fn fmt_i_rs_imm_type(w: anytype, _op: []const u8, args: InstArgs) !void {
    const a = args.i_type;
    try w.print("{s} {}, 0x{x}", .{ _op, FmtReg{ .v = a.rs }, a.imm });
}
fn fmt_r_type(w: anytype, _op: []const u8, args: InstArgs) !void {
    const a = args.r_type;
    try w.print("{s} {}, {}, {}", .{ _op, FmtReg{ .v = a.rd }, FmtReg{ .v = a.rs }, FmtReg{ .v = a.rt } });
}
fn fmt_r_shift_type(w: anytype, _op: []const u8, args: InstArgs) !void {
    const a = args.r_type;
    try w.print("{s} {}, {}, {}", .{ _op, FmtReg{ .v = a.rd }, FmtReg{ .v = a.rt }, FmtReg{ .v = a.rs } });
}
fn fmt_r_rs_rt_type(w: anytype, _op: []const u8, args: InstArgs) !void {
    const a = args.r_type;
    try w.print("{s} {}, {}", .{ _op, FmtReg{ .v = a.rs }, FmtReg{ .v = a.rt } });
}
fn fmt_r_rd_rt_imm_type(w: anytype, _op: []const u8, args: InstArgs) !void {
    const a = args.r_type;
    try w.print("{s} {}, {}, 0x{x}", .{ _op, FmtReg{ .v = a.rd }, FmtReg{ .v = a.rt }, a.imm5 });
}
fn fmt_j_type(w: anytype, _op: []const u8, pc: u32, args: InstArgs) !void {
    const a = args.j_type;
    try w.print("{s} 0x{x}", .{ _op, (pc & 0xf0000000) + a.offset });
}

pub const FmtInst = struct {
    v: Inst,
    pc: u32,
    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const inst = self.v;
        // Pseudo opcodes
        switch (inst.op) {
            Op.SLL => {
                const a = inst.args.r_type;
                if (a.rd == 0 and a.rt == 0 and a.imm5 == 0) {
                    try writer.print("nop", .{});
                    return;
                }
            },
            Op.OR => {
                const a = inst.args.r_type;
                if (a.rt == 0) {
                    try writer.print("mov {}, {}", .{ FmtReg{ .v = a.rd }, FmtReg{ .v = a.rs } });
                    return;
                }
            },
            else => {},
        }

        try switch (inst.op) {
            Op.LB => fmt_i_ld_st_type(writer, "lb", inst.args),
            Op.LBU => fmt_i_ld_st_type(writer, "lbu", inst.args),
            Op.LH => fmt_i_ld_st_type(writer, "lh", inst.args),
            Op.LHU => fmt_i_ld_st_type(writer, "lhu", inst.args),
            Op.LW => fmt_i_ld_st_type(writer, "lw", inst.args),
            Op.LWL => fmt_i_ld_st_type(writer, "lwl", inst.args),
            Op.LWR => fmt_i_ld_st_type(writer, "lwr", inst.args),
            Op.SB => fmt_i_ld_st_type(writer, "sb", inst.args),
            Op.SH => fmt_i_ld_st_type(writer, "sh", inst.args),
            Op.SW => fmt_i_ld_st_type(writer, "sw", inst.args),
            Op.SWL => fmt_i_ld_st_type(writer, "swl", inst.args),
            Op.SWR => fmt_i_ld_st_type(writer, "swr", inst.args),
            Op.ADDI => fmt_i_type(writer, "addi", inst.args),
            Op.ADDIU => fmt_i_type(writer, "addiu", inst.args),
            Op.SLTI => fmt_i_type(writer, "slti", inst.args),
            Op.SLTIU => fmt_i_type(writer, "sltiu", inst.args),
            Op.ANDI => fmt_i_type(writer, "andi", inst.args),
            Op.ORI => fmt_i_type(writer, "ori", inst.args),
            Op.XORI => fmt_i_type(writer, "xori", inst.args),
            Op.LUI => fmt_i_rt_imm_type(writer, "lui", inst.args),
            Op.ADD => fmt_r_type(writer, "add", inst.args),
            Op.ADDU => fmt_r_type(writer, "addu", inst.args),
            Op.SUB => fmt_r_type(writer, "sub", inst.args),
            Op.SUBU => fmt_r_type(writer, "subu", inst.args),
            Op.SLT => fmt_r_type(writer, "slt", inst.args),
            Op.SLTU => fmt_r_type(writer, "sltu", inst.args),
            Op.AND => fmt_r_type(writer, "and", inst.args),
            Op.OR => fmt_r_type(writer, "or", inst.args),
            Op.XOR => fmt_r_type(writer, "xor", inst.args),
            Op.NOR => fmt_r_type(writer, "nor", inst.args),
            Op.SLL => fmt_r_rd_rt_imm_type(writer, "sll", inst.args),
            Op.SRL => fmt_r_rd_rt_imm_type(writer, "srl", inst.args),
            Op.SRA => fmt_r_rd_rt_imm_type(writer, "sra", inst.args),
            Op.SLLV => fmt_r_shift_type(writer, "sllv", inst.args),
            Op.SRLV => fmt_r_shift_type(writer, "srlv", inst.args),
            Op.SRAV => fmt_r_shift_type(writer, "srav", inst.args),
            Op.MULT => fmt_r_rs_rt_type(writer, "mult", inst.args),
            Op.MULTU => fmt_r_rs_rt_type(writer, "multu", inst.args),
            Op.DIV => fmt_r_rs_rt_type(writer, "div", inst.args),
            Op.DIVU => fmt_r_rs_rt_type(writer, "divu", inst.args),
            Op.MFHI => fmt_r_type(writer, "mfhi", inst.args),
            Op.MFLO => fmt_r_type(writer, "mflo", inst.args),
            Op.MTHI => fmt_r_type(writer, "mthi", inst.args),
            Op.MTLO => fmt_r_type(writer, "mtlo", inst.args),
            Op.J => fmt_j_type(writer, "j", self.pc, inst.args),
            Op.JAL => fmt_j_type(writer, "jal", self.pc, inst.args),
            Op.JR => fmt_r_type(writer, "jr", inst.args),
            Op.JALR => fmt_r_type(writer, "jalr", inst.args),
            Op.BEQ => fmt_i_type(writer, "beq", inst.args),
            Op.BNE => fmt_i_type(writer, "bne", inst.args),
            Op.BLEZ => fmt_i_rs_imm_type(writer, "blez", inst.args),
            Op.BGTZ => fmt_i_rs_imm_type(writer, "bgtz", inst.args),
            Op.BLTZ => fmt_i_rs_imm_type(writer, "bltz", inst.args),
            Op.BGEZ => fmt_i_rs_imm_type(writer, "bgez", inst.args),
            Op.BLTZAL => fmt_i_rs_imm_type(writer, "bltzal", inst.args),
            Op.BGEZAL => fmt_i_rs_imm_type(writer, "bgezal", inst.args),
            Op.SYS => fmt_r_type(writer, "sys", inst.args),
            Op.BRK => fmt_r_type(writer, "brk", inst.args),
            Op.LWC0 => fmt_i_ld_st_type(writer, "lwc0", inst.args),
            Op.SWC0 => fmt_i_ld_st_type(writer, "swc0", inst.args),
            Op.MTC0 => fmt_i_type(writer, "mtc0", inst.args),
            Op.MFC0 => fmt_i_type(writer, "mfc0", inst.args),
            Op.CTC0 => fmt_i_type(writer, "ctc0", inst.args),
            Op.CFC0 => fmt_i_type(writer, "cfc0", inst.args),
            Op.COP0 => fmt_i_type(writer, "cop0", inst.args),
            Op.BC0F => fmt_i_type(writer, "bc0f", inst.args),
            Op.BC0T => fmt_i_type(writer, "bc0t", inst.args),
            Op.LWC2 => fmt_i_ld_st_type(writer, "lwc2", inst.args),
            Op.SWC2 => fmt_i_ld_st_type(writer, "swc2", inst.args),
            Op.MTC2 => fmt_i_type(writer, "mtc2", inst.args),
            Op.MFC2 => fmt_i_type(writer, "mfc2", inst.args),
            Op.CTC2 => fmt_i_type(writer, "ctc2", inst.args),
            Op.CFC2 => fmt_i_type(writer, "cfc2", inst.args),
            Op.COP2 => fmt_i_type(writer, "cop2", inst.args),
            Op.BC2F => fmt_i_type(writer, "bc2f", inst.args),
            Op.BC2T => fmt_i_type(writer, "bc2t", inst.args),
            Op.TLBR => fmt_r_type(writer, "tlbr", inst.args),
            Op.TLBWI => fmt_r_type(writer, "tlbwi", inst.args),
            Op.TLBWR => fmt_r_type(writer, "tlbwr", inst.args),
            Op.TLBP => fmt_r_type(writer, "tlbp", inst.args),
            Op.RFE => fmt_r_type(writer, "rfe", inst.args),
        };
    }
};
