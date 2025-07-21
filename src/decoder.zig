const root = @import("root.zig");
const RegIdx = root.RegIdx;
const Op = root.Op;
const Inst = root.Inst;
const ITypeArgs = root.ITypeArgs;
const RTypeArgs = root.RTypeArgs;
const JTypeArgs = root.JTypeArgs;

const COP_OPCODE_MASK: u32 = 0b00000_11111_000000_00000_00000_111111;
const COP_BC_OPCODE_MASK: u32 = 0b00000_11111_111111_00000_00000_000000;

const COP_OPCODE_MFC: u32 = 0b000000_00000_00000_00000_00000_000000;
const COP_OPCODE_CFC: u32 = 0b000000_00010_00000_00000_00000_000000;
const COP_OPCODE_MTC: u32 = 0b000000_00100_00000_00000_00000_000000;
const COP_OPCODE_CTC: u32 = 0b000000_00110_00000_00000_00000_000000;

const COP_BC_OPCODE_BCF: u32 = 0b000000_01000_00000_00000_00000_000000;
const COP_BC_OPCODE_BCT: u32 = 0b000000_01000_00001_00000_00000_000000;

const COP0_OPCODE_TLBR: u32 = 0b000000_10000_00000_00000_00000_000001;
const COP0_OPCODE_TLBWI: u32 = 0b000000_10000_00000_00000_00000_000010;
const COP0_OPCODE_TLBWR: u32 = 0b000000_10000_00000_00000_00000_000110;
const COP0_OPCODE_TLBP: u32 = 0b000000_10000_00000_00000_00000_001000;
const COP0_OPCODE_RFE: u32 = 0b000000_10000_00000_00000_00000_010000;

const RTypeArgsRaw = packed struct {
    special_op: u6,
    imm5: u5,
    rd: u5,
    rt: u5,
    rs: u5,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 26);
    }
};

const JTypeArgsRaw = packed struct {
    imm26: u26,
};

const ITypeArgsRaw = packed struct {
    imm16: i16,
    rt: u5,
    rs: u5,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 26);
    }
};

const ArgsRaw = packed union {
    r_type: RTypeArgsRaw,
    j_type: JTypeArgsRaw,
    i_type: ITypeArgsRaw,

    fn as_r_type(self: @This()) RTypeArgs {
        const args = self.r_type;
        return RTypeArgs{
            .rs = RegIdx{args.rs},
            .rt = RegIdx{args.rt},
            .rd = RegIdx{args.rd},
            .imm = args.imm5,
        };
    }
    fn as_j_type(self: @This()) JTypeArgs {
        const args = self.j_type;
        return JTypeArgs{
            .imm = args.imm26,
        };
    }
    fn as_i_type(self: @This()) ITypeArgs {
        const args = self.i_type;
        return ITypeArgs{
            .rs = RegIdx{args.rs},
            .rt = RegIdx{args.rt},
            .imm = args.imm16,
        };
    }
};

const InstRaw = packed struct {
    args: ArgsRaw,
    opcode: u6,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }
};

pub fn decode(v: u32) ?Inst {
    const inst_raw: InstRaw = @bitCast(v);
    const args = inst_raw.args;
    return switch (inst_raw.opcode) {
        // Special
        0x00 => switch (args.r_type.special_op) {
            0x00 => Inst{ .sll = args.as_r_type() },
            0x02 => Inst{ .srl = args.as_r_type() },
            0x03 => Inst{ .sra = args.as_r_type() },
            0x04 => Inst{ .sllv = args.as_r_type() },
            0x06 => Inst{ .srlv = args.as_r_type() },
            0x07 => Inst{ .srav = args.as_r_type() },

            0x08 => Inst{ .jr = args.as_r_type() },
            0x09 => Inst{ .jalr = args.as_r_type() },
            0x0c => Inst{ .sys = args.as_r_type() },
            0x0d => Inst{ .brk = args.as_r_type() },

            0x10 => Inst{ .mfhi = args.as_r_type() },
            0x11 => Inst{ .mthi = args.as_r_type() },
            0x12 => Inst{ .mflo = args.as_r_type() },
            0x13 => Inst{ .mtlo = args.as_r_type() },

            0x18 => Inst{ .mult = args.as_r_type() },
            0x19 => Inst{ .multu = args.as_r_type() },
            0x1a => Inst{ .div = args.as_r_type() },
            0x1b => Inst{ .divu = args.as_r_type() },

            0x20 => Inst{ .add = args.as_r_type() },
            0x21 => Inst{ .addu = args.as_r_type() },
            0x22 => Inst{ .sub = args.as_r_type() },
            0x23 => Inst{ .subu = args.as_r_type() },
            0x24 => Inst{ .@"and" = args.as_r_type() },
            0x25 => Inst{ .@"or" = args.as_r_type() },
            0x26 => Inst{ .xor = args.as_r_type() },
            0x27 => Inst{ .nor = args.as_r_type() },

            0x2a => Inst{ .slt = args.as_r_type() },
            0x2b => Inst{ .sltu = args.as_r_type() },

            else => null,
        },
        // BcondZ
        0x01 => switch (args.i_type.rt) {
            0x00 => Inst{ .bltz = args.as_i_type() },
            0x01 => Inst{ .bgez = args.as_i_type() },
            0x10 => Inst{ .bltzal = args.as_i_type() },
            0x11 => Inst{ .bgezal = args.as_i_type() },
            else => null,
        },
        0x02 => Inst{ .j = args.as_j_type() },
        0x03 => Inst{ .jal = args.as_j_type() },
        0x04 => Inst{ .beq = args.as_i_type() },
        0x05 => Inst{ .bne = args.as_i_type() },
        0x06 => Inst{ .blez = args.as_i_type() },
        0x07 => Inst{ .bgtz = args.as_i_type() },

        0x08 => Inst{ .addi = args.as_i_type() },
        0x09 => Inst{ .addiu = args.as_i_type() },
        0x0a => Inst{ .slti = args.as_i_type() },
        0x0b => Inst{ .sltiu = args.as_i_type() },
        0x0c => Inst{ .andi = args.as_i_type() },
        0x0d => Inst{ .ori = args.as_i_type() },
        0x0e => Inst{ .xori = args.as_i_type() },
        0x0f => Inst{ .lui = args.as_i_type() },

        // Cop0
        0x10 => switch (v & COP_OPCODE_MASK) {
            COP_OPCODE_MFC => Inst{ .mfc0 = args.as_r_type() },
            COP_OPCODE_MTC => Inst{ .mtc0 = args.as_r_type() },
            COP0_OPCODE_RFE => Inst{ .rfe = args.as_r_type() },
            else => null,
        },
        // Cop1
        0x11 => Inst{ .cop1 = args.as_j_type() },
        // Cop2
        0x12 => switch (v & COP_OPCODE_MASK) {
            COP_OPCODE_MFC => Inst{ .mfc2 = args.as_r_type() },
            COP_OPCODE_CFC => Inst{ .cfc2 = args.as_r_type() },
            COP_OPCODE_MTC => Inst{ .mtc2 = args.as_r_type() },
            COP_OPCODE_CTC => Inst{ .ctc2 = args.as_r_type() },
            else => null,
        },

        0x20 => Inst{ .lb = args.as_i_type() },
        0x21 => Inst{ .lh = args.as_i_type() },
        0x22 => Inst{ .lwl = args.as_i_type() },
        0x23 => Inst{ .lw = args.as_i_type() },
        0x24 => Inst{ .lbu = args.as_i_type() },
        0x25 => Inst{ .lhu = args.as_i_type() },
        0x26 => Inst{ .lwr = args.as_i_type() },

        0x28 => Inst{ .sb = args.as_i_type() },
        0x29 => Inst{ .sh = args.as_i_type() },
        0x2a => Inst{ .swl = args.as_i_type() },
        0x2b => Inst{ .sw = args.as_i_type() },
        0x2e => Inst{ .swr = args.as_i_type() },

        0x30 => Inst{ .lwc0 = args.as_i_type() },
        0x32 => Inst{ .lwc2 = args.as_i_type() },

        0x38 => Inst{ .swc0 = args.as_i_type() },
        0x3a => Inst{ .swc2 = args.as_i_type() },

        else => null,
    };
}

const std = @import("std");
const testing = std.testing;

test "decode" {
    _ = decode(0);
}
