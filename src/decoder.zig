const root = @import("root.zig");
const Op = root.Op;
const Inst = root.Inst;
// Masks & Shifts

const OPCODE_MASK: u32 = 0b111111_00000_00000_00000_00000_000000;
const OPCODE_SHIFT = 26;
const RS_MASK: u32 = 0b000000_11111_00000_00000_00000_000000;
const RS_SHIFT = 21;
const RT_MASK: u32 = 0b000000_00000_11111_00000_00000_000000;
const RT_SHIFT = 16;
const RD_MASK: u32 = 0b000000_00000_00000_11111_00000_000000;
const RD_SHIFT = 11;
const IMM5_MASK: u32 = 0b000000_00000_00000_00000_11111_000000;
const IMM5_SHIFT = 6;
const IMM16_MASK: u32 = 0b000000_00000_00000_11111_11111_111111;
const IMM16_SHIFT = 0;
const IMM25_MASK: u32 = 0b000000_01111_11111_11111_11111_111111;
const IMM25_SHIFT = 0;
const IMM26_MASK: u32 = 0b000000_11111_11111_11111_11111_111111;
const IMM26_SHIFT = 0;
const COP_MASK: u32 = 0b000011_00000_00000_00000_00000_000000;
const COP_SHIFT = 26;
const SPECIAL_OPCODE_MASK: u32 = 0b00000_00000_000000_00000_00000_111111;
const SPECIAL_OPCODE_SHIFT = 0;
const BCONDZ_OPCODE_MASK: u32 = RT_MASK;
const BCONDZ_OPCODE_SHIFT = RT_SHIFT;
const COP_OPCODE_MASK: u32 = 0b00000_11111_000000_00000_00000_111111;
const COP_OPCODE_SHIFT = 0;
const COP_BC_OPCODE_MASK: u32 = 0b00000_11111_111111_00000_00000_000000;
const COP_BC_OPCODE_SHIFT = 0;

// Opcode

const OPCODE_SPECIAL: u8 = 0x00;
const OPCODE_BcondZ: u8 = 0x01;
const OPCODE_J: u8 = 0x02;
const OPCODE_JAL: u8 = 0x03;
const OPCODE_BEQ: u8 = 0x04;
const OPCODE_BNE: u8 = 0x05;
const OPCODE_BLEZ: u8 = 0x06;
const OPCODE_BGTZ: u8 = 0x07;

const OPCODE_ADDI: u8 = 0x08;
const OPCODE_ADDIU: u8 = 0x09;
const OPCODE_SLTI: u8 = 0x0a;
const OPCODE_SLTIU: u8 = 0x0b;
const OPCODE_ANDI: u8 = 0x0c;
const OPCODE_ORI: u8 = 0x0d;
const OPCODE_XORI: u8 = 0x0e;
const OPCODE_LUI: u8 = 0x0f;

const OPCODE_COP0: u8 = 0x10;
const OPCODE_COP1: u8 = 0x11;
const OPCODE_COP2: u8 = 0x12;
const OPCODE_COP3: u8 = 0x13;

const OPCODE_LB: u8 = 0x20;
const OPCODE_LH: u8 = 0x21;
const OPCODE_LWL: u8 = 0x22;
const OPCODE_LW: u8 = 0x23;
const OPCODE_LBU: u8 = 0x24;
const OPCODE_LHU: u8 = 0x25;
const OPCODE_LWR: u8 = 0x26;

const OPCODE_SB: u8 = 0x28;
const OPCODE_SH: u8 = 0x29;
const OPCODE_SWL: u8 = 0x2a;
const OPCODE_SW: u8 = 0x2b;
const OPCODE_SWR: u8 = 0x2e;

const OPCODE_LWC0: u8 = 0x30;
const OPCODE_LWC1: u8 = 0x31;
const OPCODE_LWC2: u8 = 0x32;
const OPCODE_LWC3: u8 = 0x33;

const OPCODE_SWC0: u8 = 0x38;
const OPCODE_SWC1: u8 = 0x39;
const OPCODE_SWC2: u8 = 0x3a;
const OPCODE_SWC3: u8 = 0x3b;

// Special Opcode

const SPECIAL_OPCODE_SLL: u8 = 0x00;
const SPECIAL_OPCODE_SRL: u8 = 0x02;
const SPECIAL_OPCODE_SRA: u8 = 0x03;
const SPECIAL_OPCODE_SLLV: u8 = 0x04;
const SPECIAL_OPCODE_SRLV: u8 = 0x06;
const SPECIAL_OPCODE_SRAV: u8 = 0x07;

const SPECIAL_OPCODE_JR: u8 = 0x08;
const SPECIAL_OPCODE_JALR: u8 = 0x09;
const SPECIAL_OPCODE_SYS: u8 = 0x0c;
const SPECIAL_OPCODE_BRK: u8 = 0x0d;

const SPECIAL_OPCODE_MFHI: u8 = 0x10;
const SPECIAL_OPCODE_MTHI: u8 = 0x11;
const SPECIAL_OPCODE_MFLO: u8 = 0x12;
const SPECIAL_OPCODE_MTLO: u8 = 0x13;

const SPECIAL_OPCODE_MULT: u8 = 0x18;
const SPECIAL_OPCODE_MULTU: u8 = 0x19;
const SPECIAL_OPCODE_DIV: u8 = 0x1a;
const SPECIAL_OPCODE_DIVU: u8 = 0x1b;

const SPECIAL_OPCODE_ADD: u8 = 0x20;
const SPECIAL_OPCODE_ADDU: u8 = 0x21;
const SPECIAL_OPCODE_SUB: u8 = 0x22;
const SPECIAL_OPCODE_SUBU: u8 = 0x23;
const SPECIAL_OPCODE_AND: u8 = 0x24;
const SPECIAL_OPCODE_OR: u8 = 0x25;
const SPECIAL_OPCODE_XOR: u8 = 0x26;
const SPECIAL_OPCODE_NOR: u8 = 0x27;

const SPECIAL_OPCODE_SLT: u8 = 0x2a;
const SPECIAL_OPCODE_SLTU: u8 = 0x2b;

const BCONDZ_OPCODE_BLTZ: u32 = 0x00;
const BCONDZ_OPCODE_BGEZ: u32 = 0x01;
const BCONDZ_OPCODE_BLTZAL: u32 = 0x10;
const BCONDZ_OPCODE_BGEZAL: u32 = 0x11;
// |20..16|
// | xxxx0| bltz  ;\undocumented dupes
// | xxxx1| bgez  ;/(when bit17-19=nonzero)

const COP_OPCODE_MFC: u32 = 0b00000_00000_000000_00000_00000_000000;
const COP_OPCODE_CFC: u32 = 0b00000_00010_000000_00000_00000_000000;
const COP_OPCODE_MTC: u32 = 0b00000_00100_000000_00000_00000_000000;
const COP_OPCODE_CTC: u32 = 0b00000_00110_000000_00000_00000_000000;

const COP_BC_OPCODE_BCF: u32 = 0b00000_01000_000000_00000_00000_000000;
const COP_BC_OPCODE_BCT: u32 = 0b00000_01000_000001_00000_00000_000000;

const COP0_OPCODE_TLBR: u32 = 0b00000_10000_000000_00000_00000_000001;
const COP0_OPCODE_TLBWI: u32 = 0b00000_10000_000000_00000_00000_000010;
const COP0_OPCODE_TLBWR: u32 = 0b00000_10000_000000_00000_00000_000110;
const COP0_OPCODE_TLBP: u32 = 0b00000_10000_000000_00000_00000_001000;
const COP0_OPCODE_RFE: u32 = 0b00000_10000_000000_00000_00000_010000;

fn dec_opcode(v: u32) u8 {
    return @intCast((v & OPCODE_MASK) >> OPCODE_SHIFT);
}

fn dec_special_opcode(v: u32) u8 {
    return @intCast((v & SPECIAL_OPCODE_MASK) >> SPECIAL_OPCODE_SHIFT);
}

fn dec_bcondz_opcode(v: u32) u8 {
    return @intCast((v & BCONDZ_OPCODE_MASK) >> BCONDZ_OPCODE_SHIFT);
}

fn dec_cop_opcode(v: u32) u32 {
    return (v & COP_OPCODE_MASK) >> COP_OPCODE_SHIFT;
}

fn dec_cop_bc_opcode(v: u32) u32 {
    return (v & COP_BC_OPCODE_MASK) >> COP_BC_OPCODE_SHIFT;
}

fn dec_rs(v: u32) u8 {
    return @intCast((v & RS_MASK) >> RS_SHIFT);
}

fn dec_rt(v: u32) u8 {
    return @intCast((v & RT_MASK) >> RT_SHIFT);
}

fn dec_rd(v: u32) u8 {
    return @intCast((v & RD_MASK) >> RD_SHIFT);
}

fn dec_imm5(v: u32) u8 {
    return @intCast((v & IMM5_MASK) >> IMM5_SHIFT);
}

fn dec_imm16(v: u32) i16 {
    return @intCast((v & IMM16_MASK) >> IMM16_SHIFT);
}

fn dec_imm26(v: u32) i32 {
    return @intCast((v & IMM26_MASK) >> IMM26_SHIFT);
}

fn r_type(op: Op, v: u32) Inst {
    return Inst.r_type(op, dec_rs(v), dec_rt(v), dec_rd(v), dec_imm5(v));
}

fn j_type(op: Op, v: u32) Inst {
    return Inst.j_type(op, dec_imm26(v));
}

fn i_type(op: Op, v: u32) Inst {
    return Inst.i_type(op, dec_rs(v), dec_rt(v), dec_imm16(v));
}

pub fn decode(v: u32) Inst {
    const inst = switch (dec_opcode(v)) {
        OPCODE_SPECIAL => switch (dec_special_opcode(v)) {
            SPECIAL_OPCODE_SLL => r_type(Op.SLL, v),
            SPECIAL_OPCODE_SRL => r_type(Op.SRL, v),
            SPECIAL_OPCODE_SRA => r_type(Op.SRA, v),
            SPECIAL_OPCODE_SLLV => r_type(Op.SLLV, v),
            SPECIAL_OPCODE_SRLV => r_type(Op.SRLV, v),
            SPECIAL_OPCODE_SRAV => r_type(Op.SRAV, v),
            SPECIAL_OPCODE_JR => r_type(Op.JR, v),
            SPECIAL_OPCODE_JALR => r_type(Op.JALR, v),
            SPECIAL_OPCODE_SYS => r_type(Op.SYS, v),
            SPECIAL_OPCODE_BRK => r_type(Op.BRK, v),
            SPECIAL_OPCODE_MFHI => r_type(Op.MFHI, v),
            SPECIAL_OPCODE_MTHI => r_type(Op.MTHI, v),
            SPECIAL_OPCODE_MFLO => r_type(Op.MFLO, v),
            SPECIAL_OPCODE_MTLO => r_type(Op.MTLO, v),
            SPECIAL_OPCODE_MULT => r_type(Op.MULT, v),
            SPECIAL_OPCODE_MULTU => r_type(Op.MULTU, v),
            SPECIAL_OPCODE_DIV => r_type(Op.DIV, v),
            SPECIAL_OPCODE_DIVU => r_type(Op.DIVU, v),
            SPECIAL_OPCODE_ADD => r_type(Op.ADD, v),
            SPECIAL_OPCODE_ADDU => r_type(Op.ADDU, v),
            SPECIAL_OPCODE_SUB => r_type(Op.SUB, v),
            SPECIAL_OPCODE_SUBU => r_type(Op.SUBU, v),
            SPECIAL_OPCODE_SLT => r_type(Op.SLT, v),
            SPECIAL_OPCODE_SLTU => r_type(Op.SLTU, v),
            SPECIAL_OPCODE_AND => r_type(Op.AND, v),
            SPECIAL_OPCODE_OR => r_type(Op.OR, v),
            SPECIAL_OPCODE_XOR => r_type(Op.XOR, v),
            SPECIAL_OPCODE_NOR => r_type(Op.NOR, v),
            else => @panic("TODO: unknown opcode"),
        },
        OPCODE_BcondZ => switch (dec_bcondz_opcode(v)) {
            BCONDZ_OPCODE_BLTZ => i_type(Op.BLTZ, v),
            BCONDZ_OPCODE_BGEZ => i_type(Op.BGEZ, v),
            BCONDZ_OPCODE_BLTZAL => i_type(Op.BLTZAL, v),
            BCONDZ_OPCODE_BGEZAL => i_type(Op.BGEZAL, v),
            else => @panic("TODO: unknown opcode"),
        },
        OPCODE_J => j_type(Op.J, v),
        OPCODE_JAL => j_type(Op.JAL, v),
        OPCODE_BEQ => i_type(Op.BEQ, v),
        OPCODE_BNE => i_type(Op.BNE, v),
        OPCODE_BLEZ => i_type(Op.BLEZ, v),
        OPCODE_BGTZ => i_type(Op.BGTZ, v),
        OPCODE_ADDI => i_type(Op.ADDI, v),
        OPCODE_ADDIU => i_type(Op.ADDIU, v),
        OPCODE_SLTI => i_type(Op.SLTI, v),
        OPCODE_SLTIU => i_type(Op.SLTIU, v),
        OPCODE_ANDI => i_type(Op.ANDI, v),
        OPCODE_ORI => i_type(Op.ORI, v),
        OPCODE_XORI => i_type(Op.XORI, v),
        OPCODE_LUI => i_type(Op.LUI, v),
        OPCODE_LB => i_type(Op.LB, v),
        OPCODE_LH => i_type(Op.LH, v),
        OPCODE_LWL => i_type(Op.LWL, v),
        OPCODE_LW => i_type(Op.LW, v),
        OPCODE_LBU => i_type(Op.LBU, v),
        OPCODE_LHU => i_type(Op.LHU, v),
        OPCODE_LWR => i_type(Op.LWR, v),
        OPCODE_SB => i_type(Op.SB, v),
        OPCODE_SH => i_type(Op.SH, v),
        OPCODE_SWL => i_type(Op.SWL, v),
        OPCODE_SW => i_type(Op.SW, v),
        OPCODE_SWR => i_type(Op.SWR, v),
        OPCODE_COP0 => switch (dec_cop_opcode(v)) {
            COP_OPCODE_MFC => r_type(Op.MFC0, v),
            COP_OPCODE_CFC => r_type(Op.CFC0, v),
            COP_OPCODE_MTC => r_type(Op.MTC0, v),
            COP_OPCODE_CTC => r_type(Op.CTC0, v),
            COP0_OPCODE_TLBR => r_type(Op.TLBR, v),
            COP0_OPCODE_TLBWI => r_type(Op.TLBWI, v),
            COP0_OPCODE_TLBWR => r_type(Op.TLBWR, v),
            COP0_OPCODE_TLBP => r_type(Op.TLBP, v),
            COP0_OPCODE_RFE => r_type(Op.RFE, v),
            else => switch (dec_cop_bc_opcode(v)) {
                COP_BC_OPCODE_BCF => i_type(Op.BC0F, v),
                COP_BC_OPCODE_BCT => i_type(Op.BC0T, v),
                else => @panic("TODO: unknown opcode"),
            },
        },
        OPCODE_COP2 => switch (dec_cop_opcode(v)) {
            COP_OPCODE_MFC => r_type(Op.MFC2, v),
            COP_OPCODE_CFC => r_type(Op.CFC2, v),
            COP_OPCODE_MTC => r_type(Op.MTC2, v),
            COP_OPCODE_CTC => r_type(Op.CTC2, v),
            else => switch (dec_cop_bc_opcode(v)) {
                COP_BC_OPCODE_BCF => i_type(Op.BC2F, v),
                COP_BC_OPCODE_BCT => i_type(Op.BC2T, v),
                else => @panic("TODO: unknown opcode"),
            },
        },
        OPCODE_LWC0 => i_type(Op.LWC0, v),
        OPCODE_LWC2 => i_type(Op.LWC2, v),
        OPCODE_SWC0 => i_type(Op.SWC0, v),
        OPCODE_SWC2 => i_type(Op.SWC2, v),
        else => @panic("TODO: unknown opcode"),
    };
    return inst;
}

const std = @import("std");
const testing = std.testing;

test "decode" {
    _ = decode(0);
}
