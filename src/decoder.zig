const root = @import("root.zig");
const Op = root.Op;
const Inst = root.Inst;
const ITypeArgs = root.ITypeArgs;
const RTypeArgs = root.RTypeArgs;
const JTypeArgs = root.JTypeArgs;

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

fn rs(v: u32) u8 {
    return @intCast((v & RS_MASK) >> RS_SHIFT);
}

fn rt(v: u32) u8 {
    return @intCast((v & RT_MASK) >> RT_SHIFT);
}

fn rd(v: u32) u8 {
    return @intCast((v & RD_MASK) >> RD_SHIFT);
}

fn imm5(v: u32) u8 {
    return @intCast((v & IMM5_MASK) >> IMM5_SHIFT);
}

fn imm16(v: u32) i16 {
    return @intCast((v & IMM16_MASK) >> IMM16_SHIFT);
}

fn imm26(v: u32) u32 {
    return (v & IMM26_MASK) >> IMM26_SHIFT;
}

fn r_type(v: u32) RTypeArgs {
    return .{ .rs = rs(v), .rt = rt(v), .rd = rd(v), .imm5 = imm5(v) };
}

fn j_type(v: u32) JTypeArgs {
    return .{ .offset = imm26(v) };
}

fn i_type(v: u32) ITypeArgs {
    return .{ .rs = rs(v), .rt = rt(v), .imm = imm16(v) };
}

pub fn decode(v: u32) Inst {
    const inst = switch (dec_opcode(v)) {
        OPCODE_SPECIAL => switch (dec_special_opcode(v)) {
            SPECIAL_OPCODE_SLL => Inst{ .sll = r_type(v) },
            SPECIAL_OPCODE_SRL => Inst{ .srl = r_type(v) },
            SPECIAL_OPCODE_SRA => Inst{ .sra = r_type(v) },
            SPECIAL_OPCODE_SLLV => Inst{ .sllv = r_type(v) },
            SPECIAL_OPCODE_SRLV => Inst{ .srlv = r_type(v) },
            SPECIAL_OPCODE_SRAV => Inst{ .srav = r_type(v) },
            SPECIAL_OPCODE_JR => Inst{ .jr = r_type(v) },
            SPECIAL_OPCODE_JALR => Inst{ .jalr = r_type(v) },
            SPECIAL_OPCODE_SYS => Inst{ .sys = r_type(v) },
            SPECIAL_OPCODE_BRK => Inst{ .brk = r_type(v) },
            SPECIAL_OPCODE_MFHI => Inst{ .mfhi = r_type(v) },
            SPECIAL_OPCODE_MTHI => Inst{ .mthi = r_type(v) },
            SPECIAL_OPCODE_MFLO => Inst{ .mflo = r_type(v) },
            SPECIAL_OPCODE_MTLO => Inst{ .mtlo = r_type(v) },
            SPECIAL_OPCODE_MULT => Inst{ .mult = r_type(v) },
            SPECIAL_OPCODE_MULTU => Inst{ .multu = r_type(v) },
            SPECIAL_OPCODE_DIV => Inst{ .div = r_type(v) },
            SPECIAL_OPCODE_DIVU => Inst{ .divu = r_type(v) },
            SPECIAL_OPCODE_ADD => Inst{ .add = r_type(v) },
            SPECIAL_OPCODE_ADDU => Inst{ .addu = r_type(v) },
            SPECIAL_OPCODE_SUB => Inst{ .sub = r_type(v) },
            SPECIAL_OPCODE_SUBU => Inst{ .subu = r_type(v) },
            SPECIAL_OPCODE_SLT => Inst{ .slt = r_type(v) },
            SPECIAL_OPCODE_SLTU => Inst{ .sltu = r_type(v) },
            SPECIAL_OPCODE_AND => Inst{ .@"and" = r_type(v) },
            SPECIAL_OPCODE_OR => Inst{ .@"or" = r_type(v) },
            SPECIAL_OPCODE_XOR => Inst{ .xor = r_type(v) },
            SPECIAL_OPCODE_NOR => Inst{ .nor = r_type(v) },
            else => @panic("TODO: unknown opcode"),
        },
        OPCODE_BcondZ => switch (dec_bcondz_opcode(v)) {
            BCONDZ_OPCODE_BLTZ => Inst{ .bltz = i_type(v) },
            BCONDZ_OPCODE_BGEZ => Inst{ .bgez = i_type(v) },
            BCONDZ_OPCODE_BLTZAL => Inst{ .bltzal = i_type(v) },
            BCONDZ_OPCODE_BGEZAL => Inst{ .bgezal = i_type(v) },
            else => @panic("TODO: unknown opcode"),
        },
        OPCODE_J => Inst{ .j = j_type(v) },
        OPCODE_JAL => Inst{ .jal = j_type(v) },
        OPCODE_BEQ => Inst{ .beq = i_type(v) },
        OPCODE_BNE => Inst{ .bne = i_type(v) },
        OPCODE_BLEZ => Inst{ .blez = i_type(v) },
        OPCODE_BGTZ => Inst{ .bgtz = i_type(v) },
        OPCODE_ADDI => Inst{ .addi = i_type(v) },
        OPCODE_ADDIU => Inst{ .addiu = i_type(v) },
        OPCODE_SLTI => Inst{ .slti = i_type(v) },
        OPCODE_SLTIU => Inst{ .sltiu = i_type(v) },
        OPCODE_ANDI => Inst{ .andi = i_type(v) },
        OPCODE_ORI => Inst{ .ori = i_type(v) },
        OPCODE_XORI => Inst{ .xori = i_type(v) },
        OPCODE_LUI => Inst{ .lui = i_type(v) },
        OPCODE_LB => Inst{ .lb = i_type(v) },
        OPCODE_LH => Inst{ .lh = i_type(v) },
        OPCODE_LWL => Inst{ .lwl = i_type(v) },
        OPCODE_LW => Inst{ .lw = i_type(v) },
        OPCODE_LBU => Inst{ .lbu = i_type(v) },
        OPCODE_LHU => Inst{ .lhu = i_type(v) },
        OPCODE_LWR => Inst{ .lwr = i_type(v) },
        OPCODE_SB => Inst{ .sb = i_type(v) },
        OPCODE_SH => Inst{ .sh = i_type(v) },
        OPCODE_SWL => Inst{ .swl = i_type(v) },
        OPCODE_SW => Inst{ .sw = i_type(v) },
        OPCODE_SWR => Inst{ .swr = i_type(v) },
        OPCODE_COP0 => switch (dec_cop_opcode(v)) {
            COP_OPCODE_MFC => Inst{ .mfc0 = r_type(v) },
            COP_OPCODE_CFC => Inst{ .cfc0 = r_type(v) },
            COP_OPCODE_MTC => Inst{ .mtc0 = r_type(v) },
            COP_OPCODE_CTC => Inst{ .ctc0 = r_type(v) },
            COP0_OPCODE_TLBR => Inst{ .tlbr = r_type(v) },
            COP0_OPCODE_TLBWI => Inst{ .tlbwi = r_type(v) },
            COP0_OPCODE_TLBWR => Inst{ .tlbwr = r_type(v) },
            COP0_OPCODE_TLBP => Inst{ .tlbp = r_type(v) },
            COP0_OPCODE_RFE => Inst{ .rfe = r_type(v) },
            else => switch (dec_cop_bc_opcode(v)) {
                COP_BC_OPCODE_BCF => Inst{ .bc0f = i_type(v) },
                COP_BC_OPCODE_BCT => Inst{ .bc0t = i_type(v) },
                else => @panic("TODO: unknown opcode"),
            },
        },
        OPCODE_COP2 => switch (dec_cop_opcode(v)) {
            COP_OPCODE_MFC => Inst{ .mfc0 = r_type(v) },
            COP_OPCODE_CFC => Inst{ .cfc0 = r_type(v) },
            COP_OPCODE_MTC => Inst{ .mtc0 = r_type(v) },
            COP_OPCODE_CTC => Inst{ .ctc0 = r_type(v) },
            else => switch (dec_cop_bc_opcode(v)) {
                COP_BC_OPCODE_BCF => Inst{ .bc0f = i_type(v) },
                COP_BC_OPCODE_BCT => Inst{ .bc0t = i_type(v) },
                else => @panic("TODO: unknown opcode"),
            },
        },
        OPCODE_LWC0 => Inst{ .lwc0 = i_type(v) },
        OPCODE_LWC2 => Inst{ .lwc2 = i_type(v) },
        OPCODE_SWC0 => Inst{ .swc0 = i_type(v) },
        OPCODE_SWC2 => Inst{ .swc2 = i_type(v) },
        else => @panic("TODO: unknown opcode"),
    };
    return inst;
}

const std = @import("std");
const testing = std.testing;

test "decode" {
    _ = decode(0);
}
