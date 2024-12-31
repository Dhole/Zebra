const std = @import("std");
const testing = std.testing;

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

const Op = enum {
    // Load and Store
    LB,
    LBU,
    LH,
    LHU,
    LW,
    LWL,
    LWR,
    SB,
    SH,
    SW,
    SWL,
    SWR,

    // ALU Immediate
    ADDI,
    ADDIU,
    SLTI,
    LSTIU,
    ANDI,
    ORI,
    XORI,
    LUI,

    // ALU Register
    ADD,
    ADDU,
    SUB,
    SUBU,
    SLT,
    SLTU,
    AND,
    OR,
    XOR,
    NOR,

    // Shift
    SLL,
    SRL,
    SRA,
    SLLV,
    SRLV,
    SRAV,

    // Multiply and Divide
    MULT,
    MULTU,
    DIV,
    DIVU,
    MFHI,
    MFLO,
    MTHI,
    MTLO,

    // Jump
    J,
    JAL,
    JR,
    JALR,

    // Branch
    BEQ,
    BNE,
    BLEZ,
    BGTZ,
    BLTZ,
    BGEZ,
    BLTZAL,
    BGEZAL,

    // Special
    SYS,
    BRK,

    // Co-processor
    LWC0,
    SWC0,
    MTC0,
    MFC0,
    CTC0,
    CFC0,
    COP0,

    LWC2,
    SWC2,
    MTC2,
    MFC2,
    CTC2,
    CFC2,
    COP2,

    // System control
    TLBR,
    TLBWI,
    TLBWR,
    TLBP,
    RFE,
};

const InstArgs = union {
    i_type: ITypeArgs,
    j_type: JTypeArgs,
    r_type: RTypeArgs,
};

const ITypeArgs = struct {
    rs: u8,
    rt: u8,
    imm: i16,
};

const JTypeArgs = struct {
    target: i32,
};

const RTypeArgs = struct {
    rs: u8,
    rt: u8,
    rd: u8,
    imm5: u8,
};

const Inst = struct {
    op: Op,
    args: InstArgs,

    const Self = @This();

    fn op(self: *Self) Op {
        return self.op;
    }
    fn i_type(_op: Op, rs: u8, rt: u8, imm: i16) Self {
        return Self{ .op = _op, .args = ITypeArgs{ .rs = rs, .rt = rt, .imm = imm } };
    }
    fn j_type(_op: Op, target: i32) Self {
        return Self{ .op = _op, .args = JTypeArgs{ .target = target } };
    }
    fn r_type(_op: Op, rs: u8, rt: u8, rd: u8, imm5: u8) Self {
        return Self{ .op = _op, .args = RTypeArgs{ .rs = rs, .rt = rt, .rd = rd, .imm5 = imm5 } };
    }
    fn get_i_type(self: *Self) ITypeArgs {
        return self.args.ITypeArgs;
    }
    fn get_j_type(self: *Self) JTypeArgs {
        return self.args.JTypeArgs;
    }
    fn get_r_type(self: *Self) RTypeArgs {
        return self.args.RTypeArgs;
    }
};

const OPCODE_MASK: u32 = 0b11111100_00000000_00000000_00000000;
const OPCODE_SHIFT = 26;
const RS_MASK: u32 = 0b00000011_11100000_00000000_00000000;
const RS_SHIFT = 21;
const RT_MASK: u32 = 0b00000000_00011111_00000000_00000000;
const RT_SHIFT = 16;
const RD_MASK: u32 = 0b00000000_00000000_11111000_00000000;
const RD_SHIFT = 11;
const IMM5_MASK: u32 = 0b00000000_00000000_00000111_11000000;
const IMM5_SHIFT = 6;
const IMM16_MASK: u32 = 0b00000000_00000000_11111111_11111111;
const IMM16_SHIFT = 0;
const IMM25_MASK: u32 = 0b00000001_11111111_11111111_11111111;
const IMM25_SHIFT = 0;
const IMM26_MASK: u32 = 0b00000001_11111111_11111111_11111111;
const IMM26_SHIFT = 0;
const COP_MASK: u32 = 0b00001100_00000000_00000000_00000000;
const COP_SHIFT = 26;
const SPECIAL_OPCODE_MASK: u32 = 0b00000000_00000000_00000000_00111111;
const SPECIAL_OPCODE_SHIFT = 0;

const Opcode = enum(u8) {
    SPECIAL = 0x00,
    BcondZ = 0x01,
    J = 0x02,
    JAL = 0x03,
    BEQ = 0x04,
    BNE = 0x05,
    BLEZ = 0x06,
    BGTZ = 0x07,

    ADDI = 0x08,
    ADDIU = 0x09,
    SLTI = 0x0a,
    SLTIU = 0x0b,
    ANDI = 0x0c,
    ORI = 0x0d,
    XORI = 0x0e,
    LUI = 0x0f,

    COP0 = 0x10,
    COP1 = 0x11,
    COP2 = 0x12,
    COP3 = 0x13,

    LB = 0x20,
    LH = 0x21,
    LWL = 0x22,
    LW = 0x23,
    LBU = 0x24,
    LHU = 0x25,
    LWR = 0x26,

    SB = 0x28,
    SH = 0x29,
    SWL = 0x2a,
    SW = 0x2b,
    SWR = 0x2e,

    LWC0 = 0x30,
    LWC1 = 0x31,
    LWC2 = 0x32,
    LWC3 = 0x33,

    SWC0 = 0x38,
    SWC1 = 0x39,
    SWC2 = 0x3a,
    SWC3 = 0x3b,
};

const SpecialOpcode = enum(u8) {
    SLL = 0x00,
    SRL = 0x02,
    SRA = 0x03,
    SLLV = 0x04,
    SRLV = 0x06,
    SRAV = 0x07,

    JR = 0x08,
    JALR = 0x09,
    SYS = 0x0c,
    BRK = 0x0d,

    MFHI = 0x10,
    MTHI = 0x11,
    MFLO = 0x12,
    MTLO = 0x13,

    MULT = 0x18,
    MULTU = 0x19,
    DIV = 0x1a,
    DIVU = 0x1b,

    ADD = 0x20,
    ADDU = 0x21,
    SUB = 0x22,
    SUBU = 0x23,
    AND = 0x24,
    OR = 0x25,
    XOR = 0x26,
    NOR = 0x27,

    SLT = 0x2a,
    SLTU = 0x2b,
};

fn dec_opcode(v: u32) u8 {
    return (v & OPCODE_MASK) >> OPCODE_SHIFT;
}

fn dec_special_opcode(v: u32) u8 {
    return (v & SPECIAL_OPCODE_MASK) >> SPECIAL_OPCODE_SHIFT;
}

fn dec_rs(v: u32) u8 {
    return (v & RS_MASK) >> RS_SHIFT;
}

fn dec_rt(v: u32) u8 {
    return (v & RT_MASK) >> RT_SHIFT;
}

fn dec_rd(v: u32) u8 {
    return (v & RD_MASK) >> RD_SHIFT;
}

fn dec_imm5(v: u32) i16 {
    return (v & IMM5_MASK) >> IMM5_SHIFT;
}

fn dec_imm16(v: u32) i16 {
    return (v & IMM16_MASK) >> IMM16_SHIFT;
}

fn dec_imm26(v: u32) u32 {
    return (v & IMM26_MASK) >> IMM26_SHIFT;
}

fn r_type(op: Op, v: u32) void {
    return Inst.r_type(op, dec_rs(v), dec_rt(v), dec_rd(v));
}

fn j_type(op: Op, v: u32) void {
    return Inst.j_type(op, dec_imm26(v));
}

fn i_type(op: Op, v: u32) void {
    return Inst.i_type(op, dec_rs(v), dec_rt(v), dec_imm16(v));
}

fn decode(v: u32) Inst {
    const inst = switch (dec_opcode(v)) {
        Opcode.SPECIAL => {
            switch (dec_special_opcode(v)) {
                SpecialOpcode.SLL => r_type(Op.SLL, v),
                SpecialOpcode.SRL => r_type(Op.SRL, v),
                SpecialOpcode.SRA => r_type(Op.SRA, v),
                SpecialOpcode.SLLV => r_type(Op.SLLV, v),
                SpecialOpcode.SRLV => r_type(Op.SRLV, v),
                SpecialOpcode.SRAV => r_type(Op.SRAV, v),

                SpecialOpcode.JR => r_type(Op.JR, v),
                SpecialOpcode.JALR => r_type(Op.JALR, v),
                SpecialOpcode.SYS => r_type(Op.SYS, v),
                SpecialOpcode.BRK => r_type(Op.BRK, v),

                SpecialOpcode.MFHI => r_type(Op.MFHI, v),
                SpecialOpcode.MTHI => r_type(Op.MTHI, v),
                SpecialOpcode.MFLO => r_type(Op.MFLO, v),
                SpecialOpcode.MTLO => r_type(Op.MTLO, v),

                SpecialOpcode.JALR => r_type(Op.JALR, v),

                SpecialOpcode.MULT => r_type(Op.MULT, v),
                SpecialOpcode.MULTU => r_type(Op.MULTU, v),
                SpecialOpcode.DIV => r_type(Op.DIV, v),
                SpecialOpcode.DIVU => r_type(Op.DIVU, v),

                SpecialOpcode.ADD => r_type(Op.ADD, v),
                SpecialOpcode.ADDU => r_type(Op.ADDU, v),
                SpecialOpcode.SUB => r_type(Op.SUB, v),
                SpecialOpcode.SUBU => r_type(Op.SUBU, v),
                SpecialOpcode.SLT => r_type(Op.SLT, v),
                SpecialOpcode.SLTU => r_type(Op.SLTU, v),
                SpecialOpcode.AND => r_type(Op.AND, v),
                SpecialOpcode.OR => r_type(Op.OR, v),
                SpecialOpcode.XOR => r_type(Op.XOR, v),
                SpecialOpcode.NOR => r_type(Op.NOR, v),

                SpecialOpcode.SLT => r_type(Op.SLT, v),
                SpecialOpcode.SLTU => r_type(Op.SLTU, v),
            }
        },
        Opcode.BcondZ => @panic("TODO"),
        Opcode.J => j_type(Op.J, v),
        Opcode.JAL => j_type(Op.JAL, v),
        Opcode.BEQ => i_type(Op.BEQ, v),
        Opcode.BNE => i_type(Op.BNE, v),
        else => @panic("TODO: unknown opcode"),
    };
    return inst;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}
