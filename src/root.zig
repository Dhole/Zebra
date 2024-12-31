const std = @import("std");

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
    SLTIU,
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
    BC0F,
    BC0T,

    LWC2,
    SWC2,
    MTC2,
    MFC2,
    CTC2,
    CFC2,
    COP2,
    BC2F,
    BC2T,

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
        const args = ITypeArgs{ .rs = rs, .rt = rt, .imm = imm };
        return Self{ .op = _op, .args = InstArgs{ .i_type = args } };
    }
    fn j_type(_op: Op, target: i32) Self {
        const args = JTypeArgs{ .target = target };
        return Self{ .op = _op, .args = InstArgs{ .j_type = args } };
    }
    fn r_type(_op: Op, rs: u8, rt: u8, rd: u8, imm5: u8) Self {
        const args = RTypeArgs{ .rs = rs, .rt = rt, .rd = rd, .imm5 = imm5 };
        return Self{ .op = _op, .args = InstArgs{ .r_type = args } };
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
