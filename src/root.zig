const std = @import("std");

pub const RegIdx = struct { u8 };

pub fn r(idx: u8) RegIdx {
    return RegIdx{idx};
}

pub const ITypeArgs = struct {
    rs: RegIdx,
    rt: RegIdx,
    imm: i16,
};

pub const JTypeArgs = struct {
    imm: u32,
};

pub const RTypeArgs = struct {
    rs: RegIdx,
    rt: RegIdx,
    rd: RegIdx,
    imm: u8,
};

pub const Inst = union(enum) {
    // Load and Store
    lb: ITypeArgs,
    lbu: ITypeArgs,
    lh: ITypeArgs,
    lhu: ITypeArgs,
    lw: ITypeArgs,
    lwl: ITypeArgs,
    lwr: ITypeArgs,
    sb: ITypeArgs,
    sh: ITypeArgs,
    sw: ITypeArgs,
    swl: ITypeArgs,
    swr: ITypeArgs,

    // ALU Immediate
    addi: ITypeArgs,
    addiu: ITypeArgs,
    slti: ITypeArgs,
    sltiu: ITypeArgs,
    andi: ITypeArgs,
    ori: ITypeArgs,
    xori: ITypeArgs,
    lui: ITypeArgs,

    // ALU Register
    add: RTypeArgs,
    addu: RTypeArgs,
    sub: RTypeArgs,
    subu: RTypeArgs,
    slt: RTypeArgs,
    sltu: RTypeArgs,
    @"and": RTypeArgs,
    @"or": RTypeArgs,
    xor: RTypeArgs,
    nor: RTypeArgs,

    // Shift
    sll: RTypeArgs,
    srl: RTypeArgs,
    sra: RTypeArgs,
    sllv: RTypeArgs,
    srlv: RTypeArgs,
    srav: RTypeArgs,

    // Multiply and Divide
    mult: RTypeArgs,
    multu: RTypeArgs,
    div: RTypeArgs,
    divu: RTypeArgs,
    mfhi: RTypeArgs,
    mflo: RTypeArgs,
    mthi: RTypeArgs,
    mtlo: RTypeArgs,

    // Jump
    j: JTypeArgs,
    jal: JTypeArgs,
    jr: RTypeArgs,
    jalr: RTypeArgs,

    // Branch
    beq: ITypeArgs,
    bne: ITypeArgs,
    blez: ITypeArgs,
    bgtz: ITypeArgs,
    bltz: ITypeArgs,
    bgez: ITypeArgs,
    bltzal: ITypeArgs,
    bgezal: ITypeArgs,

    // Special
    sys: RTypeArgs,
    brk: RTypeArgs,

    // Co-processor
    lwc0: ITypeArgs,
    swc0: ITypeArgs,
    mtc0: RTypeArgs,
    mfc0: RTypeArgs,
    // ctc0: RTypeArgs,
    // cfc0: RTypeArgs,
    // bc0f: ITypeArgs,
    // bc0t: ITypeArgs,

    lwc2: ITypeArgs,
    swc2: ITypeArgs,
    mtc2: RTypeArgs,
    mfc2: RTypeArgs,
    ctc2: RTypeArgs,
    cfc2: RTypeArgs,
    // bc2f: ITypeArgs,
    // bc2t: ITypeArgs,

    // Unsupporeted by PSX
    cop1: JTypeArgs,
    cop3: JTypeArgs,

    // System control
    // tlbr: RTypeArgs,
    // tlbwi: RTypeArgs,
    // tlbwr: RTypeArgs,
    // tlbp: RTypeArgs,
    rfe: RTypeArgs,
};
