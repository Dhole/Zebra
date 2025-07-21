const std = @import("std");

pub const RegIdx = struct { u5 };

pub fn r(idx: u5) RegIdx {
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

pub fn buf_read(comptime T: type, buf: []const u8, addr: u32) T {
    return switch (T) {
        u8 => buf[addr],
        u16 => @as(u16, buf[addr]) + @as(u16, buf[addr + 1]) * 0x100,
        u32 => @as(u32, buf[addr]) + @as(u32, buf[addr + 1]) * 0x100 +
            @as(u32, buf[addr + 2]) * 0x10000 + @as(u32, buf[addr + 3]) * 0x1000000,
        else => @compileError("invalid T"),
    };
}

pub fn buf_write(comptime T: type, buf: []u8, addr: u32, v: T) void {
    switch (T) {
        u8 => {
            buf[addr] = @intCast(v);
        },
        u16 => {
            buf[addr + 0] = @intCast((v & 0x00ff) >> 0);
            buf[addr + 1] = @intCast((v & 0xff00) >> 8);
        },
        u32 => {
            buf[addr + 0] = @intCast((v & 0x000000ff) >> 0);
            buf[addr + 1] = @intCast((v & 0x0000ff00) >> 8);
            buf[addr + 2] = @intCast((v & 0x00ff0000) >> 16);
            buf[addr + 3] = @intCast((v & 0xff000000) >> 24);
        },
        else => @compileError("invalid T"),
    }
}
