use MicroOp::*;
use RegisterName8::*;
use RegisterName16::*;
use ArithOperand::*;

pub type InstructionOps = [MicroOp; 8];

pub type MemIndex = u16;

pub const INSTRUCTION_MICRO_OPS: [InstructionOps; 256] = [
    [Nop, End, Nop, Nop, Nop, Nop, Nop, Nop], 
    [Nop, ReadImmR8(C), ReadImmR8(B), End, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(BC, A), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, IncR16(BC), End, Nop, Nop, Nop, Nop, Nop],
    [IncR8(B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [DecR8(B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(B), End, Nop, Nop, Nop, Nop, Nop],
    [Rlc(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Nop, Write8IndImm(SPLower), Write8IndImm(SPHigher), End, Nop, Nop],
    [Nop, AddR16R16(HL, BC), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(A, BC), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, DecR16(BC), End, Nop, Nop, Nop, Nop, Nop],
    [IncR8(C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [DecR8(C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(C), End, Nop, Nop, Nop, Nop, Nop],
    [Rrc(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Stop, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(E), ReadImmR8(B), End, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(DE, A), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, IncR16(DE), End, Nop, Nop, Nop, Nop, Nop],
    [IncR8(D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [DecR8(D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(D), End, Nop, Nop, Nop, Nop, Nop],
    [Rl(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, JmpRelativeI8, End, Nop, Nop, Nop, Nop, Nop],
    [Nop, AddR16R16(HL, DE), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(A, DE), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, DecR16(DE), End, Nop, Nop, Nop, Nop, Nop],
    [IncR8(E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [DecR8(E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(E), End, Nop, Nop, Nop, Nop, Nop],
    [Rr(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, CheckFlagNotI8(Flag::Zero), JmpRelativeI8, End, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(L), ReadImmR8(H), End, Nop, Nop, Nop, Nop],
    [Nop, ReadAHLPlus, End, Nop, Nop, Nop, Nop, Nop],
    [Nop, IncR16(HL), End, Nop, Nop, Nop, Nop, Nop],
    [IncR8(H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [DecR8(H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(H), End, Nop, Nop, Nop, Nop, Nop],
    [Daa, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, CheckFlagI8(Flag::Zero), JmpRelativeI8, End, Nop, Nop, Nop, Nop],
    [Nop, AddR16R16(HL, DE), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, WriteHLPlusA, End, Nop, Nop, Nop, Nop, Nop],
    [Nop, DecR16(HL), End, Nop, Nop, Nop, Nop, Nop],
    [IncR8(L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [DecR8(L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(L), End, Nop, Nop, Nop, Nop, Nop],
    [Cpl, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, CheckFlagNotI8(Flag::Carry), JmpRelativeI8, End, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(SPLower), ReadImmR8(SPHigher), End, Nop, Nop, Nop, Nop],
    [Nop, ReadAHLMinus, End, Nop, Nop, Nop, Nop, Nop],
    [Nop, IncR16(SP), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, IncIndR16(HL), End, Nop, Nop, Nop, Nop],
    [Nop, Nop, DecIndR16(HL), End, Nop, Nop, Nop, Nop],
    [Scf, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, CheckFlagI8(Flag::Carry), JmpRelativeI8, End, Nop, Nop, Nop, Nop],
    [Nop, AddR16R16(HL, SP), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, WriteHLMinusA, End, Nop, Nop, Nop, Nop, Nop],
    [Nop, DecR16(SP), End, Nop, Nop, Nop, Nop, Nop],
    [IncR8(A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [DecR8(A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadImmR8(A), End, Nop, Nop, Nop, Nop, Nop],
    [Ccf, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(B,B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(B,C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(B,D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(B,E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(B,H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(B,L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(B,HL), End, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(B,A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(C,B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(C,C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(C,D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(C,E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(C,H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(C,L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(C,HL), End, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(C,A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(D,B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(D,C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(D,D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(D,E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(D,H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(D,L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(D,HL), End, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(D,A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(E,B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(E,C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(E,D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(E,E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(E,H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(E,L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(E,HL), End, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(E,A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(H,B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(H,C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(H,D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(H,E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(H,H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(H,L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(H,HL), End, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(H,A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(L,B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(L,C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(L,D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(L,E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(L,H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(L,L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(L,HL), End, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(L,A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(HL, B), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(HL, C), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(HL, D), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(HL, E), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(HL, H), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(HL, L), End, Nop, Nop, Nop, Nop, Nop],
    [Halt, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Write8Ind16(HL, A), End, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(A,B), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(A,C), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(A,D), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(A,E), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(A,H), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(A,L), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, ReadIndR8(A,HL), End, Nop, Nop, Nop, Nop, Nop],
    [ReadR8R8(A,A), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AddA(Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AddA(Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AddA(Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AddA(Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AddA(Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AddA(Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, AddA(HLIndirect), End, Nop, Nop, Nop, Nop, Nop],
    [AddA(Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AdcA(Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AdcA(Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AdcA(Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AdcA(Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AdcA(Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AdcA(Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, AdcA(HLIndirect), End, Nop, Nop, Nop, Nop, Nop],
    [AdcA(Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SubA(Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SubA(Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SubA(Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SubA(Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SubA(Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SubA(Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, SubA(HLIndirect), End, Nop, Nop, Nop, Nop, Nop],
    [SubA(Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SbcA(Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SbcA(Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SbcA(Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SbcA(Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SbcA(Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [SbcA(Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, SbcA(HLIndirect), End, Nop, Nop, Nop, Nop, Nop],
    [SbcA(Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AndA(Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AndA(Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AndA(Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AndA(Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AndA(Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [AndA(Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, AndA(HLIndirect), End, Nop, Nop, Nop, Nop, Nop],
    [AndA(Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [XorA(Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [XorA(Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [XorA(Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [XorA(Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [XorA(Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [XorA(Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, XorA(HLIndirect), End, Nop, Nop, Nop, Nop, Nop],
    [XorA(Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [OrA(Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [OrA(Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [OrA(Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [OrA(Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [OrA(Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [OrA(Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, OrA(HLIndirect), End, Nop, Nop, Nop, Nop, Nop],
    [OrA(Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [CpA(Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [CpA(Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [CpA(Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [CpA(Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [CpA(Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [CpA(Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, CpA(HLIndirect), End, Nop, Nop, Nop, Nop, Nop],
    [CpA(Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, CheckFlagNot(Flag::Zero), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, PopR8(C), PopR8(B), End, Nop, Nop, Nop, Nop],
    [Nop, Nop, CheckFlagNotU16(Flag::Zero), JmpU16, End, Nop, Nop, Nop],
    [Nop, Nop, Nop, JmpU16, End, Nop, Nop, Nop],
    [Nop, Nop, CheckFlagNotU16(Flag::Zero), Nop, PushR8(PCHigher), PushR8CallU16(PCLower), End, Nop],
    [Nop, Nop, PushR8(B), PushR8(C), End, Nop, Nop, Nop],
    [Nop, AddA(Immediate), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(PCHigher), PushR8CallConst(PCLower, 0), End, Nop, Nop, Nop],
    [Nop, CheckFlag(Flag::Zero), PopR8(PCLower), PopR8(PCHigher), Nop, End, Nop, Nop],
    [Nop, PopR8(PCLower), PopR8(PCHigher), Nop, End, Nop, Nop, Nop],
    [Nop, Nop, CheckFlagU16(Flag::Zero), JmpU16, End, Nop, Nop, Nop],
    [FetchExtended, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, CheckFlagU16(Flag::Zero), Nop, PushR8(PCHigher), PushR8CallU16(PCLower), End, Nop],
    [Nop, Nop, Nop, PushR8(PCHigher), Nop, PushR8CallU16(PCLower), End, Nop],
    [Nop, AdcA(Immediate), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(PCHigher), PushR8CallConst(PCLower, 0x08), End, Nop, Nop, Nop],
    [Nop, CheckFlagNot(Flag::Carry), PopR8(PCLower), PopR8(PCHigher), Nop, End, Nop, Nop],
    [Nop, PopR8(E), PopR8(D), End, Nop, Nop, Nop, Nop],
    [Nop, Nop, CheckFlagNotU16(Flag::Carry), JmpU16, End, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, CheckFlagNotU16(Flag::Carry), Nop, PushR8(PCHigher), PushR8CallU16(PCLower), End, Nop],
    [Nop, Nop, PushR8(D), PushR8(E), End, Nop, Nop, Nop],
    [Nop, SubA(Immediate), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(PCHigher), PushR8CallConst(PCLower, 0x10), End, Nop, Nop, Nop],
    [Nop, CheckFlag(Flag::Carry), PopR8(PCLower), PopR8(PCHigher), Nop, End, Nop, Nop],
    [Nop, PopR8(PCLower), PopR8(PCHigher), Ei, End, Nop, Nop, Nop],
    [Nop, Nop, CheckFlagU16(Flag::Carry), JmpU16, End, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, CheckFlagU16(Flag::Carry), Nop, PushR8(PCHigher), PushR8CallU16(PCLower), End, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, SbcA(Immediate), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(PCHigher), PushR8CallConst(PCLower, 0x18), End, Nop, Nop, Nop],
    [Nop, Nop, WriteAIndImm(ImmType::HighImm), End, Nop, Nop, Nop, Nop],
    [Nop, PopR8(L), PopR8(H), End, Nop, Nop, Nop, Nop],
    [Nop, WriteAIndImm(ImmType::HighImmPlusC), End, Nop, Nop, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(H), PushR8(L), End, Nop, Nop, Nop],
    [Nop, AndA(Immediate), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(PCHigher), PushR8CallConst(PCLower, 0x20), End, Nop, Nop, Nop],
    [Nop, AddSPI8, End, Nop, Nop, Nop, Nop, Nop],
    [JmpHL, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Nop, WriteAIndImm(ImmType::DefaultImm), End, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, XorA(Immediate), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(PCHigher), PushR8CallConst(PCLower, 0x28), End, Nop, Nop, Nop],
    [Nop, Nop, ReadIndImmA(ImmType::HighImm), End, Nop, Nop, Nop, Nop],
    [Nop, PopR8(F), PopR8(A), End, Nop, Nop, Nop, Nop],
    [Nop, Nop, ReadIndImmA(ImmType::HighImmPlusC), End, Nop, Nop, Nop, Nop],
    [Di, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(A), PushR8(F), End, Nop, Nop, Nop],
    [Nop, OrA(Immediate), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(PCHigher), PushR8CallConst(PCLower, 0x30), End, Nop, Nop, Nop],
    [Nop, Nop, WriteSPI8HL, End, Nop, Nop, Nop, Nop],
    [Nop, ReadR16R16(SP, HL), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, ReadIndImmA(ImmType::DefaultImm), End, Nop, Nop, Nop, Nop],
    [Ei, End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Halt, Nop, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, CpA(Immediate), End, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, PushR8(PCHigher), PushR8CallConst(PCLower, 0x38), End, Nop, Nop, Nop],
];

pub const EXTENDED_INSTRUCTION_OPS: [InstructionOps; 256] = [
    [Rlc(ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rlc(ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rlc(ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rlc(ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rlc(ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rlc(ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Rlc(ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Rlc(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rrc(ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rrc(ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rrc(ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rrc(ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rrc(ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rrc(ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Rrc(ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Rrc(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rl(ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rl(ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rl(ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rl(ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rl(ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rl(ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Rl(ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Rl(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rr(ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rr(ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rr(ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rr(ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rr(ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Rr(ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Rr(ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Rr(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sla(ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sla(ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sla(ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sla(ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sla(ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sla(ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Sla(ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Sla(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sra(ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sra(ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sra(ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sra(ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sra(ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Sra(ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Sra(ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Sra(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Swap(ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Swap(ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Swap(ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Swap(ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Swap(ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Swap(ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Swap(ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Swap(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Srl(ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Srl(ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Srl(ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Srl(ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Srl(ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Srl(ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Srl(ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Srl(ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(0, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(0, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(0, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(0, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(0, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(0, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Bit(0, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Bit(0, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(1, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(1, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(1, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(1, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(1, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(1, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Bit(1, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Bit(1, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(2, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(2, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(2, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(2, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(2, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(2, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Bit(2, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Bit(2, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(3, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(3, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(3, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(3, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(3, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(3, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Bit(3, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Bit(3, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(4, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(4, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(4, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(4, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(4, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(4, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Bit(4, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Bit(4, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(5, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(5, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(5, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(5, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(5, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(5, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Bit(5, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Bit(5, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(6, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(6, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(6, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(6, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(6, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(6, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Bit(6, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Bit(6, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(7, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(7, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(7, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(7, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(7, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Bit(7, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Bit(7, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Bit(7, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(0, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(0, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(0, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(0, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(0, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(0, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Res(0, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Res(0, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(1, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(1, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(1, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(1, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(1, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(1, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Res(1, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Res(1, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(2, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(2, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(2, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(2, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(2, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(2, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Res(2, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Res(2, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(3, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(3, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(3, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(3, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(3, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(3, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Res(3, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Res(3, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(4, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(4, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(4, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(4, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(4, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(4, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Res(4, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Res(4, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(5, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(5, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(5, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(5, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(5, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(5, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Res(5, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Res(5, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(6, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(6, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(6, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(6, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(6, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(6, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Res(6, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Res(6, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(7, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(7, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(7, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(7, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(7, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Res(7, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Res(7, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Res(7, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(0, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(0, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(0, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(0, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(0, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(0, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Set(0, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Set(0, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(1, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(1, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(1, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(1, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(1, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(1, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Set(1, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Set(1, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(2, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(2, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(2, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(2, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(2, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(2, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Set(2, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Set(2, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(3, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(3, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(3, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(3, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(3, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(3, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Set(3, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Set(3, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(4, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(4, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(4, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(4, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(4, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(4, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Set(4, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Set(4, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(5, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(5, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(5, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(5, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(5, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(5, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Set(5, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Set(5, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(6, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(6, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(6, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(6, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(6, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(6, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Set(6, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Set(6, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(7, ExtOperand::Reg(B)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(7, ExtOperand::Reg(C)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(7, ExtOperand::Reg(D)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(7, ExtOperand::Reg(E)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(7, ExtOperand::Reg(H)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Set(7, ExtOperand::Reg(L)), End, Nop, Nop, Nop, Nop, Nop, Nop],
    [Nop, Nop, Set(7, ExtOperand::HLIndirect), End, Nop, Nop, Nop, Nop],
    [Set(7, ExtOperand::Reg(A)), End, Nop, Nop, Nop, Nop, Nop, Nop],
];

#[derive(Clone, Copy)]
pub enum MicroOp {
    Nop,
    Daa,
    Stop,
    Halt,
    Ei,
    End,
    Di,
    Cpl,
    Scf,
    Ccf,
    Rl(ExtOperand),
    Rlc(ExtOperand),
    Rr(ExtOperand),
    Rrc(ExtOperand),
    Sla(ExtOperand),
    Sra(ExtOperand),
    Swap(ExtOperand),
    Srl(ExtOperand),
    Bit(u8, ExtOperand),
    Res(u8, ExtOperand),
    Set(u8, ExtOperand),
    FetchExtended,
    ReadAHLPlus,
    ReadAHLMinus,
    ReadImmR8(RegisterName8),
    ReadIndR8(RegisterName8, RegisterName16),
    ReadIndImmA(ImmType),
    ReadAIndImm(ImmType),
    ReadR8R8(RegisterName8, RegisterName8),
    ReadR16R16(RegisterName16, RegisterName16),
    WriteHLPlusA,
    WriteHLMinusA,
    WriteSPI8HL,
    Write8(MemIndex, RegisterName8),
    Write8Ind16(RegisterName16, RegisterName8),
    Write8IndImm(RegisterName8),
    WriteIndImmA(ImmType),
    WriteAIndImm(ImmType),
    ReadMem8(RegisterName8, u16),
    AddR16R16(RegisterName16, RegisterName16),
    AddSPI8,
    AddA(ArithOperand),
    AdcA(ArithOperand),
    SubA(ArithOperand),
    SbcA(ArithOperand),
    AndA(ArithOperand),
    XorA(ArithOperand),
    OrA(ArithOperand),
    CpA(ArithOperand),
    IncR16(RegisterName16),
    IncIndR16(RegisterName16),
    DecIndR16(RegisterName16),
    DecR16(RegisterName16),
    IncR8(RegisterName8),
    DecR8(RegisterName8),
    PopR8(RegisterName8),
    PushR8(RegisterName8),
    PushR8CallU16(RegisterName8),
    PushR8CallConst(RegisterName8, u16),
    JmpRelativeI8,
    JmpU16,
    JmpHL,
    CheckFlag(Flag),
    CheckFlagNot(Flag),
    CheckFlagNotI8(Flag),
    CheckFlagI8(Flag),
    CheckFlagNotU16(Flag),
    CheckFlagU16(Flag),
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum RegisterName16 {
    AF = 0,
    BC = 1,
    DE = 2,
    HL = 3,
    SP = 4,
    PC = 5,
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum RegisterName8 {
    A = 0,
    F = 1,
    B = 2,
    C = 3,
    D = 4,
    E = 5,
    H = 6,
    L = 7,
    SPLower = 8,
    SPHigher = 9,
    PCLower = 10,
    PCHigher = 11,
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum Flag {
    Zero = 7,
    Subtraction = 6,
    HalfCarry = 5,
    Carry = 4,
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum ArithOperand {
    Reg(RegisterName8),
    HLIndirect,
    Immediate,
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum ExtOperand {
    Reg(RegisterName8),
    HLIndirect
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum ImmType {
    HighImm,
    HighImmPlusC,
    DefaultImm,
}