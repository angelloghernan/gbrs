use std::ops::Index;
use std::ops::IndexMut;

use crate::micro_ops::MicroOp;
use crate::micro_ops::RegisterName8;
use crate::micro_ops::RegisterName16;
use crate::micro_ops::InstructionOps;
use crate::micro_ops::Flag;
use crate::micro_ops::INSTRUCTION_MICRO_OPS;
use crate::micro_ops::EXTENDED_INSTRUCTION_OPS;
use crate::micro_ops::ImmType;
use crate::micro_ops::ExtOperand;
use crate::micro_ops::ArithOperand;

#[derive (Debug)]
pub struct Cpu {
    pub registers: Registers,
    pub memory: Memory,
    pub instr_complete: bool,
    pub cur_micro_ops: &'static InstructionOps,
    pub micro_op_index: u8,
    pub ime: bool,
    pub fetched_value: u8,
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            registers: Registers::new(),
            memory: Memory::new(),
            instr_complete: false,
            cur_micro_ops: &INSTRUCTION_MICRO_OPS[0],
            micro_op_index: 0,
            fetched_value: 0,
            ime: false,
        }
    }

    pub fn fetch(&mut self) {
        let pc = self.registers.get_mut_16(RegisterName16::PC);

        let byte = self.memory[*pc];

        *pc += 1;

        self.fetched_value = byte;
        self.instr_complete = false;
    }

    pub fn decode(&mut self) {
        self.cur_micro_ops = &INSTRUCTION_MICRO_OPS[self.fetched_value as usize];
    }

    pub fn step(&mut self) {
        use MicroOp::*;
        use RegisterName8::*;
        use RegisterName16::*;

        let op = self.cur_micro_ops[self.micro_op_index as usize];
        self.micro_op_index += 1;
        match op {
            Nop => {},
            Daa => self.daa(),
            Ei => { self.ime = true }
            Di => { self.ime = false }
            Cpl => { 
                let a = self.registers.get_8(A);
                self.registers.set_8(A, !a);
            }
            Scf => {
                self.registers.set_flag(Flag::Carry);
            }
            Ccf => {
                let c = self.registers.check_flag(Flag::Carry);
                self.registers.assign_flag(Flag::Carry, !c);
            }
            Rl(op) => self.rl(op, true),
            Rlc(op) => self.rl(op, false),
            Rr(op) => self.rr(op, true),
            Rrc(op) => self.rr(op, false),
            Sla(op) => self.sla(op),
            Sra(op) => self.sra(op),
            Srl(op) => self.srl(op),
            Swap(op) => self.swap(op),
            Bit(b, op) => self.bit(b, op),
            Res(b, op) => self.res(b, op),
            Set(b, op) => self.set(b, op),
            ReadAHLPlus => {
                let val = self.get_ind(HL);
                self.read_r8(A, val);
                *self.registers.get_mut_16(HL) += 1;
            }
            ReadAHLMinus => {
                let val = self.get_ind(HL);
                self.read_r8(A, val);
                *self.registers.get_mut_16(HL) -= 1;
            }
            ReadImmR8(r8) => {
                let imm = self.get_imm();
                self.read_r8(r8, imm);
            }
            ReadIndR8(r8, r16) => {
                let val = self.get_ind(r16);
                self.read_r8(r8, val);
            }
            ReadIndImmA(ind_imm) => {
                let val = self.deref_imm(ind_imm);
                self.read_r8(A, val);
            }
            ReadR8R8(lhs, rhs) => {
                let val = self.registers.get_8(rhs);
                self.read_r8(lhs, val);
            }
            ReadR16R16(lhs, rhs) => {
                let val = *self.registers.get_16(rhs);
                *self.registers.get_mut_16(lhs) = val;
            }
            WriteHLPlusA => {
                let addr = *self.registers.get_16(HL);
                self.write_r8(A, addr);
                *self.registers.get_mut_16(HL) += 1;
            }
            WriteHLMinusA => {
                let addr = *self.registers.get_16(HL);
                self.write_r8(A, addr);
                *self.registers.get_mut_16(HL) -= 1;
            }
            ReadHLSPI8 => {
                let imm = self.get_imm();
                let val = *self.registers.get_16(SP) + imm as u16;
                *self.registers.get_mut_16(HL) = val;
            }
            Write8Ind16(r16, r8) => {
                let addr = *self.registers.get_16(r16);
                self.write_r8(r8, addr)
            }
            Write8LowerSP => {
                let addr = self.get_imm_16_const();
                self.memory[addr] = self.registers.get_8(SPLower);
            }
            Write8HigherSP => {
                let addr = self.get_imm_16_const();
                self.memory[addr] = self.registers.get_8(SPHigher);
                *self.registers.get_mut_16(PC) += 2;
            }
            WriteAIndImm(ind_imm) => {
                let addr = self.eval_imm(ind_imm);
                self.write_r8(A, addr);
            },
            AddR16R16(lhs, rhs) => {
                let rhs_val = *self.registers.get_16(rhs);
                self.add_r16_u16(lhs, rhs_val)
            },
            AddSPI8 => {
                let imm = self.get_imm() as i16;
                self.add_r16_i16(SP, imm);
            },
            AddA(op) => {
                let val = self.eval_arith_op(op);
                self.add_8_u8(ExtOperand::Reg(A), val, false);
            }
            AdcA(op) => {
                let val = self.eval_arith_op(op);
                self.add_8_u8(ExtOperand::Reg(A), val, true);
            }
            SubA(op) => {
                let val = self.eval_arith_op(op);
                self.sub_8_u8(ExtOperand::Reg(A), val, false);
            }
            SbcA(op) => {
                let val = self.eval_arith_op(op);
                self.sub_8_u8(ExtOperand::Reg(A), val, true);
            }
            AndA(op) => {
                let val = self.eval_arith_op(op);
                self.bitwise_op_r8(A, val, |x, y| x & y);
            }
            XorA(op) => {
                let val = self.eval_arith_op(op);
                self.bitwise_op_r8(A, val, |x, y| x ^ y);
            }
            OrA(op) => {
                let val = self.eval_arith_op(op);
                self.bitwise_op_r8(A, val, |x, y| x | y);
            }
            CpA(op) => {
                let val = self.eval_arith_op(op);
                self.cp_r8(A, val);
            }
            IncR16(r16) => {
                *self.registers.get_mut_16(r16) += 1
            }
            DecR16(r16) => {
                *self.registers.get_mut_16(r16) -= 1
            }
            IncR8(op) => {
                self.add_8_u8(op, 1, false)
            }
            DecR8(op) => {
                self.sub_8_u8(op, 1, false)
            }
            PopR8(r8) => {
                let sp = *self.registers.get_16(SP);
                let val = self.memory[sp];
                self.registers.set_8(r8, val);
                *self.registers.get_mut_16(SP) += 1;
            }
            PushR8(r8) => self.push_r8(r8),
            PushR8CallU16(r8) => {
                self.push_r8(r8);
                let imm = self.get_imm_16_const();
                *self.registers.get_mut_16(PC) = imm;
            }
            PushR8CallConst(r8, vector) => {
                self.push_r8(r8);
                *self.registers.get_mut_16(PC) = vector;
            }
            JmpRelativeI8 => {
                let amt = self.get_imm() as i8;
                println!("Jump by {}", amt);
                let pc = *self.registers.get_16(PC);
                *self.registers.get_mut_16(PC) = pc.wrapping_add_signed(amt.into());
            }
            JmpU16 => {
                let addr = self.get_imm_16_const();
                *self.registers.get_mut_16(PC) = addr;
            }
            JmpHL => {
                let addr = *self.registers.get_16(HL);
                *self.registers.get_mut_16(PC) = addr;
            }
            CheckFlag(flag) => {
                self.instr_complete = !self.registers.check_flag(flag);
            }
            CheckFlagNot(flag) => {
                self.instr_complete = self.registers.check_flag(flag);
            }
            CheckFlagNotI8(flag) => {
                if self.registers.check_flag(flag) {
                    self.instr_complete = true;
                    *self.registers.get_mut_16(PC) += 1;
                }
            }
            CheckFlagI8(flag) => {
                if !self.registers.check_flag(flag) {
                    self.instr_complete = true;
                    *self.registers.get_mut_16(PC) += 1;
                }
            }
            CheckFlagNotU16(flag) => {
                if self.registers.check_flag(flag) {
                    self.instr_complete = true;
                    *self.registers.get_mut_16(PC) += 2;
                }
            }
            CheckFlagU16(flag) => {
                if !self.registers.check_flag(flag) {
                    self.instr_complete = true;
                    *self.registers.get_mut_16(PC) += 2;
                }
            }
            FetchExtended => self.fetch_extended(),
            Stop => panic!("Stop not yet implemented"),
            Halt => panic!("Halt not yet implemented"),
            End => panic!("Should not reach here"),
        }
    }

    fn get_imm(&mut self) -> u8 {
        let pc = self.registers.get_mut_16(RegisterName16::PC);
        let imm = self.memory[*pc];
        *pc += 1;
        imm
    }

    fn get_imm_16_const(&self) -> u16 {
        let pc = *self.registers.get_16(RegisterName16::PC);
        let imm1 = self.memory[pc];
        let imm2 = self.memory[pc + 1];

        u16::from_le_bytes([imm2, imm1])
    }

    fn eval_arith_op(&mut self, arith_op: ArithOperand) -> u8 {
        use ArithOperand::*;
        match arith_op {
            Reg(r8) => {
                self.registers.get_8(r8)
            }
            HLIndirect => {
                let hl = *self.registers.get_16(RegisterName16::HL);
                self.memory[hl]
            }
            Immediate => {
                self.get_imm()
            }
        }
    }

    fn deref_imm(&mut self, imm: ImmType) -> u8 {
        let addr = self.eval_imm(imm);
        self.memory[addr]
    }

    fn eval_imm(&mut self, imm: ImmType) -> u16 {
        use ImmType::*;
        match imm {
            HighImm => {
                let val = self.get_imm();
                0xFF00 + val as u16
            }
            HighImmPlusC => {
                let c = self.registers.check_flag(Flag::Carry);
                0xFF00 + c as u16
            }
            DefaultImm => {
                let val = self.get_imm();
                val as u16
            }
        }
    }

    fn get_ind(&self, r16: RegisterName16) -> u8 {
        let reg = *self.registers.get_16(r16);
        self.memory[reg]
    }

    fn fetch_extended(&mut self) {
        let pc = self.registers.get_mut_16(RegisterName16::PC);
        let val = self.memory[*pc];
        *pc += 1;
        self.cur_micro_ops = &EXTENDED_INSTRUCTION_OPS[val as usize];
        self.micro_op_index = 0;
    }

    fn eval_ext_operand(&self, oper: ExtOperand) -> u8 {
        use ExtOperand::*;
        match oper {
            Reg(r8) => self.registers.get_8(r8),
            HLIndirect => self.memory[*self.registers.get_16(RegisterName16::HL)],
        }
    }

    fn set_ext_operand(&mut self, oper: ExtOperand, val: u8) {
        use ExtOperand::*;
        match oper {
            Reg(r8) => self.registers.set_8(r8, val),
            HLIndirect => self.memory[*self.registers.get_16(RegisterName16::HL)] = val,
        }
    }

    fn set_shift_flags(&mut self, res: u8, c: bool) {
        self.registers.assign_flag(Flag::Zero, res == 0);
        self.registers.assign_flag(Flag::Carry, c);
        self.registers.clear_flag(Flag::Subtraction);
        self.registers.clear_flag(Flag::HalfCarry);
    }
    
    fn rl(&mut self, oper: ExtOperand, use_carry: bool) {
        use ExtOperand::*;
        
        let use_carry = use_carry as u8;
        let carry = self.registers.check_flag(Flag::Carry) as u8 | use_carry;

        let res = self.eval_ext_operand(oper);
        let c = res & 0x80 > 0;
        let res = res.rotate_left(1) & (0xFE | (carry) | (c as u8 & !use_carry));

        self.set_ext_operand(oper, res);
        
        self.set_shift_flags(res, c);
    }

    fn rr(&mut self, oper: ExtOperand, use_carry: bool) {
        use ExtOperand::*;
        
        let use_carry = use_carry as u8;
        let carry = self.registers.check_flag(Flag::Carry) as u8 | use_carry;

        let res = self.eval_ext_operand(oper);
        let c = res & 0b1 > 0;
        let res = res.rotate_right(1) & (0b01111111u8 | (carry) | (c as u8 & !use_carry));

        self.set_ext_operand(oper, res);

        self.set_shift_flags(res, c);
    }

    fn sla(&mut self, oper: ExtOperand) {
        let mut res = self.eval_ext_operand(oper);
        let c = res & 0x80 > 0;

        res <<= 1;

        self.set_ext_operand(oper, res);

        self.set_shift_flags(res, c);
    }

    fn sra(&mut self, oper: ExtOperand) {
        let mut res = self.eval_ext_operand(oper);
        let c = res & 0b1 > 0;
        let sign = res & 0b10000000;

        res >>= 1;
        res |= sign;

        self.set_ext_operand(oper, res);

        self.set_shift_flags(res, c);
    }

    fn srl(&mut self, oper: ExtOperand) {
        let mut res = self.eval_ext_operand(oper);
        let c = res & 0b1 > 0;

        res >>= 1;
        self.set_ext_operand(oper, res);

        self.set_shift_flags(res, c);
    }

    fn swap(&mut self, oper: ExtOperand) {
        let mut res = self.eval_ext_operand(oper);
        res = ((res & 0xF) << 4) | ((res & 0xF0) >> 4);
        self.set_ext_operand(oper, res);
        self.registers.set_8(RegisterName8::F, 0);
    }

    fn bit(&mut self, b: u8, oper: ExtOperand) {
        let val = self.eval_ext_operand(oper);
        self.registers.assign_flag(Flag::Zero, val & (1 << b) > 0);
        self.registers.clear_flag(Flag::Subtraction);
        self.registers.set_flag(Flag::HalfCarry);
    }

    fn res(&mut self, b: u8, oper: ExtOperand) {
        let val = self.eval_ext_operand(oper);
        self.set_ext_operand(oper, val & !(1 << b));
    }

    fn set(&mut self, b: u8, oper: ExtOperand) {
        let val = self.eval_ext_operand(oper);
        self.set_ext_operand(oper, val | (1 << b));
    }

    fn daa(&mut self) {
        let mut a_reg = self.registers.get_8(RegisterName8::A);
        if !self.registers.check_flag(Flag::Subtraction) {
            if self.registers.check_flag(Flag::Carry) || a_reg > 0x99 {
                a_reg += 0x60;
                self.registers.set_flag(Flag::Carry);
            }

            if self.registers.check_flag(Flag::HalfCarry) || (a_reg & 0x0F) > 0x09 {
                a_reg += 0x6;
            }
        } else {
            if self.registers.check_flag(Flag::Carry) {
                a_reg -= 0x60;
            }

            if self.registers.check_flag(Flag::HalfCarry) {
                a_reg -= 0x6;
            }
        }

        self.registers.assign_flag(Flag::Zero, a_reg == 0);
        self.registers.clear_flag(Flag::HalfCarry);
    }

    fn read_r8(&mut self, r8: RegisterName8, val: u8) {
        self.registers.set_8(r8, val);
    }

    fn write_r8(&mut self, r8: RegisterName8, addr: u16) {
        let val = self.registers.get_8(r8);
        self.memory[addr] = val;
    }

    fn add_r16_u16(&mut self, lhs: RegisterName16, rhs_val: u16) {
        let lhs_val = *self.registers.get_16(lhs);
        let half_carry = ((lhs_val & 0xFFF) + (rhs_val & 0xFFF)) & 0x1000 == 0x1000;
        let sum = lhs_val.wrapping_add(rhs_val);

        let carry = sum < rhs_val;

        *self.registers.get_mut_16(lhs) = sum;

        self.registers.assign_flag(Flag::Carry, carry);
        self.registers.assign_flag(Flag::HalfCarry, half_carry);
        self.registers.clear_flag(Flag::Subtraction);
    }

    fn add_r16_i16(&mut self, lhs: RegisterName16, rhs_val: i16) {
        let lhs_val = *self.registers.get_16(lhs);
        let half_carry = ((lhs_val & 0xF).wrapping_add_signed(rhs_val & 0xF)) & 0x10 == 0x10;
        let carry = ((lhs_val & 0xFF).wrapping_add_signed(rhs_val & 0xFF) & 0x100) == 0x100;
        let sum = lhs_val.wrapping_add_signed(rhs_val);

        *self.registers.get_mut_16(lhs) = sum;

        self.registers.assign_flag(Flag::Carry, carry);
        self.registers.assign_flag(Flag::HalfCarry, half_carry);
        self.registers.clear_flag(Flag::Subtraction);
    }

    fn add_8_u8(&mut self, op: ExtOperand, rhs_val: u8, use_carry: bool) {
        let c = self.registers.check_flag(Flag::Carry);
        let lhs_val = self.eval_ext_operand(op);
        let half_carry = ((lhs_val & 0xF).wrapping_add(rhs_val & 0xF)) & 0x10 == 0x10;
        let sum = lhs_val.wrapping_add(rhs_val).wrapping_add(use_carry as u8 & c as u8);
        let carry = sum < lhs_val;

        self.set_ext_operand(op, sum);
        self.registers.clear_flag(Flag::Carry);
        self.registers.clear_flag(Flag::Subtraction);
        self.registers.set_flag(Flag::HalfCarry);
        self.registers.assign_flag(Flag::Zero, sum == 0);
    }

    fn sub_8_u8(&mut self, op: ExtOperand, rhs_val: u8, use_carry: bool) {
        let lhs_val = self.eval_ext_operand(op);
        let c = self.registers.check_flag(Flag::Carry);
        let half_carry = ((lhs_val & 0xF).wrapping_sub(rhs_val & 0xF)) & 0x10 == 0x10;
        let sum = lhs_val.wrapping_sub(rhs_val).wrapping_sub(use_carry as u8 & c as u8);
        let carry = sum < lhs_val;
        
        self.set_ext_operand(op, lhs_val);
        self.registers.clear_flag(Flag::Carry);
        self.registers.clear_flag(Flag::Subtraction);
        self.registers.assign_flag(Flag::Zero, sum == 0);
    }

    fn bitwise_op_r8(&mut self, lhs: RegisterName8, rhs_val: u8, f: impl Fn(u8, u8) -> u8) {
        let lhs_val = self.registers.get_8(lhs);
        let result = f(lhs_val, rhs_val);

        self.registers.set_8(lhs, result);
        self.registers.assign_flag(Flag::Zero, result == 0);
        self.registers.clear_flag(Flag::Carry);
        self.registers.set_flag(Flag::HalfCarry);
        self.registers.clear_flag(Flag::Subtraction);
    }

    fn cp_r8(&mut self, lhs: RegisterName8, rhs_val: u8) {
        let lhs_val = self.registers.get_8(lhs);
        let result = lhs_val.wrapping_sub(rhs_val);
        let half_carry = ((lhs_val & 0xF).wrapping_sub(rhs_val & 0xF)) & 0x10 == 0x10;
        let carry = lhs_val < rhs_val;
        self.registers.assign_flag(Flag::Zero, result == 0);
        self.registers.set_flag(Flag::Subtraction);
        self.registers.assign_flag(Flag::HalfCarry, half_carry);
        self.registers.assign_flag(Flag::Carry, carry);
    }
    
    fn push_r8(&mut self, r8: RegisterName8) {
        use RegisterName16::*;
        *self.registers.get_mut_16(SP) -= 1;
        let sp = *self.registers.get_16(SP);
        self.memory[sp] = self.registers.get_8(r8);
    }
}


pub struct Memory {
    mem: [u8; 0x10000],
}

impl std::fmt::Debug for Memory {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        Ok(())
    }
}

impl Memory {
    pub fn new() -> Self {
        Self {
            mem: [0; 0x10000],
        }
    }
}

impl Index<u16> for Memory {
    type Output = u8;

    fn index(&self, index: u16) -> &u8 {
        match index {
            0xC000..=0xDDFF => &self.mem[(index - 0x2000) as usize],
            _ => &self.mem[index as usize],
        }
    } 
}

impl IndexMut<u16> for Memory {
    fn index_mut(&mut self, index: u16) -> &mut u8 {
        match index {
            0xC000..=0xDDFF => &mut self.mem[(index - 0x2000) as usize],
            _ => &mut self.mem[index as usize],
        }
    } 
}

pub struct Registers {
    registers: [u16; 6],
}

impl Registers {
    pub fn new() -> Self {
        Self {
            registers: [0x0100, 0xFF13, 0x00C1, 0x8403, 0xFFFE, 0x0100],
        }
    }
}

impl std::fmt::Debug for Registers {
    fn fmt(&self, form: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let a = self.get_8(RegisterName8::A);
        let f = self.get_8(RegisterName8::F);
        let b = self.get_8(RegisterName8::B);
        let c = self.get_8(RegisterName8::C);
        let d = self.get_8(RegisterName8::D);
        let e = self.get_8(RegisterName8::E);
        let h = self.get_8(RegisterName8::H);
        let l = self.get_8(RegisterName8::L);
        let sp = self.get_16(RegisterName16::SP);
        let pc = self.get_16(RegisterName16::PC);
        form.debug_struct("Registers")
            .field("A", &a)
            .field("F", &f)
            .field("B", &b)
            .field("C", &c)
            .field("D", &d)
            .field("E", &e)
            .field("H", &h)
            .field("L", &l)
            .field("SP", &sp)
            .field("PC", &pc)
            .finish()?;
        Ok(())
    }
}

impl Registers {
    pub fn get_16(&self, reg: RegisterName16) -> &u16 {
        &self.registers[reg as usize]
    }

    pub fn get_mut_16(&mut self, reg: RegisterName16) -> &mut u16 {
        &mut self.registers[reg as usize]
    }

    pub fn get_8(&self, reg: RegisterName8) -> u8 {
        if (reg as usize) % 2 == 0 {
            (self.registers[reg as usize / 2] >> 8) as u8
        } else {
            self.registers[reg as usize / 2] as u8
        }
    }

    pub fn set_8(&mut self, reg: RegisterName8, val: u8) {
        let prev_val = self.registers[reg as usize / 2];
        let val = val as u16;
        if (reg as usize) % 2 == 0 {
            self.registers[reg as usize / 2] = (prev_val & 0xFF) | val << 8;
        } else {
            self.registers[reg as usize / 2] = (prev_val & 0xFF00) | val;
        }
    }

    pub fn check_flag(&self, flag: Flag) -> bool {
        let f = self.registers[RegisterName8::F as usize / 2] as u8;

        f & (flag as u8) > 0
    }

    pub fn assign_flag(&mut self, flag: Flag, set: bool) {
        if set {
            self.set_flag(flag)
        } else {
            self.clear_flag(flag)
        }
    }

    pub fn set_flag(&mut self, flag: Flag) {
        let mut f = self.get_8(RegisterName8::F);

        f |= flag as u8;

        self.set_8(RegisterName8::F, f);
    }

    pub fn clear_flag(&mut self, flag: Flag) {
        let mut f = self.get_8(RegisterName8::F);

        f &= !(flag as u8);

        self.set_8(RegisterName8::F, f);
    }
}

