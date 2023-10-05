use std::ops::Index;

use crate::micro_ops::MicroOp;
use crate::micro_ops::RegisterName8;
use crate::micro_ops::RegisterName16;
use crate::micro_ops::InstructionOps;
use crate::micro_ops::INSTRUCTION_MICRO_OPS;
use crate::micro_ops::EXTENDED_INSTRUCTION_OPS;
use crate::micro_ops::MemIndex;

pub struct Cpu {
    pub registers: Registers,
    pub memory: Memory,
    pub fetched_value: u8,
    pub instr_complete: bool,
    pub cur_micro_ops: &'static InstructionOps,
    indirect: u16,
}

impl Cpu {
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

    fn step(&mut self) {

    }
}


pub struct Memory {
    mem: [u8; 0x10000],
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

pub struct Registers {
    registers: [u16; 6],
}

impl Registers {
    pub fn get_16(&self, reg: RegisterName16) -> &u16 {
        &self.registers[reg as usize]
    }

    pub fn get_mut_16(&mut self, reg: RegisterName16) -> &mut u16 {
        &mut self.registers[reg as usize]
    }

    pub fn get_8(&self, reg: RegisterName8) -> u8 {
        if (reg as usize) & 1 != 0 {
            (self.registers[reg as usize / 2] >> 8) as u8
        } else {
            self.registers[reg as usize / 2] as u8
        }
    }

    pub fn set_8(&mut self, reg: RegisterName8, val: u8) {
        let val = self.registers[reg as usize];
        if (reg as usize) & 1 != 0 {
            self.registers[reg as usize / 2] = (val & 0xFF) | ((val as u16) << 8);
        } else {
            self.registers[reg as usize / 2] = (val & 0xFF00) | (val as u16);
        }
    }
}

