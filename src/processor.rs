use crate::bus::Bus;

pub struct Processor {
    program_counter: u16,
    stack_pointer: u8,
    acc: u8,
    index_x: u8,
    index_y: u8,
    status: ProcessorStatus,
    execution_state: ExecutionState,
    bus: Bus,
}

#[derive(Clone, Copy)]
pub enum ExecutionState {
    Ready,
    Pending { remaining_ticks: u8 },
}

#[derive(Clone, Copy)]
enum AddressMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
    Accumulator,
}

struct InstructionInfo {
    cycles: u8,
    size: u8,
    jumps: bool,
}

impl Processor {
    pub fn new() -> Self {
        Self {
            program_counter: 0,
            stack_pointer: 0xff,
            acc: 0,
            index_x: 0,
            index_y: 0,
            status: ProcessorStatus::new(),
            execution_state: ExecutionState::Ready,
            bus: Bus::new(),
        }
    }

    pub fn tick(&mut self) {
        match self.execution_state {
            ExecutionState::Ready => {
                let InstructionInfo {
                    cycles,
                    size,
                    jumps,
                } = self.perform_instruction();

                if !jumps {
                    self.program_counter = self.program_counter.wrapping_add(size.into());
                }

                self.execution_state = ExecutionState::Pending {
                    remaining_ticks: cycles - 1,
                }
            }
            ExecutionState::Pending { remaining_ticks } => {
                if remaining_ticks - 1 == 0 {
                    self.execution_state = ExecutionState::Ready;
                } else {
                    self.execution_state = ExecutionState::Pending {
                        remaining_ticks: remaining_ticks - 1,
                    }
                }
            }
        }
    }

    pub fn accumulator(&self) -> u8 {
        self.acc
    }

    pub fn index_x(&self) -> u8 {
        self.index_x
    }

    pub fn index_y(&self) -> u8 {
        self.index_y
    }

    pub fn program_counter(&self) -> u16 {
        self.program_counter
    }

    pub fn stack_pointer(&self) -> u8 {
        self.stack_pointer
    }

    pub fn execution_state(&self) -> ExecutionState {
        self.execution_state
    }

    pub fn status(&self) -> ProcessorStatus {
        self.status
    }

    pub fn vram(&self) -> &[u8; 2048] {
        self.bus.vram()
    }

    fn perform_instruction(&mut self) -> InstructionInfo {
        let instruction = self.current_instruction();

        match instruction {
            // LDA
            0xA9 => self.instruction_lda(AddressMode::Immediate),
            0xA5 => self.instruction_lda(AddressMode::ZeroPage),
            0xB5 => self.instruction_lda(AddressMode::ZeroPageX),
            0xAD => self.instruction_lda(AddressMode::Absolute),
            0xBD => self.instruction_lda(AddressMode::AbsoluteX),
            0xB9 => self.instruction_lda(AddressMode::AbsoluteY),
            0xA1 => self.instruction_lda(AddressMode::IndexedIndirect),
            0xB1 => self.instruction_lda(AddressMode::IndirectIndexed),
            //LDX
            0xA2 => self.instruction_ldx(AddressMode::Immediate),
            0xA6 => self.instruction_ldx(AddressMode::ZeroPage),
            0xB6 => self.instruction_ldx(AddressMode::ZeroPageY),
            0xAE => self.instruction_ldx(AddressMode::Absolute),
            0xBE => self.instruction_ldx(AddressMode::AbsoluteY),
            // LDY
            0xA0 => self.instruction_ldy(AddressMode::Immediate),
            0xA4 => self.instruction_ldy(AddressMode::ZeroPage),
            0xB4 => self.instruction_ldy(AddressMode::ZeroPageX),
            0xAC => self.instruction_ldy(AddressMode::Absolute),
            0xBC => self.instruction_ldy(AddressMode::AbsoluteX),
            // STA
            0x85 => self.instruction_sta(AddressMode::ZeroPage),
            0x95 => self.instruction_sta(AddressMode::ZeroPageX),
            0x8D => self.instruction_sta(AddressMode::Absolute),
            0x9D => self.instruction_sta(AddressMode::AbsoluteX),
            0x99 => self.instruction_sta(AddressMode::AbsoluteY),
            0x81 => self.instruction_sta(AddressMode::IndexedIndirect),
            0x91 => self.instruction_sta(AddressMode::IndirectIndexed),
            // STX
            0x86 => self.instruction_stx(AddressMode::ZeroPage),
            0x96 => self.instruction_stx(AddressMode::ZeroPageY),
            0x8E => self.instruction_stx(AddressMode::Absolute),
            // STY
            0x84 => self.instruction_sty(AddressMode::ZeroPage),
            0x94 => self.instruction_sty(AddressMode::ZeroPageX),
            0x8C => self.instruction_sty(AddressMode::Absolute),
            // TAX
            0xAA => self.instruction_tax(),
            // TAY
            0xA8 => self.instruction_tay(),
            // TXA
            0x8A => self.instruction_txa(),
            // TYA
            0x98 => self.instruction_tya(),
            // TSX
            0xBA => self.instruction_tsx(),
            // TXS
            0x9A => self.instruction_txs(),
            // PHA
            0x48 => self.instruction_pha(),
            // PHP
            0x08 => self.instruction_php(),
            // PLA
            0x68 => self.instruction_pla(),
            // PLP
            0x28 => self.instruction_plp(),
            // AND
            0x29 => self.instruction_and(AddressMode::Immediate),
            0x25 => self.instruction_and(AddressMode::ZeroPage),
            0x35 => self.instruction_and(AddressMode::ZeroPageX),
            0x2D => self.instruction_and(AddressMode::Absolute),
            0x3D => self.instruction_and(AddressMode::AbsoluteX),
            0x39 => self.instruction_and(AddressMode::AbsoluteY),
            0x21 => self.instruction_and(AddressMode::IndexedIndirect),
            0x31 => self.instruction_and(AddressMode::IndirectIndexed),
            // EOR
            0x49 => self.instruction_eor(AddressMode::Immediate),
            0x45 => self.instruction_eor(AddressMode::ZeroPage),
            0x55 => self.instruction_eor(AddressMode::ZeroPageX),
            0x4D => self.instruction_eor(AddressMode::Absolute),
            0x5D => self.instruction_eor(AddressMode::AbsoluteX),
            0x59 => self.instruction_eor(AddressMode::AbsoluteY),
            0x41 => self.instruction_eor(AddressMode::IndexedIndirect),
            0x51 => self.instruction_eor(AddressMode::IndirectIndexed),
            // ORA
            0x09 => self.instruction_ora(AddressMode::Immediate),
            0x05 => self.instruction_ora(AddressMode::ZeroPage),
            0x15 => self.instruction_ora(AddressMode::ZeroPageX),
            0x0D => self.instruction_ora(AddressMode::Absolute),
            0x1D => self.instruction_ora(AddressMode::AbsoluteX),
            0x19 => self.instruction_ora(AddressMode::AbsoluteY),
            0x01 => self.instruction_ora(AddressMode::IndexedIndirect),
            0x11 => self.instruction_ora(AddressMode::IndirectIndexed),
            // BIT
            0x24 => self.instruction_bit(AddressMode::ZeroPage),
            0x2C => self.instruction_bit(AddressMode::Absolute),
            // ADC
            0x69 => self.instruction_adc(AddressMode::Immediate),
            0x65 => self.instruction_adc(AddressMode::ZeroPage),
            0x75 => self.instruction_adc(AddressMode::ZeroPageX),
            0x6D => self.instruction_adc(AddressMode::Absolute),
            0x7D => self.instruction_adc(AddressMode::AbsoluteX),
            0x79 => self.instruction_adc(AddressMode::AbsoluteY),
            0x61 => self.instruction_adc(AddressMode::IndexedIndirect),
            0x71 => self.instruction_adc(AddressMode::IndirectIndexed),
            // SBC
            0xE9 => self.instruction_sbc(AddressMode::Immediate),
            0xE5 => self.instruction_sbc(AddressMode::ZeroPage),
            0xF5 => self.instruction_sbc(AddressMode::ZeroPageX),
            0xED => self.instruction_sbc(AddressMode::Absolute),
            0xFD => self.instruction_sbc(AddressMode::AbsoluteX),
            0xF9 => self.instruction_sbc(AddressMode::AbsoluteY),
            0xE1 => self.instruction_sbc(AddressMode::IndexedIndirect),
            0xF1 => self.instruction_sbc(AddressMode::IndirectIndexed),
            // CMP
            0xC9 => self.instruction_cmp(AddressMode::Immediate),
            0xC5 => self.instruction_cmp(AddressMode::ZeroPage),
            0xD5 => self.instruction_cmp(AddressMode::ZeroPageX),
            0xCD => self.instruction_cmp(AddressMode::Absolute),
            0xDD => self.instruction_cmp(AddressMode::AbsoluteX),
            0xD9 => self.instruction_cmp(AddressMode::AbsoluteY),
            0xC1 => self.instruction_cmp(AddressMode::IndexedIndirect),
            0xD1 => self.instruction_cmp(AddressMode::IndirectIndexed),
            // CPX
            0xE0 => self.instruction_cpx(AddressMode::Immediate),
            0xE4 => self.instruction_cpx(AddressMode::ZeroPage),
            0xEC => self.instruction_cpx(AddressMode::Absolute),
            // CPY
            0xC0 => self.instruction_cpy(AddressMode::Immediate),
            0xC4 => self.instruction_cpy(AddressMode::ZeroPage),
            0xCC => self.instruction_cpy(AddressMode::Absolute),
            // INC
            0xE6 => self.instruction_inc(AddressMode::ZeroPage),
            0xF6 => self.instruction_inc(AddressMode::ZeroPageX),
            0xEE => self.instruction_inc(AddressMode::Absolute),
            0xFE => self.instruction_inc(AddressMode::AbsoluteX),
            // INX
            0xE8 => self.instruction_inx(),
            // INY
            0xC8 => self.instruction_iny(),
            // DEC
            0xC6 => self.instruction_dec(AddressMode::ZeroPage),
            0xD6 => self.instruction_dec(AddressMode::ZeroPageX),
            0xCE => self.instruction_dec(AddressMode::Absolute),
            0xDE => self.instruction_dec(AddressMode::AbsoluteX),
            // DEX
            0xCA => self.instruction_dex(),
            // DEY
            0x88 => self.instruction_dey(),
            // ASL
            0x0A => self.instruction_asl(AddressMode::Accumulator),
            0x06 => self.instruction_asl(AddressMode::ZeroPage),
            0x16 => self.instruction_asl(AddressMode::ZeroPageX),
            0x0E => self.instruction_asl(AddressMode::Absolute),
            0x1E => self.instruction_asl(AddressMode::AbsoluteX),
            // LSR
            0x4A => self.instruction_lsr(AddressMode::Accumulator),
            0x46 => self.instruction_lsr(AddressMode::ZeroPage),
            0x56 => self.instruction_lsr(AddressMode::ZeroPageX),
            0x4E => self.instruction_lsr(AddressMode::Absolute),
            0x5E => self.instruction_lsr(AddressMode::AbsoluteX),
            // ROL
            0x2A => self.instruction_rol(AddressMode::Accumulator),
            0x26 => self.instruction_rol(AddressMode::ZeroPage),
            0x36 => self.instruction_rol(AddressMode::ZeroPageX),
            0x2E => self.instruction_rol(AddressMode::Absolute),
            0x3E => self.instruction_rol(AddressMode::AbsoluteX),
            // ROR
            0x6A => self.instruction_ror(AddressMode::Accumulator),
            0x66 => self.instruction_ror(AddressMode::ZeroPage),
            0x76 => self.instruction_ror(AddressMode::ZeroPageX),
            0x6E => self.instruction_ror(AddressMode::Absolute),
            0x7E => self.instruction_ror(AddressMode::AbsoluteX),
            // JMP
            0x4C => self.instruction_jmp(AddressMode::Absolute),
            0x6C => self.instruction_jmp(AddressMode::Indirect),
            // JSR
            0x20 => self.instruction_jsr(),
            // RTS
            0x60 => self.instruction_rts(),
            // BCC
            0x90 => self.instruction_bcc(),
            // BCS
            0xB0 => self.instruction_bcs(),
            // BEQ
            0xF0 => self.instruction_beq(),
            // BMI
            0x30 => self.instruction_bmi(),
            // BNE
            0xD0 => self.instruction_bne(),
            // BPL
            0x10 => self.instruction_bpl(),
            // BVC
            0x50 => self.instruction_bvc(),
            // BVS
            0x70 => self.instruction_bvs(),
            // CLC
            0x18 => self.instruction_clc(),
            // CLD
            0xD8 => self.instruction_cld(),
            // CLI
            0x58 => self.instruction_cli(),
            // CLV
            0xB8 => self.instruction_clv(),
            // SEC
            0x38 => self.instruction_sec(),
            // SED
            0xF8 => self.instruction_sed(),
            // SEI
            0x78 => self.instruction_sei(),
            // BRK
            0x00 => self.instruction_brk(),
            // NOP
            0xEA => self.instruction_nop(),
            // NOP
            0x40 => self.instruction_rti(),
            _ => panic!("Invalid instruction"),
        }
    }

    fn current_instruction(&self) -> u8 {
        self.read(self.program_counter)
    }

    fn resolve_address_mode(&self, mode: AddressMode) -> (u16, bool) {
        match mode {
            AddressMode::Immediate => (self.program_counter + 1, false),
            AddressMode::ZeroPage => (self.read(self.program_counter + 1) as u16, false),
            AddressMode::ZeroPageX => {
                let base = self.read(self.program_counter + 1);
                (base.wrapping_add(self.index_x) as u16, false)
            }
            AddressMode::ZeroPageY => {
                let base = self.read(self.program_counter + 1);
                (base.wrapping_add(self.index_y) as u16, false)
            }
            AddressMode::Absolute => {
                let lb = self.read(self.program_counter + 1);
                let hb = self.read(self.program_counter + 2);
                let addr = u16::from_be_bytes([hb, lb]);

                (addr, false)
            }
            AddressMode::AbsoluteX => {
                let lb = self.read(self.program_counter + 1);
                let hb = self.read(self.program_counter + 2);
                let base = u16::from_be_bytes([hb, lb]);

                let addr = base.wrapping_add(self.index_x as u16);
                let (_, page_crossed) = base.to_be_bytes()[1].overflowing_add(self.index_x);

                (addr, page_crossed)
            }
            AddressMode::AbsoluteY => {
                let lb = self.read(self.program_counter + 1);
                let hb = self.read(self.program_counter + 2);
                let base = u16::from_be_bytes([hb, lb]);

                let addr = base.wrapping_add(self.index_y as u16);
                let (_, page_crossed) = base.to_be_bytes()[1].overflowing_add(self.index_y);

                (addr, page_crossed)
            }

            AddressMode::Relative => {
                let rel = self.read(self.program_counter + 1) as i8;
                let addr = (self.program_counter as i16).wrapping_add(rel as i16);

                let (_, page_crossed) =
                    (self.program_counter.to_be_bytes()[1] as i8).overflowing_add(rel);

                (addr as u16, page_crossed)
            }
            AddressMode::Indirect => {
                let lb = self.read(self.program_counter + 1);
                let hb = self.read(self.program_counter + 2);
                let first_addr = u16::from_be_bytes([hb, lb]);

                let lb = self.read(first_addr);
                let hb = self.read(first_addr + 1);
                let addr = u16::from_be_bytes([hb, lb]);

                (addr, false)
            }
            AddressMode::IndexedIndirect => {
                let base = self.read(self.program_counter + 1);
                let indirect_addr = (base.wrapping_add(self.index_x)) as u16;
                let lb = self.read(indirect_addr);
                let hb = self.read(indirect_addr + 1);

                let addr = u16::from_le_bytes([lb, hb]);

                (addr, false)
            }
            AddressMode::IndirectIndexed => {
                let base = self.read(self.program_counter + 1) as u16;

                let (lb, carry) = self.read(base).overflowing_add(self.index_y);
                let hb = self.read(base + 1) + if carry { 1 } else { 0 };

                let addr = u16::from_le_bytes([lb, hb]);

                (addr, carry)
            }
            _ => panic!("Not supported"),
        }
    }

    fn read(&self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn write(&mut self, addr: u16, value: u8) {
        self.bus.mem_write(addr, value);
    }

    fn push(&mut self, value: u8) {
        self.write((self.stack_pointer as u16) + 0x0100, value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn pop(&mut self) -> u8 {
        let value = self.read((self.stack_pointer as u16) + 0x0100);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        value
    }

    fn instruction_lda(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);
        self.acc = value;

        self.status.set_zero(self.acc == 0x00);
        self.status.set_negative((self.acc as i8) < 0);

        match mode {
            AddressMode::Immediate => InstructionInfo {
                size: 2,
                cycles: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                size: 2,
                cycles: 3,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                size: 2,
                cycles: 4,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                size: 3,
                cycles: 4,
                jumps: false,
            },
            AddressMode::AbsoluteX => {
                if page_crossed {
                    InstructionInfo {
                        size: 3,
                        cycles: 4,
                        jumps: false,
                    }
                } else {
                    InstructionInfo {
                        size: 3,
                        cycles: 5,
                        jumps: false,
                    }
                }
            }
            AddressMode::AbsoluteY => {
                if page_crossed {
                    InstructionInfo {
                        size: 3,
                        cycles: 4,
                        jumps: false,
                    }
                } else {
                    InstructionInfo {
                        size: 3,
                        cycles: 5,
                        jumps: false,
                    }
                }
            }
            AddressMode::IndexedIndirect => InstructionInfo {
                size: 2,
                cycles: 6,
                jumps: false,
            },
            AddressMode::IndirectIndexed => {
                if page_crossed {
                    InstructionInfo {
                        size: 3,
                        cycles: 6,
                        jumps: false,
                    }
                } else {
                    InstructionInfo {
                        size: 3,
                        cycles: 5,
                        jumps: false,
                    }
                }
            }
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_ldx(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);
        self.index_x = value;

        self.status.set_zero(self.index_x == 0x00);
        self.status.set_negative((self.index_x as i8) < 0);

        match mode {
            AddressMode::Immediate => InstructionInfo {
                size: 2,
                cycles: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                size: 2,
                cycles: 3,
                jumps: false,
            },
            AddressMode::ZeroPageY => InstructionInfo {
                size: 2,
                cycles: 4,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                size: 3,
                cycles: 4,
                jumps: false,
            },
            AddressMode::AbsoluteY => {
                if page_crossed {
                    InstructionInfo {
                        size: 3,
                        cycles: 5,
                        jumps: false,
                    }
                } else {
                    InstructionInfo {
                        size: 3,
                        cycles: 4,
                        jumps: false,
                    }
                }
            }
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_ldy(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);
        self.index_y = value;

        self.status.set_zero(self.index_y == 0x00);
        self.status.set_negative((self.index_y as i8) < 0);

        match mode {
            AddressMode::Immediate => InstructionInfo {
                size: 2,
                cycles: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                size: 2,
                cycles: 3,
                jumps: false,
            },
            AddressMode::ZeroPageY => InstructionInfo {
                size: 2,
                cycles: 4,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                size: 3,
                cycles: 4,
                jumps: false,
            },
            AddressMode::AbsoluteY => {
                if page_crossed {
                    InstructionInfo {
                        size: 3,
                        cycles: 5,
                        jumps: false,
                    }
                } else {
                    InstructionInfo {
                        size: 3,
                        cycles: 4,
                        jumps: false,
                    }
                }
            }
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_sta(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);
        self.write(addr, self.acc);

        match mode {
            AddressMode::ZeroPage => InstructionInfo {
                size: 2,
                cycles: 3,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                size: 2,
                cycles: 4,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                size: 3,
                cycles: 4,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                size: 3,
                cycles: 5,
                jumps: false,
            },
            AddressMode::AbsoluteY => InstructionInfo {
                size: 3,
                cycles: 5,
                jumps: false,
            },
            AddressMode::IndexedIndirect => InstructionInfo {
                size: 2,
                cycles: 6,
                jumps: false,
            },
            AddressMode::IndirectIndexed => InstructionInfo {
                size: 2,
                cycles: 6,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_stx(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);
        self.write(addr, self.index_x);

        match mode {
            AddressMode::ZeroPage => InstructionInfo {
                size: 2,
                cycles: 3,
                jumps: false,
            },
            AddressMode::ZeroPageY => InstructionInfo {
                size: 2,
                cycles: 4,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                size: 3,
                cycles: 4,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_sty(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);
        self.write(addr, self.index_y);

        match mode {
            AddressMode::ZeroPage => InstructionInfo {
                size: 2,
                cycles: 3,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                size: 2,
                cycles: 4,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                size: 3,
                cycles: 4,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_tax(&mut self) -> InstructionInfo {
        self.index_x = self.acc;

        self.status.set_zero(self.index_x == 0x00);
        self.status.set_negative((self.index_x as i8) < 0);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_tay(&mut self) -> InstructionInfo {
        self.index_y = self.acc;

        self.status.set_zero(self.index_y == 0x00);
        self.status.set_negative((self.index_y as i8) < 0);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_txa(&mut self) -> InstructionInfo {
        self.acc = self.index_x;

        if self.acc == 0x00 {
            self.status.set_zero(true);
        }

        if (self.acc as i8) < 0 {
            self.status.set_negative(true);
        }

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_tya(&mut self) -> InstructionInfo {
        self.acc = self.index_y;

        self.status.set_zero(self.acc == 0x00);
        self.status.set_negative((self.acc as i8) < 0);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_tsx(&mut self) -> InstructionInfo {
        self.index_x = self.stack_pointer;

        self.status.set_zero(self.index_x == 0x00);
        self.status.set_negative((self.index_x as i8) < 0);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_txs(&mut self) -> InstructionInfo {
        self.stack_pointer = self.index_x;

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_pha(&mut self) -> InstructionInfo {
        self.push(self.acc);

        InstructionInfo {
            size: 1,
            cycles: 3,
            jumps: false,
        }
    }

    fn instruction_php(&mut self) -> InstructionInfo {
        self.push(self.status.0);

        InstructionInfo {
            cycles: 3,
            size: 1,
            jumps: false,
        }
    }

    fn instruction_pla(&mut self) -> InstructionInfo {
        let value = self.pop();
        self.acc = value;

        self.status.set_zero(self.acc == 0x00);
        self.status.set_negative((self.acc as i8) < 0);

        InstructionInfo {
            size: 1,
            cycles: 4,
            jumps: false,
        }
    }

    fn instruction_plp(&mut self) -> InstructionInfo {
        let status = self.pop();
        self.status = ProcessorStatus(status);

        InstructionInfo {
            cycles: 4,
            size: 1,
            jumps: false,
        }
    }

    fn instruction_and(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);

        self.acc &= value;

        self.status.set_zero(self.acc == 0x00);
        self.status.set_negative((self.acc as i8) < 0);

        match mode {
            AddressMode::Immediate => InstructionInfo {
                cycles: 2,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 4,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteY => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::IndexedIndirect => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::IndirectIndexed => InstructionInfo {
                cycles: 5 + if page_crossed { 1 } else { 0 },
                size: 2,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_eor(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);

        self.acc ^= value;

        self.status.set_zero(self.acc == 0x00);
        self.status.set_negative((self.acc as i8) < 0);

        match mode {
            AddressMode::Immediate => InstructionInfo {
                cycles: 2,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 4,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteY => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::IndexedIndirect => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::IndirectIndexed => InstructionInfo {
                cycles: 5 + if page_crossed { 1 } else { 0 },
                size: 2,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_ora(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);

        self.acc |= value;

        self.status.set_zero(self.acc == 0x00);
        self.status.set_negative((self.acc as i8) < 0);

        match mode {
            AddressMode::Immediate => InstructionInfo {
                cycles: 2,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 4,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteY => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::IndexedIndirect => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::IndirectIndexed => InstructionInfo {
                cycles: 5 + if page_crossed { 1 } else { 0 },
                size: 2,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_bit(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);
        let value = self.read(addr);

        let result = self.acc & value;

        self.status.set_zero(result == 0x00);
        self.status.set_overflow((value & 0b01000000) != 0);
        self.status.set_negative((value & 0b10000000) != 0);

        match mode {
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_adc(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);
        let (result, carry) = self.acc.overflowing_add(value);
        let (_, overflow) = (self.acc as i8).overflowing_add(value as i8);

        self.acc = result;

        self.status.set_zero(self.acc == 0x00);
        self.status.set_negative((self.acc as i8) < 0);
        self.status.set_carry(carry);
        self.status.set_overflow(overflow);

        match mode {
            AddressMode::Immediate => InstructionInfo {
                cycles: 2,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 4,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteY => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::IndexedIndirect => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::IndirectIndexed => InstructionInfo {
                cycles: 5 + if page_crossed { 1 } else { 0 },
                size: 2,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_sbc(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);
        let (result, carry) = self.acc.overflowing_sub(value);
        let (_, overflow) = (self.acc as i8).overflowing_sub(value as i8);

        self.acc = result;

        self.status.set_zero(self.acc == 0x00);
        self.status.set_negative((self.acc as i8) < 0);
        self.status.set_carry(carry);
        self.status.set_overflow(overflow);

        match mode {
            AddressMode::Immediate => InstructionInfo {
                cycles: 2,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 4,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteY => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::IndexedIndirect => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::IndirectIndexed => InstructionInfo {
                cycles: 5 + if page_crossed { 1 } else { 0 },
                size: 2,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_cmp(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, page_crossed) = self.resolve_address_mode(mode);
        let value = self.read(addr);

        let (result, carry) = self.acc.overflowing_sub(value);

        self.status.set_zero(result == 0x00);
        self.status.set_negative((result as i8) < 0);
        self.status.set_carry(!carry); // ????? <https://www.nesdev.org/obelisk-6502-guide/reference.html#CMP>

        match mode {
            AddressMode::Immediate => InstructionInfo {
                cycles: 2,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 4,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteY => InstructionInfo {
                cycles: 4 + if page_crossed { 1 } else { 0 },
                size: 3,
                jumps: false,
            },
            AddressMode::IndexedIndirect => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::IndirectIndexed => InstructionInfo {
                cycles: 5 + if page_crossed { 1 } else { 0 },
                size: 2,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_cpx(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);
        let value = self.read(addr);

        let (result, carry) = self.index_x.overflowing_sub(value);

        self.status.set_zero(result == 0x00);
        self.status.set_negative((result as i8) < 0);
        self.status.set_carry(!carry); // ????? <https://www.nesdev.org/obelisk-6502-guide/reference.html#CPX>

        match mode {
            AddressMode::Immediate => InstructionInfo {
                cycles: 2,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_cpy(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);
        let value = self.read(addr);

        let (result, carry) = self.index_y.overflowing_sub(value);

        self.status.set_zero(result == 0x00);
        self.status.set_negative((result as i8) < 0);
        self.status.set_carry(!carry); // ????? <https://www.nesdev.org/obelisk-6502-guide/reference.html#CPY>

        match mode {
            AddressMode::Immediate => InstructionInfo {
                cycles: 2,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 3,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 4,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_inc(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);

        let prev = self.read(addr);
        let value = prev.wrapping_add(1);
        self.write(addr, value);

        self.status.set_zero(value == 0x00);
        self.status.set_negative((value as i8) < 0);

        match mode {
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 5,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 6,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 7,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_inx(&mut self) -> InstructionInfo {
        self.index_x = self.index_x.wrapping_add(1);

        self.status.set_zero(self.index_x == 0x00);
        self.status.set_negative((self.index_x as i8) < 0);

        InstructionInfo {
            cycles: 2,
            size: 1,
            jumps: false,
        }
    }

    fn instruction_iny(&mut self) -> InstructionInfo {
        self.index_y = self.index_y.wrapping_add(1);

        self.status.set_zero(self.index_y == 0x00);
        self.status.set_negative((self.index_y as i8) < 0);

        InstructionInfo {
            cycles: 2,
            size: 1,
            jumps: false,
        }
    }

    fn instruction_dec(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);

        let prev = self.read(addr);
        let value = prev.wrapping_sub(1);
        self.write(addr, value);

        self.status.set_zero(value == 0x00);
        self.status.set_negative((value as i8) < 0);

        match mode {
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 5,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 6,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 7,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_dex(&mut self) -> InstructionInfo {
        self.index_x = self.index_x.wrapping_sub(1);

        self.status.set_zero(self.index_x == 0x00);
        self.status.set_negative((self.index_x as i8) < 0);

        InstructionInfo {
            cycles: 2,
            size: 1,
            jumps: false,
        }
    }

    fn instruction_dey(&mut self) -> InstructionInfo {
        self.index_x = self.index_x.wrapping_sub(1);

        self.status.set_zero(self.index_y == 0x00);
        self.status.set_negative((self.index_y as i8) < 0);

        InstructionInfo {
            cycles: 2,
            size: 1,
            jumps: false,
        }
    }

    fn instruction_asl(&mut self, mode: AddressMode) -> InstructionInfo {
        let value = match mode {
            AddressMode::Accumulator => self.acc,

            _ => {
                let (addr, _) = self.resolve_address_mode(mode);
                self.read(addr)
            }
        };

        let result = value << 1;
        self.status.set_zero(result == 0x00);
        self.status.set_negative((result as i8) < 0);
        self.status.set_carry((value & 0x80) != 0x00);
        self.acc = result;

        match mode {
            AddressMode::Accumulator => InstructionInfo {
                cycles: 2,
                size: 1,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 5,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 6,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 7,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_lsr(&mut self, mode: AddressMode) -> InstructionInfo {
        let value = match mode {
            AddressMode::Accumulator => self.acc,

            _ => {
                let (addr, _) = self.resolve_address_mode(mode);
                self.read(addr)
            }
        };

        let result = value >> 1;
        self.status.set_zero(result == 0x00);
        self.status.set_negative((result as i8) < 0);
        self.status.set_carry((value & 0x01) != 0x00);
        self.acc = result;

        match mode {
            AddressMode::Accumulator => InstructionInfo {
                cycles: 2,
                size: 1,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 5,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 6,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 7,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_rol(&mut self, mode: AddressMode) -> InstructionInfo {
        let value = match mode {
            AddressMode::Accumulator => self.acc,

            _ => {
                let (addr, _) = self.resolve_address_mode(mode);
                self.read(addr)
            }
        };

        let mut result = value << 1;
        if self.status.carry() {
            result |= 0x01;
        }
        self.status.set_zero(result == 0x00);
        self.status.set_negative((result as i8) < 0);
        self.status.set_carry((value & 0x80) != 0x00);
        self.acc = result;

        match mode {
            AddressMode::Accumulator => InstructionInfo {
                cycles: 2,
                size: 1,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 5,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 6,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 7,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_ror(&mut self, mode: AddressMode) -> InstructionInfo {
        let value = match mode {
            AddressMode::Accumulator => self.acc,

            _ => {
                let (addr, _) = self.resolve_address_mode(mode);
                self.read(addr)
            }
        };

        let mut result = value >> 1;
        if self.status.carry() {
            result |= 0x80;
        }
        self.status.set_zero(result == 0x00);
        self.status.set_negative((result as i8) < 0);
        self.status.set_carry((value & 0x01) != 0x00);
        self.acc = result;

        match mode {
            AddressMode::Accumulator => InstructionInfo {
                cycles: 2,
                size: 1,
                jumps: false,
            },
            AddressMode::ZeroPage => InstructionInfo {
                cycles: 5,
                size: 2,
                jumps: false,
            },
            AddressMode::ZeroPageX => InstructionInfo {
                cycles: 6,
                size: 2,
                jumps: false,
            },
            AddressMode::Absolute => InstructionInfo {
                cycles: 6,
                size: 3,
                jumps: false,
            },
            AddressMode::AbsoluteX => InstructionInfo {
                cycles: 7,
                size: 3,
                jumps: false,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_jmp(&mut self, mode: AddressMode) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(mode);
        self.program_counter = addr;

        match mode {
            AddressMode::Absolute => InstructionInfo {
                size: 3,
                cycles: 3,
                jumps: true,
            },
            AddressMode::Indirect => InstructionInfo {
                size: 3,
                cycles: 5,
                jumps: true,
            },
            _ => panic!("Mode not supported"),
        }
    }

    fn instruction_jsr(&mut self) -> InstructionInfo {
        let (addr, _) = self.resolve_address_mode(AddressMode::Absolute);
        self.program_counter = addr;

        let [lb, hb] = (addr + 2).to_le_bytes();
        self.write((self.stack_pointer as u16) + 0x0100, hb);
        self.stack_pointer -= 1;
        self.write((self.stack_pointer as u16) + 0x0100, lb);
        self.stack_pointer -= 1;

        InstructionInfo {
            cycles: 6,
            size: 3,
            jumps: true,
        }
    }

    fn instruction_rts(&mut self) -> InstructionInfo {
        self.stack_pointer += 1;
        let lb = self.read((self.stack_pointer as u16) + 0x0100);
        self.stack_pointer += 1;
        let hb = self.read((self.stack_pointer as u16) + 0x0100);

        let addr = u16::from_le_bytes([lb, hb]);
        let addr = addr + 1;

        self.program_counter = addr;

        InstructionInfo {
            cycles: 6,
            size: 1,
            jumps: true,
        }
    }

    fn instruction_bcc(&mut self) -> InstructionInfo {
        let should_branch = !self.status.carry();
        let (addr, page_crossed) = self.resolve_address_mode(AddressMode::Relative);

        if should_branch {
            self.program_counter = addr;
        }

        InstructionInfo {
            size: 2,
            cycles: 2 + if should_branch { 1 } else { 0 } + if page_crossed { 2 } else { 0 },
            jumps: should_branch,
        }
    }

    fn instruction_bcs(&mut self) -> InstructionInfo {
        let should_branch = self.status.carry();
        let (addr, page_crossed) = self.resolve_address_mode(AddressMode::Relative);

        if should_branch {
            self.program_counter = addr;
        }

        InstructionInfo {
            size: 2,
            cycles: 2 + if should_branch { 1 } else { 0 } + if page_crossed { 2 } else { 0 },
            jumps: should_branch,
        }
    }

    fn instruction_beq(&mut self) -> InstructionInfo {
        let should_branch = self.status.zero();
        let (addr, page_crossed) = self.resolve_address_mode(AddressMode::Relative);

        if should_branch {
            self.program_counter = addr;
        }

        InstructionInfo {
            size: 2,
            cycles: 2 + if should_branch { 1 } else { 0 } + if page_crossed { 2 } else { 0 },
            jumps: should_branch,
        }
    }

    fn instruction_bmi(&mut self) -> InstructionInfo {
        let should_branch = self.status.negative();
        let (addr, page_crossed) = self.resolve_address_mode(AddressMode::Relative);

        if should_branch {
            self.program_counter = addr;
        }

        InstructionInfo {
            size: 2,
            cycles: 2 + if should_branch { 1 } else { 0 } + if page_crossed { 2 } else { 0 },
            jumps: should_branch,
        }
    }

    fn instruction_bne(&mut self) -> InstructionInfo {
        let should_branch = !self.status.zero();
        let (addr, page_crossed) = self.resolve_address_mode(AddressMode::Relative);

        if should_branch {
            self.program_counter = addr;
        }

        InstructionInfo {
            size: 2,
            cycles: 2 + if should_branch { 1 } else { 0 } + if page_crossed { 2 } else { 0 },
            jumps: should_branch,
        }
    }

    fn instruction_bpl(&mut self) -> InstructionInfo {
        let should_branch = !self.status.negative();
        let (addr, page_crossed) = self.resolve_address_mode(AddressMode::Relative);

        if should_branch {
            self.program_counter = addr;
        }

        InstructionInfo {
            size: 2,
            cycles: 2 + if should_branch { 1 } else { 0 } + if page_crossed { 2 } else { 0 },
            jumps: should_branch,
        }
    }

    fn instruction_bvc(&mut self) -> InstructionInfo {
        let should_branch = !self.status.overflow();
        let (addr, page_crossed) = self.resolve_address_mode(AddressMode::Relative);

        if should_branch {
            self.program_counter = addr;
        }

        InstructionInfo {
            size: 2,
            cycles: 2 + if should_branch { 1 } else { 0 } + if page_crossed { 2 } else { 0 },
            jumps: should_branch,
        }
    }

    fn instruction_bvs(&mut self) -> InstructionInfo {
        let should_branch = self.status.overflow();
        let (addr, page_crossed) = self.resolve_address_mode(AddressMode::Relative);

        if should_branch {
            self.program_counter = addr;
        }

        InstructionInfo {
            size: 2,
            cycles: 2 + if should_branch { 1 } else { 0 } + if page_crossed { 2 } else { 0 },
            jumps: should_branch,
        }
    }

    fn instruction_clc(&mut self) -> InstructionInfo {
        self.status.set_carry(false);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_cld(&mut self) -> InstructionInfo {
        self.status.set_decimal(false);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_cli(&mut self) -> InstructionInfo {
        self.status.set_interrupt_disable(false);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_clv(&mut self) -> InstructionInfo {
        self.status.set_overflow(false);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_sec(&mut self) -> InstructionInfo {
        self.status.set_carry(true);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_sed(&mut self) -> InstructionInfo {
        self.status.set_decimal(true);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_sei(&mut self) -> InstructionInfo {
        self.status.set_interrupt_disable(true);

        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_brk(&mut self) -> InstructionInfo {
        let [lb, hb] = self.program_counter.to_le_bytes();
        let status = self.status.0;

        self.push(hb);
        self.push(lb);
        self.push(status);

        let addr = {
            let lb = self.read(0xFFFE);
            let hb = self.read(0xFFFF);

            u16::from_le_bytes([lb, hb])
        };

        self.status.set_break(true);
        self.program_counter = addr;

        InstructionInfo {
            jumps: true,
            size: 1,
            cycles: 7,
        }
    }

    fn instruction_nop(&mut self) -> InstructionInfo {
        InstructionInfo {
            size: 1,
            cycles: 2,
            jumps: false,
        }
    }

    fn instruction_rti(&mut self) -> InstructionInfo {
        let status = self.pop();
        let lb = self.pop();
        let hb = self.pop();

        let addr = u16::from_le_bytes([lb, hb]);
        self.status = ProcessorStatus(status);
        self.program_counter = addr;

        InstructionInfo {
            cycles: 6,
            size: 1,
            jumps: true,
        }
    }
}

#[derive(Clone, Copy)]
pub struct ProcessorStatus(u8);

impl ProcessorStatus {
    pub fn new() -> Self {
        Self(0b00100000)
    }

    pub fn negative(&self) -> bool {
        (self.0 & 0b10000000) != 0x00
    }

    pub fn set_negative(&mut self, value: bool) {
        if value {
            self.0 |= 0b10000000;
        } else {
            self.0 &= 0b01111111;
        }
    }

    pub fn overflow(&self) -> bool {
        (self.0 & 0b01000000) != 0x00
    }

    pub fn set_overflow(&mut self, value: bool) {
        if value {
            self.0 |= 0b01000000;
        } else {
            self.0 &= 0b10111111;
        }
    }

    pub fn set_break(&mut self, value: bool) {
        if value {
            self.0 |= 0b00010000;
        } else {
            self.0 &= 0b11101111;
        }
    }

    pub fn decimal(&self) -> bool {
        (self.0 & 0b00001000) != 0x00
    }

    pub fn set_decimal(&mut self, value: bool) {
        if value {
            self.0 |= 0b00001000;
        } else {
            self.0 &= 0b11110111;
        }
    }

    pub fn interrupt_disable(&self) -> bool {
        (self.0 & 0b00000100) != 0x00
    }

    pub fn set_interrupt_disable(&mut self, value: bool) {
        if value {
            self.0 |= 0b00000100;
        } else {
            self.0 &= 0b11111011;
        }
    }

    pub fn zero(&self) -> bool {
        (self.0 & 0b00000010) != 0x00
    }

    pub fn set_zero(&mut self, value: bool) {
        if value {
            self.0 |= 0b00000010;
        } else {
            self.0 &= 0b11111101;
        }
    }

    pub fn carry(&self) -> bool {
        (self.0 & 0b00000001) != 0x00
    }

    pub fn set_carry(&mut self, value: bool) {
        if value {
            self.0 |= 0b00000001;
        } else {
            self.0 &= 0b11111110;
        }
    }
}
