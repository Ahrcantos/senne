pub struct Processor {
    program_counter: u16,
    stack_pointer: u8,
    acc: u8,
    index_x: u8,
    index_y: u8,
    status: ProcessorStatus,

    // Reserved memory
    zero_page: [u8; 256],
    stack: [u8; 256],
    non_maskable_interupt_handler: [u8; 2],
    reset_location: [u8; 2],
    interupt_request_handler: [u8; 2],

    execution_state: ExecutionState,
}

enum ExecutionState {
    Ready,
    Pending { remaining_ticks: u8 },
}

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
}

impl Processor {
    pub fn new() -> Self {
        // TODO: Replace with a `ProcessorBuilder`
        Self {
            program_counter: 0,
            stack_pointer: 0x01ff,
            acc: 0,
            index_x: 0,
            index_y: 0,
            status: ProcessorStatus(0),
            zero_page: [0x00; 256],
            stack: [0x00; 256],
            non_maskable_interupt_handler: [0x00, 0x00],
            reset_location: [0x00, 0x00],
            interupt_request_handler: [0x00, 0x00],
            execution_state: ExecutionState::Ready,
        }
    }

    pub fn tick(&mut self) {
        todo!()
    }

    fn perform_instruction(&mut self) {
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
            _ => {},
        }
    }

    fn current_instruction(&self) -> u8 {
        self.read(self.program_counter)
    }


    fn resolve_address_mode(&self, mode: AddressMode) -> u16 {
        match mode {
            AddressMode::Immediate => {
                self.program_counter + 1
            },
            AddressMode::ZeroPage => {
                self.read(self.program_counter + 1) as u16
            }
            AddressMode::ZeroPageX => {
                let base = self.read(self.program_counter + 1);
                base.wrapping_add(self.index_x) as u16
            },
            AddressMode::ZeroPageY => {
                let base = self.read(self.program_counter + 1);
                base.wrapping_add(self.index_y) as u16
            }
            _ => todo!()
        }
    }

    fn read(&self, addr: u16) -> u8 {
        match addr {
            // Zero Page
            0x00..=0xFF => {
                self.zero_page[addr as usize]
            },
            // Stack
            0x100..=0x1FF => {
                self.stack[(addr - 0x100) as usize]
            }
            _ => todo!(),
        }
    }

    fn write(&mut self, addr: u16, value: u8) {
        todo!()
    }

    fn instruction_lda(&mut self, mode: AddressMode) {
        todo!()
    }

    fn was_page_crossed(mode: AddressMode) -> bool {
        todo!()
    }

}

pub struct ProcessorStatus(u8);

impl ProcessorStatus {}