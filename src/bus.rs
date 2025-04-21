pub struct Bus {
    internal_ram: [u8; 0x0800],
}

impl Bus {
    pub fn new() -> Self {
        let ram = [0x00; 0x0800];

        Bus { internal_ram: ram }
    }

    pub fn vram(&self) -> &[u8; 0x0800] {
        &self.internal_ram
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            // Internal Ram
            0x00..=0x7FF => self.internal_ram[addr as usize],
            _ => todo!(),
        }
    }

    pub fn mem_write(&mut self, addr: u16, value: u8) {
        match addr {
            // Internal Ram
            0x00..=0x7FF => self.internal_ram[addr as usize] = value,
            _ => todo!(),
        }
    }
}
