package nes

// Bus handles communication between different components of the emulator
type Bus struct {
	ram [65536]byte
}

func (b *Bus) write(addr uint16, data byte) {
	b.ram[addr] = data
}

func (b *Bus) read(addr uint16) byte {
	return b.ram[addr]
}
