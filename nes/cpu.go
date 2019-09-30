package nes

// CPUFlag is used to notify CPU of certain conditions happening when performing an operation, such as a carry, overflow, etc.
type CPUFlag byte

// CPU Flags
const (
	C CPUFlag = (1 << 0) // Carry Bit
	Z CPUFlag = (1 << 1) // Zero
	I CPUFlag = (1 << 2) // Disable Interrupts
	U CPUFlag = (1 << 3) // Decimal (unused by NES, always set to 1)
	B CPUFlag = (1 << 4) // Break
	R CPUFlag = (1 << 5) // Reserved (unused by NES, always set to 1)
	V CPUFlag = (1 << 6) // Overflow
	N CPUFlag = (1 << 7) // Negative
)

// CPU is where program execution happens.
type CPU struct {
	A          byte   // accumulator
	X          byte   // index
	Y          byte   // index
	PC         uint16 // program counter
	S          byte   // stack pointer
	P          byte   // status
	bus        *Bus
	cyclesLeft byte // Cycles left in current instruction
}

// Stores instruction information that makes up an opcode.
type instruction struct {
	name           string
	operation      func() bool
	addressingMode func() bool
	cycles         int
}

// New return a new CPU that is connected to the Bus passed into it.
func New(bus *Bus) *CPU {
	var cpu CPU
	cpu.bus = bus

	return &cpu
}

func (c *CPU) read(addr uint16) byte {
	return c.bus.read(addr)
}

func (c *CPU) write(addr uint16, data byte) {
	c.bus.write(addr, data)
}

func (c *CPU) setFlag(flag CPUFlag, val bool) {
	if val {
		c.P |= byte(flag)
	} else {
		c.P &= byte(^flag)
	}
}

var instructions = map[byte]instruction{
	0x00: instruction{"BRK", nil, nil, 1}, // 255 more of these ...
}

func (c *CPU) run(instr instruction) {
	if instr.addressingMode() && instr.operation() {
		c.cyclesLeft++
	}
}
