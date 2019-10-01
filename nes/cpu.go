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
	operate        func(c *CPU) bool
	addressingMode func(c *CPU) bool
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

func (c *CPU) run(instr instruction) {
	if instr.addressingMode != nil && instr.operate != nil && instr.addressingMode(c) && instr.operate(c) {
		c.cyclesLeft++
	}
}

// It's either this or a big ol switch statement, ¯\_(ツ)_/¯
var instructions = map[byte]instruction{
	0x00: instruction{"BRK", (*CPU).BRK, (*CPU).IMP, 7},
}

// Addressing Modes
// Implied
func (c *CPU) IMP() bool {
	return false
}

// Immediate
func (c *CPU) IMM() bool {
	return false
}

// Zero Page
func (c *CPU) ZP0() bool {
	return false
}

// Zero Page with X Offset
func (c *CPU) ZPX() bool {
	return false
}

// Zero Page with Y Offset
func (c *CPU) ZPY() bool {
	return false
}

// Relative
func (c *CPU) REL() bool {
	return false
}

// Absolute
func (c *CPU) ABS() bool {
	return false
}

// Absolute with X Offset
func (c *CPU) ABX() bool {
	return false
}

// Absolute with Y Offset
func (c *CPU) ABY() bool {
	return false
}

// Indirect
func (c *CPU) IND() bool {
	return false
}

// Indirect X
func (c *CPU) IZX() bool {
	return false
}

// Indirect Y
func (c *CPU) IZY() bool {
	return false
}

// Operations
func (c *CPU) ADC() bool {
	return false
}

func (c *CPU) AND() bool {
	return false
}

func (c *CPU) ASL() bool {
	return false
}

func (c *CPU) BCC() bool {
	return false
}

func (c *CPU) BCS() bool {
	return false
}

func (c *CPU) BEQ() bool {
	return false
}

func (c *CPU) BIT() bool {
	return false
}

func (c *CPU) BMI() bool {
	return false
}

func (c *CPU) BNE() bool {
	return false
}

func (c *CPU) BPL() bool {
	return false
}

func (c *CPU) BRK() bool {
	return false
}

func (c *CPU) BVC() bool {
	return false
}

func (c *CPU) BVS() bool {
	return false
}

func (c *CPU) CLC() bool {
	return false
}

func (c *CPU) CLD() bool {
	return false
}

func (c *CPU) CLI() bool {
	return false
}

func (c *CPU) CLV() bool {
	return false
}

func (c *CPU) CMP() bool {
	return false
}

func (c *CPU) CPX() bool {
	return false
}

func (c *CPU) CPY() bool {
	return false
}

func (c *CPU) DEC() bool {
	return false
}

func (c *CPU) DEX() bool {
	return false
}

func (c *CPU) DEY() bool {
	return false
}

func (c *CPU) EOR() bool {
	return false
}

func (c *CPU) INC() bool {
	return false
}

func (c *CPU) INX() bool {
	return false
}

func (c *CPU) INY() bool {
	return false
}

func (c *CPU) JMP() bool {
	return false
}

func (c *CPU) JSR() bool {
	return false
}

func (c *CPU) LDA() bool {
	return false
}

func (c *CPU) LDX() bool {
	return false
}

func (c *CPU) LDY() bool {
	return false
}

func (c *CPU) LSR() bool {
	return false
}

func (c *CPU) NOP() bool {
	return false
}

func (c *CPU) ORA() bool {
	return false
}

func (c *CPU) PHA() bool {
	return false
}

func (c *CPU) PHP() bool {
	return false
}

func (c *CPU) PLA() bool {
	return false
}

func (c *CPU) PLP() bool {
	return false
}

func (c *CPU) ROL() bool {
	return false
}

func (c *CPU) ROR() bool {
	return false
}

func (c *CPU) RTI() bool {
	return false
}

func (c *CPU) RTS() bool {
	return false
}

func (c *CPU) SBC() bool {
	return false
}

func (c *CPU) SEC() bool {
	return false
}

func (c *CPU) SED() bool {
	return false
}

func (c *CPU) SEI() bool {
	return false
}

func (c *CPU) STA() bool {
	return false
}

func (c *CPU) STX() bool {
	return false
}

func (c *CPU) STY() bool {
	return false
}

func (c *CPU) TAX() bool {
	return false
}

func (c *CPU) TAY() bool {
	return false
}

func (c *CPU) TSX() bool {
	return false
}

func (c *CPU) TXA() bool {
	return false
}

func (c *CPU) TXS() bool {
	return false
}

func (c *CPU) TYA() bool {
	return false
}
