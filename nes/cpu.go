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
	cyclesLeft byte   // Cycles left in current instruction
	operand    uint16 // operand for current instruction
}

// Stores instruction information that makes up an opcode.
type instruction struct {
	name           string
	operate        func(c *CPU)
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

	// If the addressing mode crosses the page boundary, 1 extra cycle needed.
	if instr.addressingMode != nil && instr.operate != nil && instr.addressingMode(c) {
		c.cyclesLeft++
	}
	instr.operate(c)

	c.PC++
}

// It's either this or a big ol switch statement, ¯\_(ツ)_/¯
var instructions = map[byte]instruction{
	0xEA: instruction{"NOP", (*CPU).nop, (*CPU).imp, 2},
}

// Addressing Modes
// Implied
func (c *CPU) imp() bool {
	return false
}

// Immediate
func (c *CPU) imm() bool {
	return false
}

// Zero Page
func (c *CPU) zp0() bool {
	return false
}

// Zero Page with X Offset
func (c *CPU) zpx() bool {
	return false
}

// Zero Page with Y Offset
func (c *CPU) zpy() bool {
	return false
}

// Relative
func (c *CPU) rel() bool {
	return false
}

// Absolute
func (c *CPU) abs() bool {
	return false
}

// Absolute with X Offset
func (c *CPU) abx() bool {
	return false
}

// Absolute with Y Offset
func (c *CPU) aby() bool {
	return false
}

// Indirect
func (c *CPU) ind() bool {
	return false
}

// Indirect X
func (c *CPU) izx() bool {
	return false
}

// Indirect Y
func (c *CPU) izy() bool {
	return false
}

// Operations
func (c *CPU) adc() {

}

func (c *CPU) and() {

}

func (c *CPU) asl() {

}

func (c *CPU) bcc() {

}

func (c *CPU) bcs() {

}

func (c *CPU) beq() {

}

func (c *CPU) bit() {

}

func (c *CPU) bmi() {

}

func (c *CPU) bne() {

}

func (c *CPU) bpl() {

}

func (c *CPU) brk() {

}

func (c *CPU) bvc() {

}

func (c *CPU) bvs() {

}

func (c *CPU) clc() {

}

func (c *CPU) cld() {

}

func (c *CPU) cli() {

}

func (c *CPU) clv() {

}

func (c *CPU) cmp() {

}

func (c *CPU) cpx() {

}

func (c *CPU) cpy() {

}

func (c *CPU) dec() {

}

func (c *CPU) dex() {

}

func (c *CPU) dey() {

}

func (c *CPU) eor() {

}

func (c *CPU) inc() {

}

func (c *CPU) inx() {

}

func (c *CPU) iny() {

}

func (c *CPU) jmp() {

}

func (c *CPU) jsr() {

}

func (c *CPU) lda() {

}

func (c *CPU) ldx() {

}

func (c *CPU) ldy() {

}

func (c *CPU) lsr() {

}

func (c *CPU) nop() {

}

func (c *CPU) ora() {

}

func (c *CPU) pha() {

}

func (c *CPU) php() {

}

func (c *CPU) pla() {

}

func (c *CPU) plp() {

}

func (c *CPU) rol() {

}

func (c *CPU) ror() {

}

func (c *CPU) rti() {

}

func (c *CPU) rts() {

}

func (c *CPU) sbc() {

}

func (c *CPU) sec() {

}

func (c *CPU) sed() {

}

func (c *CPU) sei() {

}

func (c *CPU) sta() {

}

func (c *CPU) stx() {

}

func (c *CPU) sty() {

}

func (c *CPU) tax() {

}

func (c *CPU) tay() {

}

func (c *CPU) tsx() {

}

func (c *CPU) txa() {

}

func (c *CPU) txs() {

}

func (c *CPU) tya() {

}
