package nes

// CPUFlag is used to notify CPU of certain conditions happening when performing an operation, such as a carry, overflow, etc.
type CPUFlag byte

// CPU Flags
const (
	C CPUFlag = (1 << 0) // Carry Bit
	Z CPUFlag = (1 << 1) // Zero
	I CPUFlag = (1 << 2) // Disable Interrupts
	D CPUFlag = (1 << 3) // Decimal (unused by NES, always set to 1)
	B CPUFlag = (1 << 4) // Break
	R CPUFlag = (1 << 5) // Reserved (unused by NES, always set to 1)
	V CPUFlag = (1 << 6) // Overflow
	N CPUFlag = (1 << 7) // Negative
)

// CPU is where program execution happens.
type CPU struct {
	A                byte   // accumulator
	X                byte   // index
	Y                byte   // index
	PC               uint16 // program counter
	S                byte   // stack pointer
	P                byte   // status
	bus              *Bus
	cyclesLeft       byte   // Cycles left in current instruction
	opcode           byte   // Current instruction byte
	operand          uint16 // operand for current instruction
	fetched          byte   // fetched data from operand
	usingAccumulator bool
}

// Stores instruction information that makes up an opcode.
type instruction struct {
	name               string
	addressingModeName string
	operate            func(c *CPU)
	addressingMode     func(c *CPU) bool
	cycles             int
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

func (c *CPU) getFlag(flag CPUFlag) bool {
	return c.P&byte(flag) > 0
}

func (c *CPU) reset() {
	lo := uint16(c.read(0xFFFC))
	hi := uint16(c.read(0xFFFD))

	c.PC = (hi << 8) | lo

	c.A = 0
	c.X = 0
	c.Y = 0
	c.P = 0 | byte(R)
	c.S = 0xFD

	c.operand = 0
	c.usingAccumulator = false
	c.fetched = 0

	c.cyclesLeft = 8
}

func (c *CPU) clock() {
	if c.cyclesLeft == 0 {
		c.opcode = c.read(c.PC)
		c.setFlag(R, true)

		c.PC++

		currentInstruction := instructions[c.opcode]
		c.run(currentInstruction)
	}

	c.cyclesLeft--
}

// Interrupt Request
func (c *CPU) irq() {
	if !c.getFlag(I) {
		c.write(0x100+uint16(c.S), byte(c.PC>>8))
		c.S--
		c.write(0x100+uint16(c.S), byte(c.PC))
		c.S--

		c.setFlag(B, false)
		c.setFlag(R, true)
		c.setFlag(I, true)
		c.write(0x100+uint16(c.S), c.P)
		c.S--

		c.operand = 0xFFFE
		low := c.read(c.operand)
		hi := c.read(c.operand + 1)
		c.PC = uint16(low) | (uint16(hi) << 8)

		c.cyclesLeft = 7
	}
}

// Nonmaskable Interrupt
func (c *CPU) nmi() {
	c.write(0x100+uint16(c.S), byte(c.PC>>8))
	c.S--
	c.write(0x100+uint16(c.S), byte(c.PC))
	c.S--

	c.setFlag(B, false)
	c.setFlag(R, true)
	c.setFlag(I, true)
	c.write(0x100+uint16(c.S), c.P)
	c.S--

	c.operand = 0xFFFA
	low := c.read(c.operand)
	hi := c.read(c.operand + 1)
	c.PC = uint16(low) | (uint16(hi) << 8)

	c.cyclesLeft = 7
}

func (c *CPU) run(instr instruction) {
	low := c.PC & 0xFF
	hi := (c.PC & 0xFF00) >> 8

	if low == 0x0A && hi <= 0x60 {
		c.usingAccumulator = true
	}

	// If the addressing mode crosses the page boundary, 1 extra cycle needed.
	if instr.addressingMode != nil && instr.operate != nil && instr.addressingMode(c) {
		c.cyclesLeft++
	}

	if c.usingAccumulator {
		c.fetched = byte(c.operand)
	} else {
		c.fetched = c.read(c.operand)
	}
	instr.operate(c)

	c.PC++
	c.usingAccumulator = false
}

// It's either this or a big ol switch statement, ¯\_(ツ)_/¯
var instructions = map[byte]instruction{
	0x00: instruction{"BRK", "IMP", (*CPU).brk, (*CPU).imp, 7},
	0x01: instruction{"ORA", "IZX", (*CPU).ora, (*CPU).izx, 6},
	0x05: instruction{"ORA", "ZP0", (*CPU).ora, (*CPU).zp0, 3},
	0x06: instruction{"ASL", "ZP0", (*CPU).asl, (*CPU).zp0, 5},
	0x08: instruction{"PHP", "IMP", (*CPU).php, (*CPU).imp, 3},
	0x09: instruction{"ORA", "IMM", (*CPU).ora, (*CPU).imm, 2},
	0x0A: instruction{"ASL", "ACC", (*CPU).asl, (*CPU).acc, 2},
	0x0D: instruction{"ORA", "ABS", (*CPU).ora, (*CPU).abs, 4},
	0x0E: instruction{"ASL", "ABS", (*CPU).asl, (*CPU).abs, 6},
	0x10: instruction{"BPL", "REL", (*CPU).bpl, (*CPU).rel, 2},
	0x11: instruction{"ORA", "IZY", (*CPU).ora, (*CPU).izy, 5},
	0x15: instruction{"ORA", "ZPX", (*CPU).ora, (*CPU).zpx, 4},
	0x16: instruction{"ASL", "ZPX", (*CPU).asl, (*CPU).zpx, 6},
	0x18: instruction{"CLC", "IMP", (*CPU).clc, (*CPU).imp, 2},
	0x19: instruction{"ORA", "ABY", (*CPU).ora, (*CPU).aby, 4},
	0x1D: instruction{"ORA", "ABX", (*CPU).ora, (*CPU).abx, 4},
	0x1E: instruction{"ASL", "ABX", (*CPU).asl, (*CPU).abx, 7},
	0x20: instruction{"JSR", "ABS", (*CPU).jsr, (*CPU).abs, 6},
	0x21: instruction{"AND", "IZX", (*CPU).and, (*CPU).izx, 6},
	0x24: instruction{"BIT", "ZP0", (*CPU).bit, (*CPU).zp0, 3},
	0x25: instruction{"AND", "ZP0", (*CPU).and, (*CPU).zp0, 3},
	0x26: instruction{"ROL", "ZP0", (*CPU).rol, (*CPU).zp0, 5},
	0x28: instruction{"PLP", "IMP", (*CPU).plp, (*CPU).imp, 4},
	0x29: instruction{"AND", "IMM", (*CPU).and, (*CPU).imm, 2},
	0x2A: instruction{"ROL", "ACC", (*CPU).rol, (*CPU).acc, 2},
	0x2C: instruction{"BIT", "ABS", (*CPU).bit, (*CPU).abs, 4},
	0x2D: instruction{"AND", "ABS", (*CPU).and, (*CPU).abs, 4},
	0x2E: instruction{"ROL", "ABS", (*CPU).rol, (*CPU).abs, 6},
	0x30: instruction{"BMI", "REL", (*CPU).bmi, (*CPU).rel, 2},
	0x31: instruction{"AND", "IZY", (*CPU).and, (*CPU).izy, 5},
	0x35: instruction{"AND", "ZPX", (*CPU).and, (*CPU).zpx, 4},
	0x36: instruction{"ROL", "ZPX", (*CPU).rol, (*CPU).zpx, 6},
	0x38: instruction{"SEC", "IMP", (*CPU).sec, (*CPU).imp, 2},
	0x39: instruction{"AND", "ABY", (*CPU).and, (*CPU).aby, 4},
	0x3D: instruction{"AND", "ABX", (*CPU).and, (*CPU).abx, 4},
	0x3E: instruction{"ROL", "ABX", (*CPU).rol, (*CPU).abx, 7},
	0x40: instruction{"RTI", "IMP", (*CPU).rti, (*CPU).imp, 6},
	0x41: instruction{"EOR", "IZX", (*CPU).eor, (*CPU).izx, 6},
	0x45: instruction{"EOR", "ZP0", (*CPU).eor, (*CPU).zp0, 3},
	0x46: instruction{"LSR", "ZP0", (*CPU).lsr, (*CPU).zp0, 5},
	0x48: instruction{"PHA", "IMP", (*CPU).pha, (*CPU).imp, 3},
	0x49: instruction{"EOR", "IMM", (*CPU).eor, (*CPU).imm, 2},
	0x4A: instruction{"LSR", "ACC", (*CPU).lsr, (*CPU).acc, 2},
	0x4C: instruction{"JMP", "ABS", (*CPU).jmp, (*CPU).abs, 3},
	0x4D: instruction{"EOR", "ABS", (*CPU).eor, (*CPU).abs, 4},
	0x4E: instruction{"LSR", "ABS", (*CPU).lsr, (*CPU).abs, 6},
	0x50: instruction{"BVC", "REL", (*CPU).bvc, (*CPU).rel, 2},
	0x51: instruction{"EOR", "IZY", (*CPU).eor, (*CPU).izy, 5},
	0x55: instruction{"EOR", "ZPX", (*CPU).eor, (*CPU).zpx, 4},
	0x56: instruction{"LSR", "ZPX", (*CPU).lsr, (*CPU).zpx, 6},
	0x58: instruction{"CLI", "IMP", (*CPU).cli, (*CPU).imp, 2},
	0x59: instruction{"EOR", "ABY", (*CPU).eor, (*CPU).aby, 4},
	0x5D: instruction{"EOR", "ABX", (*CPU).eor, (*CPU).abx, 4},
	0x5E: instruction{"LSR", "ABX", (*CPU).lsr, (*CPU).abx, 7},
	0x60: instruction{"RTS", "IMP", (*CPU).rts, (*CPU).imp, 6},
	0x65: instruction{"ADC", "ZP0", (*CPU).adc, (*CPU).zp0, 3},
	0x61: instruction{"ADC", "IZX", (*CPU).adc, (*CPU).izx, 6},
	0x66: instruction{"ROR", "ZP0", (*CPU).ror, (*CPU).zp0, 5},
	0x68: instruction{"PLA", "IMP", (*CPU).pla, (*CPU).imp, 4},
	0x69: instruction{"ADC", "IMM", (*CPU).adc, (*CPU).imm, 2},
	0x6A: instruction{"ROR", "ACC", (*CPU).ror, (*CPU).acc, 2},
	0x6C: instruction{"JMP", "IND", (*CPU).jmp, (*CPU).ind, 5},
	0x6D: instruction{"ADC", "ABS", (*CPU).adc, (*CPU).abs, 4},
	0x6E: instruction{"ROR", "ABS", (*CPU).ror, (*CPU).abs, 6},
	0x70: instruction{"BVS", "REL", (*CPU).bvs, (*CPU).rel, 2},
	0x71: instruction{"ADC", "IZY", (*CPU).adc, (*CPU).izy, 5},
	0x75: instruction{"ADC", "ZPX", (*CPU).adc, (*CPU).zpx, 4},
	0x76: instruction{"ROR", "ZPX", (*CPU).ror, (*CPU).zpx, 6},
	0x78: instruction{"SEI", "IMP", (*CPU).sei, (*CPU).imp, 2},
	0x79: instruction{"ADC", "ABY", (*CPU).adc, (*CPU).aby, 4},
	0x7D: instruction{"ADC", "ABX", (*CPU).adc, (*CPU).abx, 4},
	0x7E: instruction{"ROR", "ABX", (*CPU).ror, (*CPU).abx, 7},
	0x81: instruction{"STA", "IZX", (*CPU).sta, (*CPU).izx, 6},
	0x84: instruction{"STY", "ZP0", (*CPU).sty, (*CPU).zp0, 3},
	0x85: instruction{"STA", "ZP0", (*CPU).sta, (*CPU).zp0, 3},
	0x86: instruction{"STX", "ZP0", (*CPU).stx, (*CPU).zp0, 3},
	0x88: instruction{"DEY", "IMP", (*CPU).dey, (*CPU).imp, 2},
	0x8A: instruction{"TXA", "IMP", (*CPU).txa, (*CPU).imp, 2},
	0x8C: instruction{"STY", "ABS", (*CPU).sty, (*CPU).abs, 4},
	0x8D: instruction{"STA", "ABS", (*CPU).sta, (*CPU).abs, 4},
	0x8E: instruction{"STX", "ABS", (*CPU).stx, (*CPU).abs, 4},
	0x90: instruction{"BCC", "REL", (*CPU).bcc, (*CPU).rel, 2},
	0x91: instruction{"STA", "IZY", (*CPU).sta, (*CPU).izy, 6},
	0x94: instruction{"STY", "ZPX", (*CPU).sty, (*CPU).zpx, 4},
	0x95: instruction{"STA", "ZPX", (*CPU).sta, (*CPU).zpx, 4},
	0x96: instruction{"STX", "ZPY", (*CPU).stx, (*CPU).zpy, 4},
	0x98: instruction{"TYA", "IMP", (*CPU).tya, (*CPU).imp, 2},
	0x99: instruction{"STA", "ABY", (*CPU).sta, (*CPU).aby, 5},
	0x9A: instruction{"TXS", "IMP", (*CPU).txs, (*CPU).imp, 2},
	0x9D: instruction{"STA", "ABX", (*CPU).sta, (*CPU).abx, 5},
	0xA0: instruction{"LDY", "IMM", (*CPU).ldy, (*CPU).imm, 2},
	0xA1: instruction{"LDA", "IZX", (*CPU).lda, (*CPU).izx, 6},
	0xA2: instruction{"LDX", "IMM", (*CPU).ldx, (*CPU).imm, 2},
	0xA4: instruction{"LDY", "ZP0", (*CPU).ldy, (*CPU).zp0, 3},
	0xA5: instruction{"LDA", "ZP0", (*CPU).lda, (*CPU).zp0, 3},
	0xA6: instruction{"LDX", "ZP0", (*CPU).ldx, (*CPU).zp0, 3},
	0xA8: instruction{"TAY", "IMP", (*CPU).tay, (*CPU).imp, 2},
	0xA9: instruction{"LDA", "IMM", (*CPU).lda, (*CPU).imm, 2},
	0xAA: instruction{"TAX", "IMP", (*CPU).tax, (*CPU).imp, 2},
	0xAC: instruction{"LDY", "ABS", (*CPU).ldy, (*CPU).abs, 4},
	0xAD: instruction{"LDA", "ABS", (*CPU).lda, (*CPU).abs, 4},
	0xAE: instruction{"LDX", "ABS", (*CPU).ldx, (*CPU).abs, 4},
	0xB0: instruction{"BCS", "REL", (*CPU).bcs, (*CPU).rel, 2},
	0xB1: instruction{"LDA", "IZY", (*CPU).lda, (*CPU).izy, 5},
	0xB4: instruction{"LDY", "ZPX", (*CPU).ldy, (*CPU).zpx, 4},
	0xB5: instruction{"LDA", "ZPX", (*CPU).lda, (*CPU).zpx, 4},
	0xB6: instruction{"LDX", "ZPY", (*CPU).ldx, (*CPU).zpy, 4},
	0xB8: instruction{"CLV", "IMP", (*CPU).clv, (*CPU).imp, 2},
	0xB9: instruction{"LDA", "ABY", (*CPU).lda, (*CPU).aby, 4},
	0xBA: instruction{"TSX", "IMP", (*CPU).tsx, (*CPU).imp, 2},
	0xBC: instruction{"LDY", "ABX", (*CPU).ldy, (*CPU).abx, 4},
	0xBD: instruction{"LDA", "ABX", (*CPU).lda, (*CPU).abx, 4},
	0xBE: instruction{"LDX", "ABY", (*CPU).ldx, (*CPU).aby, 4},
	0xC0: instruction{"CPY", "IMM", (*CPU).cpy, (*CPU).imm, 2},
	0xC1: instruction{"CMP", "IZX", (*CPU).cmp, (*CPU).izx, 6},
	0xC4: instruction{"CPY", "ZP0", (*CPU).cpy, (*CPU).zp0, 3},
	0xC5: instruction{"CMP", "ZP0", (*CPU).cmp, (*CPU).zp0, 3},
	0xC6: instruction{"DEC", "ZP0", (*CPU).dec, (*CPU).zp0, 5},
	0xC8: instruction{"INY", "IMP", (*CPU).iny, (*CPU).imp, 2},
	0xC9: instruction{"CMP", "IMM", (*CPU).cmp, (*CPU).imm, 2},
	0xCA: instruction{"DEX", "IMP", (*CPU).dex, (*CPU).imp, 2},
	0xCC: instruction{"CPY", "ABS", (*CPU).cpy, (*CPU).abs, 4},
	0xCD: instruction{"CMP", "ABS", (*CPU).cmp, (*CPU).abs, 4},
	0xCE: instruction{"DEC", "ABS", (*CPU).dec, (*CPU).abs, 6},
	0xD0: instruction{"BNE", "REL", (*CPU).bne, (*CPU).rel, 2},
	0xD1: instruction{"CMP", "IZY", (*CPU).cmp, (*CPU).izy, 5},
	0xD5: instruction{"CMP", "ZPX", (*CPU).cmp, (*CPU).zpx, 4},
	0xD6: instruction{"DEC", "ZPX", (*CPU).dec, (*CPU).zpx, 6},
	0xD8: instruction{"CLD", "IMP", (*CPU).cld, (*CPU).imp, 2},
	0xD9: instruction{"CMP", "ABY", (*CPU).cmp, (*CPU).aby, 4},
	0xDD: instruction{"CMP", "ABX", (*CPU).cmp, (*CPU).abx, 4},
	0xDE: instruction{"DEC", "ABX", (*CPU).dec, (*CPU).abx, 7},
	0xE0: instruction{"CPX", "IMM", (*CPU).cpx, (*CPU).imm, 2},
	0xE1: instruction{"SBC", "IZX", (*CPU).sbc, (*CPU).izx, 6},
	0xE4: instruction{"CPX", "ZP0", (*CPU).cpx, (*CPU).zp0, 3},
	0xE5: instruction{"SBC", "ZP0", (*CPU).sbc, (*CPU).zp0, 3},
	0xE6: instruction{"INC", "ZP0", (*CPU).inc, (*CPU).zp0, 5},
	0xE8: instruction{"INX", "IMP", (*CPU).inx, (*CPU).imp, 2},
	0xE9: instruction{"SBC", "IMM", (*CPU).sbc, (*CPU).imm, 2},
	0xEA: instruction{"NOP", "IMP", (*CPU).nop, (*CPU).imp, 2},
	0xEC: instruction{"CPX", "ABS", (*CPU).cpx, (*CPU).abs, 4},
	0xED: instruction{"SBC", "ABS", (*CPU).sbc, (*CPU).abs, 4},
	0xEE: instruction{"INC", "ABS", (*CPU).inc, (*CPU).abs, 6},
	0xF0: instruction{"BEQ", "REL", (*CPU).beq, (*CPU).rel, 2},
	0xF1: instruction{"SBC", "IZY", (*CPU).sbc, (*CPU).izy, 5},
	0xF5: instruction{"SBC", "ZPX", (*CPU).sbc, (*CPU).zpx, 4},
	0xF6: instruction{"INC", "ZPX", (*CPU).inc, (*CPU).zpx, 6},
	0xF8: instruction{"SED", "IMP", (*CPU).sed, (*CPU).imp, 2},
	0xF9: instruction{"SBC", "ABY", (*CPU).sbc, (*CPU).aby, 4},
	0xFD: instruction{"SBC", "ABX", (*CPU).sbc, (*CPU).abx, 4},
	0xFE: instruction{"INC", "ABX", (*CPU).inc, (*CPU).abx, 7},
}

// Addressing Modes
// Implicit
func (c *CPU) imp() bool {
	c.PC++
	return false
}

func (c *CPU) acc() bool {
	c.PC++
	c.operand = uint16(c.A)
	return false
}

// Immediate
func (c *CPU) imm() bool {
	c.operand = uint16(c.PC)
	c.PC++

	return false
}

// Zero Page
func (c *CPU) zp0() bool {
	c.operand = uint16(c.read(c.PC))
	c.PC++

	return false
}

// Zero Page with X Offset
func (c *CPU) zpx() bool {
	c.operand = uint16(c.read(c.PC) + c.X)
	c.PC++

	return false
}

// Zero Page with Y Offset
func (c *CPU) zpy() bool {
	c.operand = uint16(c.read(c.PC) + c.Y)
	c.PC++

	return false
}

// Relative
func (c *CPU) rel() bool {
	c.operand = uint16(c.read(c.PC))
	if c.operand&0x80 == 0x80 {
		c.operand |= 0xFF00
	}
	c.PC++
	c.operand += c.PC

	return false
}

// Absolute
func (c *CPU) abs() bool {
	low := uint16(c.read(c.PC))
	c.PC++
	high := uint16(c.read(c.PC))
	c.PC++

	c.operand = (high << 8) | low

	return false
}

// Absolute with X Offset
func (c *CPU) abx() bool {
	low := uint16(c.read(c.PC))
	c.PC++
	high := uint16(c.read(c.PC))
	c.PC++

	c.operand = (high << 8) | low
	c.operand += uint16(c.X)

	return (c.operand & 0xFF00) != (high << 8)
}

// Absolute with Y Offset
func (c *CPU) aby() bool {
	low := uint16(c.read(c.PC))
	c.PC++
	high := uint16(c.read(c.PC))
	c.PC++

	c.operand = (high << 8) | low
	c.operand += uint16(c.Y)

	return (c.operand & 0xFF00) != (high << 8)
}

// Indirect
func (c *CPU) ind() bool {
	low := uint16(c.read(c.PC))
	c.PC++
	high := uint16(c.read(c.PC))
	c.PC++

	ptr := (high << 8) | low

	if low == 0x00FF { // Page boundary hardware bug
		c.operand = (uint16(c.read(ptr&0xFF00)) << 8) | uint16(c.read(ptr))
	} else {
		c.operand = (uint16(c.read(ptr+1)) << 8) | uint16(c.read(ptr))
	}

	return false
}

// Indirect X
func (c *CPU) izx() bool {
	target := uint16(c.read(c.PC) + c.X)
	c.PC++

	c.operand = (uint16(c.read(target+1)) << 8) | uint16(c.read(target))

	return false
}

// Indirect Y
func (c *CPU) izy() bool {
	target := uint16(c.read(c.PC))
	c.PC++

	low := uint16(c.read(target))
	high := uint16(c.read(target + 1))

	c.operand = ((high << 8) | low) + uint16(c.Y)

	return (c.operand & 0xFF00) != (high << 8)
}

// Operations
func (c *CPU) adc() {
	val := uint16(c.fetched) + uint16(c.A)
	if c.getFlag(C) {
		val++
	}

	c.setFlag(C, val > 255)
	c.setFlag(Z, val&0xFF == 0)
	c.setFlag(N, val&0x80 > 0)
	c.setFlag(V, (^(uint16(c.A)^uint16(c.fetched)&(uint16(c.A)^val)))&0x80 > 0)

	c.A = byte(val)
}

func (c *CPU) and() {
	c.A &= c.fetched

	c.setFlag(N, c.A&0x80 > 0)
	c.setFlag(Z, c.A == 0)
}

func (c *CPU) asl() {
	val := uint16(c.fetched) << 1

	c.setFlag(N, val&0x80 > 0)
	c.setFlag(C, val > 255)
	c.setFlag(Z, val&0xFF == 0)

	if c.usingAccumulator {
		c.A = byte(val)
	} else {
		c.write(c.operand, byte(val))
	}
}

func (c *CPU) bcc() {
	if !c.getFlag(C) {
		c.cyclesLeft++

		c.operand += c.PC
		if c.operand&0xFF00 != c.PC&0xFF00 {
			c.cyclesLeft++
		}

		c.PC = c.operand
	}
}

func (c *CPU) bcs() {
	if c.getFlag(C) {
		c.cyclesLeft++

		c.operand += c.PC
		if c.operand&0xFF00 != c.PC&0xFF00 {
			c.cyclesLeft++
		}

		c.PC = c.operand
	}
}

func (c *CPU) beq() {
	if c.getFlag(Z) {
		c.cyclesLeft++

		c.operand += c.PC
		if c.operand&0xFF00 != c.PC&0xFF00 {
			c.cyclesLeft++
		}

		c.PC = c.operand
	}
}

func (c *CPU) bit() {
	c.setFlag(Z, c.A&c.fetched == 0)
	c.setFlag(N, c.fetched&(1<<7) > 0)
	c.setFlag(V, c.fetched&(1<<6) > 0)
}

func (c *CPU) bmi() {
	if c.getFlag(N) {
		c.cyclesLeft++

		c.operand += c.PC
		if c.operand&0xFF00 != c.PC&0xFF00 {
			c.cyclesLeft++
		}

		c.PC = c.operand
	}
}

func (c *CPU) bne() {
	if !c.getFlag(Z) {
		c.cyclesLeft++

		c.operand += c.PC
		if c.operand&0xFF00 != c.PC&0xFF00 {
			c.cyclesLeft++
		}

		c.PC = c.operand
	}
}

func (c *CPU) bpl() {
	if !c.getFlag(N) {
		c.cyclesLeft++

		c.operand += c.PC
		if c.operand&0xFF00 != c.PC&0xFF00 {
			c.cyclesLeft++
		}

		c.PC = c.operand
	}
}

func (c *CPU) brk() {
	c.PC++

	c.setFlag(I, true)
	c.write(0x100+uint16(c.S), byte(c.PC>>8))
	c.S--
	c.write(0x100+uint16(c.S), byte(c.PC))
	c.S--

	c.setFlag(B, true)
	c.write(0x100+uint16(c.S), c.P)
	c.S--
	c.setFlag(B, false)

	c.PC = uint16(c.read(0xFFFE)) | (uint16(c.read(0xFFFF)) << 8)
}

func (c *CPU) bvc() {
	if !c.getFlag(V) {
		c.cyclesLeft++

		c.operand += c.PC
		if c.operand&0xFF00 != c.PC&0xFF00 {
			c.cyclesLeft++
		}

		c.PC = c.operand
	}
}

func (c *CPU) bvs() {
	if c.getFlag(V) {
		c.cyclesLeft++

		c.operand += c.PC
		if c.operand&0xFF00 != c.PC&0xFF00 {
			c.cyclesLeft++
		}

		c.PC = c.operand
	}
}

func (c *CPU) clc() {
	c.setFlag(C, false)
}

func (c *CPU) cld() {
	c.setFlag(D, false)
}

func (c *CPU) cli() {
	c.setFlag(I, false)
}

func (c *CPU) clv() {
	c.setFlag(V, false)
}

func (c *CPU) cmp() {
	val := c.A - c.fetched

	c.setFlag(C, c.A >= c.fetched)
	c.setFlag(N, val&0x80 > 0)
	c.setFlag(Z, val == 0)
}

func (c *CPU) cpx() {
	val := c.X - c.fetched

	c.setFlag(C, c.X >= c.fetched)
	c.setFlag(N, val&0x80 > 0)
	c.setFlag(Z, val == 0)
}

func (c *CPU) cpy() {
	val := c.Y - c.fetched

	c.setFlag(C, c.Y >= c.fetched)
	c.setFlag(N, val&0x80 > 0)
	c.setFlag(Z, val == 0)
}

func (c *CPU) dec() {
	val := c.fetched - 1

	c.write(c.operand, val)
	c.setFlag(Z, val == 0)
	c.setFlag(N, val&0x80 > 0)
}

func (c *CPU) dex() {
	c.X--

	c.setFlag(Z, c.X == 0)
	c.setFlag(N, c.X&0x80 > 0)
}

func (c *CPU) dey() {
	c.Y--

	c.setFlag(Z, c.Y == 0)
	c.setFlag(N, c.Y&0x80 > 0)
}

func (c *CPU) eor() {
	c.A ^= c.fetched

	c.setFlag(N, c.A&0x80 > 0)
	c.setFlag(Z, c.A == 0)
}

func (c *CPU) inc() {
	val := c.fetched + 1

	c.write(c.operand, val)
	c.setFlag(N, val&0x80 > 0)
	c.setFlag(Z, val == 0)
}

func (c *CPU) inx() {
	c.X++

	c.setFlag(N, c.X&0x80 > 0)
	c.setFlag(Z, c.X == 0)
}

func (c *CPU) iny() {
	c.Y++

	c.setFlag(N, c.Y&0x80 > 0)
	c.setFlag(Z, c.Y == 0)
}

func (c *CPU) jmp() {
	c.PC = c.operand
}

func (c *CPU) jsr() {
	c.PC--

	c.write(0x100+uint16(c.S), byte(c.PC>>8))
	c.S--
	c.write(0x100+uint16(c.S), byte(c.PC))
	c.S--

	c.PC = c.operand
}

func (c *CPU) lda() {
	c.A = c.fetched

	c.setFlag(N, c.A&0x80 > 0)
	c.setFlag(Z, c.A == 0)
}

func (c *CPU) ldx() {
	c.X = c.fetched

	c.setFlag(N, c.X&0x80 > 0)
	c.setFlag(Z, c.X == 0)
}

func (c *CPU) ldy() {
	c.Y = c.fetched

	c.setFlag(N, c.Y&0x80 > 0)
	c.setFlag(Z, c.Y == 0)
}

func (c *CPU) lsr() {
	val := c.fetched >> 1

	c.setFlag(N, false)
	c.setFlag(C, c.fetched&1 == 1)
	c.setFlag(Z, val == 0)

	if c.usingAccumulator {
		c.A = val
	} else {
		c.write(c.operand, val)
	}
}

func (c *CPU) nop() {}

func (c *CPU) ora() {
	c.A |= c.fetched

	c.setFlag(N, c.A&0x80 > 0)
	c.setFlag(Z, c.A == 0)
}

func (c *CPU) pha() {
	c.write(0x100+uint16(c.S), c.A)
	c.S--
}

func (c *CPU) php() {
	c.setFlag(B, true)
	c.setFlag(R, true)
	c.write(0x100+uint16(c.S), c.P)
	c.setFlag(B, false)
	c.setFlag(R, false)
	c.S--
}

func (c *CPU) pla() {
	c.S++
	c.A = c.read(0x100 + uint16(c.S))

	c.setFlag(Z, c.A == 0)
	c.setFlag(N, c.A&0x80 > 0)
}

func (c *CPU) plp() {
	c.S++
	c.P = c.read(0x100 + uint16(c.S))
	c.setFlag(R, true)
}

func (c *CPU) rol() {
	val := (c.fetched << 1)
	if c.getFlag(C) {
		val++
	}

	c.setFlag(N, val&0x80 > 0)
	c.setFlag(Z, val == 0)
	c.setFlag(C, c.fetched>>7 == 1)

	if c.usingAccumulator {
		c.A = val
	} else {
		c.write(c.operand, val)
	}
}

func (c *CPU) ror() {
	val := (c.fetched >> 1)
	if c.getFlag(C) {
		val |= (1 << 7)
	}

	c.setFlag(N, val&0x80 > 0)
	c.setFlag(Z, val == 0)
	c.setFlag(C, c.fetched&1 == 1)

	if c.usingAccumulator {
		c.A = val
	} else {
		c.write(c.operand, val)
	}
}

func (c *CPU) rti() {
	c.S++
	c.P = c.read(0x100 + uint16(c.S))
	c.setFlag(R, false)
	c.setFlag(B, false)

	c.S++
	low := c.read(0x100 + uint16(c.S))
	c.S++
	hi := c.read(0x100 + uint16(c.S))

	c.PC = uint16(low) | (uint16(hi) << 8)
}

func (c *CPU) rts() {
	c.S++
	low := c.read(0x100 + uint16(c.S))
	c.S++
	hi := c.read(0x100 + uint16(c.S))

	c.PC = (uint16(low) | (uint16(hi) << 8)) + 1
}

func (c *CPU) sbc() {
	c.fetched = ^c.fetched
	c.adc()
}

func (c *CPU) sec() {
	c.setFlag(C, true)
}

func (c *CPU) sed() {
	c.setFlag(D, true)
}

func (c *CPU) sei() {
	c.setFlag(I, true)
}

func (c *CPU) sta() {
	c.write(c.operand, c.A)
}

func (c *CPU) stx() {
	c.write(c.operand, c.X)
}

func (c *CPU) sty() {
	c.write(c.operand, c.Y)
}

func (c *CPU) tax() {
	c.X = c.A

	c.setFlag(Z, c.X == 0)
	c.setFlag(N, c.X&0x80 > 0)
}

func (c *CPU) tay() {
	c.Y = c.A

	c.setFlag(Z, c.Y == 0)
	c.setFlag(N, c.Y&0x80 > 0)
}

func (c *CPU) tsx() {
	c.X = c.S

	c.setFlag(Z, c.X == 0)
	c.setFlag(N, c.X&0x80 > 0)
}

func (c *CPU) txa() {
	c.A = c.X

	c.setFlag(Z, c.A == 0)
	c.setFlag(N, c.A&0x80 > 0)
}

func (c *CPU) txs() {
	c.S = c.X
}

func (c *CPU) tya() {
	c.A = c.Y

	c.setFlag(Z, c.A == 0)
	c.setFlag(N, c.A&0x80 > 0)
}
