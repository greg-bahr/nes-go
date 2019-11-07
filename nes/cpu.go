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
	operand          uint16 // operand for current instruction
	fetched          byte   // fetched data from operand
	usingAccumulator bool
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

	c.cyclesLeft = 8
}

// Interrupt Request
func (c *CPU) irq() {
	if !c.getFlag(I) {

	}
}

// Nonmaskable Interrupt
func (c *CPU) nmi() {

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
	0x00: instruction{"BRK", (*CPU).brk, (*CPU).imp, 7},
	0x01: instruction{"ORA", (*CPU).ora, (*CPU).izx, 6},
	0x05: instruction{"ORA", (*CPU).ora, (*CPU).zp0, 3},
	0x06: instruction{"ASL", (*CPU).asl, (*CPU).zp0, 5},
	0x08: instruction{"PHP", (*CPU).php, (*CPU).imp, 3},
	0x09: instruction{"ORA", (*CPU).ora, (*CPU).imm, 2},
	0x0A: instruction{"ASL", (*CPU).asl, (*CPU).acc, 2},
	0x0D: instruction{"ORA", (*CPU).ora, (*CPU).abs, 4},
	0x0E: instruction{"ASL", (*CPU).asl, (*CPU).abs, 6},
	0x10: instruction{"BPL", (*CPU).bpl, (*CPU).rel, 2},
	0x11: instruction{"ORA", (*CPU).ora, (*CPU).izy, 5},
	0x15: instruction{"ORA", (*CPU).ora, (*CPU).zpx, 4},
	0x16: instruction{"ASL", (*CPU).asl, (*CPU).zpx, 6},
	0x18: instruction{"CLC", (*CPU).clc, (*CPU).imp, 2},
	0x19: instruction{"ORA", (*CPU).ora, (*CPU).aby, 4},
	0x1D: instruction{"ORA", (*CPU).ora, (*CPU).abx, 4},
	0x1E: instruction{"ASL", (*CPU).asl, (*CPU).abx, 7},
	0x20: instruction{"JSR", (*CPU).jsr, (*CPU).abs, 6},
	0x21: instruction{"AND", (*CPU).and, (*CPU).izx, 6},
	0x24: instruction{"BIT", (*CPU).bit, (*CPU).zp0, 3},
	0x25: instruction{"AND", (*CPU).and, (*CPU).zp0, 3},
	0x26: instruction{"ROL", (*CPU).rol, (*CPU).zp0, 5},
	0x28: instruction{"PLP", (*CPU).plp, (*CPU).imp, 4},
	0x29: instruction{"AND", (*CPU).and, (*CPU).imm, 2},
	0x2A: instruction{"ROL", (*CPU).rol, (*CPU).acc, 2},
	0x2C: instruction{"BIT", (*CPU).bit, (*CPU).abs, 4},
	0x2D: instruction{"AND", (*CPU).and, (*CPU).abs, 4},
	0x2E: instruction{"ROL", (*CPU).rol, (*CPU).abs, 6},
	0x30: instruction{"BMI", (*CPU).bmi, (*CPU).rel, 2},
	0x31: instruction{"AND", (*CPU).and, (*CPU).izy, 5},
	0x35: instruction{"AND", (*CPU).and, (*CPU).zpx, 4},
	0x36: instruction{"ROL", (*CPU).rol, (*CPU).zpx, 6},
	0x38: instruction{"SEC", (*CPU).sec, (*CPU).imp, 2},
	0x39: instruction{"AND", (*CPU).and, (*CPU).aby, 4},
	0x3D: instruction{"AND", (*CPU).and, (*CPU).abx, 4},
	0x3E: instruction{"ROL", (*CPU).rol, (*CPU).abx, 7},
	0x40: instruction{"RTI", (*CPU).rti, (*CPU).imp, 6},
	0x41: instruction{"EOR", (*CPU).eor, (*CPU).izx, 6},
	0x45: instruction{"EOR", (*CPU).eor, (*CPU).zp0, 3},
	0x46: instruction{"LSR", (*CPU).lsr, (*CPU).zp0, 5},
	0x48: instruction{"PHA", (*CPU).pha, (*CPU).imp, 3},
	0x49: instruction{"EOR", (*CPU).eor, (*CPU).imm, 2},
	0x4A: instruction{"LSR", (*CPU).lsr, (*CPU).acc, 2},
	0x4C: instruction{"JMP", (*CPU).jmp, (*CPU).abs, 3},
	0x4D: instruction{"EOR", (*CPU).eor, (*CPU).abs, 4},
	0x4E: instruction{"LSR", (*CPU).lsr, (*CPU).abs, 6},
	0x50: instruction{"BVC", (*CPU).bvc, (*CPU).rel, 2},
	0x51: instruction{"EOR", (*CPU).eor, (*CPU).izy, 5},
	0x55: instruction{"EOR", (*CPU).eor, (*CPU).zpx, 4},
	0x56: instruction{"LSR", (*CPU).lsr, (*CPU).zpx, 6},
	0x58: instruction{"CLI", (*CPU).cli, (*CPU).imp, 2},
	0x59: instruction{"EOR", (*CPU).eor, (*CPU).aby, 4},
	0x5D: instruction{"EOR", (*CPU).eor, (*CPU).abx, 4},
	0x5E: instruction{"LSR", (*CPU).lsr, (*CPU).abx, 7},
	0x60: instruction{"RTS", (*CPU).rts, (*CPU).imp, 6},
	0x61: instruction{"ADC", (*CPU).adc, (*CPU).izx, 6},
	0x65: instruction{"ADC", (*CPU).adc, (*CPU).zp0, 3},
	0x66: instruction{"ROR", (*CPU).ror, (*CPU).zp0, 5},
	0x68: instruction{"PLA", (*CPU).pla, (*CPU).imp, 4},
	0x69: instruction{"ADC", (*CPU).adc, (*CPU).imm, 2},
	0x6A: instruction{"ROR", (*CPU).ror, (*CPU).acc, 2},
	0x6C: instruction{"JMP", (*CPU).jmp, (*CPU).ind, 5},
	0x6D: instruction{"ADC", (*CPU).adc, (*CPU).abs, 4},
	0x6E: instruction{"ROR", (*CPU).ror, (*CPU).abs, 6},
	0x70: instruction{"BVS", (*CPU).bvs, (*CPU).rel, 2},
	0x71: instruction{"ADC", (*CPU).adc, (*CPU).izy, 5},
	0x75: instruction{"ADC", (*CPU).adc, (*CPU).zpx, 4},
	0x76: instruction{"ROR", (*CPU).ror, (*CPU).zpx, 6},
	0x78: instruction{"SEI", (*CPU).sei, (*CPU).imp, 2},
	0x79: instruction{"ADC", (*CPU).adc, (*CPU).aby, 4},
	0x7D: instruction{"ADC", (*CPU).adc, (*CPU).abx, 4},
	0x7E: instruction{"ROR", (*CPU).ror, (*CPU).abx, 7},
	0x81: instruction{"STA", (*CPU).sta, (*CPU).izx, 6},
	0x84: instruction{"STY", (*CPU).sty, (*CPU).zp0, 3},
	0x85: instruction{"STA", (*CPU).sta, (*CPU).zp0, 3},
	0x86: instruction{"STX", (*CPU).stx, (*CPU).zp0, 3},
	0x88: instruction{"DEY", (*CPU).dey, (*CPU).imp, 2},
	0x8A: instruction{"TXA", (*CPU).txa, (*CPU).imp, 2},
	0x8C: instruction{"STY", (*CPU).sty, (*CPU).abs, 4},
	0x8D: instruction{"STA", (*CPU).sta, (*CPU).abs, 4},
	0x8E: instruction{"STX", (*CPU).stx, (*CPU).abs, 4},
	0x90: instruction{"BCC", (*CPU).bcc, (*CPU).rel, 2},
	0x91: instruction{"STA", (*CPU).sta, (*CPU).izy, 6},
	0x94: instruction{"STY", (*CPU).sty, (*CPU).zpx, 4},
	0x95: instruction{"STA", (*CPU).sta, (*CPU).zpx, 4},
	0x96: instruction{"STX", (*CPU).stx, (*CPU).zpy, 4},
	0x98: instruction{"TYA", (*CPU).tya, (*CPU).imp, 2},
	0x99: instruction{"STA", (*CPU).sta, (*CPU).aby, 5},
	0x9A: instruction{"TXS", (*CPU).txs, (*CPU).imp, 2},
	0x9D: instruction{"STA", (*CPU).sta, (*CPU).abx, 5},
	0xA0: instruction{"LDY", (*CPU).ldy, (*CPU).imm, 2},
	0xA1: instruction{"LDA", (*CPU).lda, (*CPU).izx, 6},
	0xA2: instruction{"LDX", (*CPU).ldx, (*CPU).imm, 2},
	0xA4: instruction{"LDY", (*CPU).ldy, (*CPU).zp0, 3},
	0xA5: instruction{"LDA", (*CPU).lda, (*CPU).zp0, 3},
	0xA6: instruction{"LDX", (*CPU).ldx, (*CPU).zp0, 3},
	0xA8: instruction{"TAY", (*CPU).tay, (*CPU).imp, 2},
	0xA9: instruction{"LDA", (*CPU).lda, (*CPU).imm, 2},
	0xAA: instruction{"TAX", (*CPU).tax, (*CPU).imp, 2},
	0xAC: instruction{"LDY", (*CPU).ldy, (*CPU).abs, 4},
	0xAD: instruction{"LDA", (*CPU).lda, (*CPU).abs, 4},
	0xAE: instruction{"LDX", (*CPU).ldx, (*CPU).abs, 4},
	0xB0: instruction{"BCS", (*CPU).bcs, (*CPU).rel, 2},
	0xB1: instruction{"LDA", (*CPU).lda, (*CPU).izy, 5},
	0xB4: instruction{"LDY", (*CPU).ldy, (*CPU).zpx, 4},
	0xB5: instruction{"LDA", (*CPU).lda, (*CPU).zpx, 4},
	0xB6: instruction{"LDX", (*CPU).ldx, (*CPU).zpy, 4},
	0xB8: instruction{"CLV", (*CPU).clv, (*CPU).imp, 2},
	0xB9: instruction{"LDA", (*CPU).lda, (*CPU).aby, 4},
	0xBA: instruction{"TSX", (*CPU).tsx, (*CPU).imp, 2},
	0xBC: instruction{"LDY", (*CPU).ldy, (*CPU).abx, 4},
	0xBD: instruction{"LDA", (*CPU).lda, (*CPU).abx, 4},
	0xBE: instruction{"LDX", (*CPU).ldx, (*CPU).aby, 4},
	0xC0: instruction{"CPY", (*CPU).cpy, (*CPU).imm, 2},
	0xC1: instruction{"CMP", (*CPU).cmp, (*CPU).izx, 6},
	0xC4: instruction{"CPY", (*CPU).cpy, (*CPU).zp0, 3},
	0xC5: instruction{"CMP", (*CPU).cmp, (*CPU).zp0, 3},
	0xC6: instruction{"DEC", (*CPU).dec, (*CPU).zp0, 5},
	0xC8: instruction{"INY", (*CPU).iny, (*CPU).imp, 2},
	0xC9: instruction{"CMP", (*CPU).cmp, (*CPU).imm, 2},
	0xCA: instruction{"DEX", (*CPU).dex, (*CPU).imp, 2},
	0xCC: instruction{"CPY", (*CPU).cpy, (*CPU).abs, 4},
	0xCD: instruction{"CMP", (*CPU).cmp, (*CPU).abs, 4},
	0xCE: instruction{"DEC", (*CPU).dec, (*CPU).abs, 6},
	0xD0: instruction{"BNE", (*CPU).bne, (*CPU).rel, 2},
	0xD1: instruction{"CMP", (*CPU).cmp, (*CPU).izy, 5},
	0xD5: instruction{"CMP", (*CPU).cmp, (*CPU).zpx, 4},
	0xD6: instruction{"DEC", (*CPU).dec, (*CPU).zpx, 6},
	0xD8: instruction{"CLD", (*CPU).cld, (*CPU).imp, 2},
	0xD9: instruction{"CMP", (*CPU).cmp, (*CPU).aby, 4},
	0xDD: instruction{"CMP", (*CPU).cmp, (*CPU).abx, 4},
	0xDE: instruction{"DEC", (*CPU).dec, (*CPU).abx, 7},
	0xE0: instruction{"CPX", (*CPU).cpx, (*CPU).imm, 2},
	0xE1: instruction{"SBC", (*CPU).sbc, (*CPU).izx, 6},
	0xE4: instruction{"CPX", (*CPU).cpx, (*CPU).zp0, 3},
	0xE5: instruction{"SBC", (*CPU).sbc, (*CPU).zp0, 3},
	0xE6: instruction{"INC", (*CPU).inc, (*CPU).zp0, 5},
	0xE8: instruction{"INX", (*CPU).inx, (*CPU).imp, 2},
	0xE9: instruction{"SBC", (*CPU).sbc, (*CPU).imm, 2},
	0xEA: instruction{"NOP", (*CPU).nop, (*CPU).imp, 2},
	0xEC: instruction{"CPX", (*CPU).cpx, (*CPU).abs, 4},
	0xED: instruction{"SBC", (*CPU).sbc, (*CPU).abs, 4},
	0xEE: instruction{"INC", (*CPU).inc, (*CPU).abs, 6},
	0xF0: instruction{"BEQ", (*CPU).beq, (*CPU).rel, 2},
	0xF1: instruction{"SBC", (*CPU).sbc, (*CPU).izy, 5},
	0xF5: instruction{"SBC", (*CPU).sbc, (*CPU).zpx, 4},
	0xF6: instruction{"INC", (*CPU).inc, (*CPU).zpx, 6},
	0xF8: instruction{"SED", (*CPU).sed, (*CPU).imp, 2},
	0xF9: instruction{"SBC", (*CPU).sbc, (*CPU).aby, 4},
	0xFD: instruction{"SBC", (*CPU).sbc, (*CPU).abx, 4},
	0xFE: instruction{"INC", (*CPU).inc, (*CPU).abx, 7},
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

	c.A = byte(val & 0xFF)
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
		c.A = byte(val & 0xFF)
	} else {
		c.write(c.operand, byte(val&0xFF))
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
	c.fetched = ^c.fetched
	c.adc()
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
