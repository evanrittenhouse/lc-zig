pub const MEMORY_MAX = 1 << 16;

// Keep track of all registers in the VM.
pub const Registers = struct {
    reg: [NumRegisters]u16,

    pub fn init() Registers {
        // Initialize the condition flag and program counter. Set the condition
        // flag to 0 since it must be set to something.
        var reg = std.mem.zeroes([NumRegisters]u16);
        reg[@intFromEnum(Register.COND)] = @intFromEnum(Sign.ZRO);
        reg[@intFromEnum(Register.PC)] = 0x3000;

        return Registers{ .reg = reg };
    }

    pub fn inc_program_counter(self: *Registers) void {
        self.reg[@intFromEnum(Register.PC)] += 1;
    }

    // Update a given register in the list with the provided value
    pub fn update_register(self: *Registers, reg: Register, value: u16) void {
        self.reg[@intFromEnum(reg)] = value;
    }

    // Add
    // 15-12 | 11-9 | 8-6 | 5 | 4-3 | 2-0 |
    // ----------------------------------
    //  0001 |  DR  | SR1 | 0 | 00  | SR2 | = register mode.  store sum of numbers stored in SR1 and SR2 in DR
    //  0001 |  DR  | SR1 | 1 |   imm 5   | = immediate mode. same as register mode, but second number is embedded in the instruction
    //
    // in immediate mode, the second operand is obtained by sign-extending the imm5 field to 16 bits.
    fn add(self: *Registers, instr: u16) void {
        const dst = (instr >> 9) & 0x7;
        const src1 = (instr >> 6) & 0x7;
        const immediate_flag = (instr >> 5) & 0x1 == 1;

        if (immediate_flag) {
            // 0x1F == 31, and imm5 is 5 bits wide
            const imm5 = sign_extend(@as(u16, instr & 0x1F), 5);
            std.debug.print("dst={d}, val={d}, src1={d}, imm5={d}\n", .{ dst, src1, self.reg[src1], imm5 });
            self.reg[dst] = self.reg[src1] + imm5;
        } else {
            const src2 = instr & 0x7;
            self.reg[dst] = self.reg[src1] + self.reg[src2];
        }

        self.update_flags(dst);
    }

    // Update the COND register based on the number stored in index `r`.
    fn update_flags(self: *Registers, index: u16) void {
        var value = self.reg[@intFromEnum(Register.COND)];

        if (self.reg[index] == 0) {
            value = @intFromEnum(Sign.ZRO);
        } else if (((self.reg[index] >> 15) & 0x1) == 1) {
            // 1 in the left-most bit indicates negative
            value = @intFromEnum(Sign.NEG);
        } else {
            value = @intFromEnum(Sign.POS);
        }
    }
};

pub const NumRegisters = @typeInfo(Register).@"enum".fields.len;

// Register list.
pub const Register = enum {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8, // last general-purpose register
    PC, // program counter
    COND,
    COUNT,
};

// Sign of the previous calculation.
pub const Sign = enum(u16) {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,
};

// Instruction set for the LC-3 architecture.
pub const Opcode = enum {
    BR, // branch
    ADD, // add
    LD, // load
    ST, // store
    JSR, // jump register
    AND, // bitwise and
    LDR, // load register
    STR, // store register
    RTI, // unused
    NOT, // bitwise not
    LDI, // load indirect
    STI, // store indirect
    JMP, // jump
    RES, // reserved (unused)
    LEA, // load effective address
    TRAP, // execute trap
};

// imm5 is only 5 bits, but has to be added to a 16-bit number.
//
// For positive numbers, just pad with 0.
// For negative numbers, pad with 1's. (e.g. -1 in 5 bits is 1 1111).
//
// https://en.wikipedia.org/wiki/Two%27s_complement.
// Basically, MSB is used to indicate sign. Take the bit complement, then add one.
// For example, 6 = 110, with an extra bit for sign it's 0110.
// 1. 6 = 110, with sign -> 0110
// 2. Flip all bits      -> 1001
// 3. Add 1              -> 1010
// TODO: how do we get around the u4 here?
fn sign_extend(x: u16, bit_count: u4) u16 {
    // TODO: also should remove this hideous copy, but x is a const?
    var inner = x;
    if (((inner >> (bit_count - 1)) & 1) == 1) {
        // If a negative number (MSB is 1), pad with 1's to preserve two's complement
        inner |= (@as(u16, 0xFFFF) << bit_count);
    }

    return inner;
}

const std = @import("std");

const expect = std.testing.expect;
test "add_non_immediate" {
    var reg = Registers.init();
    reg.reg[0] = 1;
    reg.reg[1] = 2;

    reg.add(0b0001_010_000_0_00_001);

    try std.testing.expect(reg.reg[2] == 3);
}

// test "add_negative_non_immediate" {
//     var reg = Registers.init();
//     reg.reg[0] = 1;
//     reg.reg[1] = 0b110;

//     reg.add(0b0001_010_000_0_00_001);

//     try std.testing.expect(reg.reg[2] == -1);
// }

test "add_immediate" {
    var reg = Registers.init();
    reg.reg[0] = 1;

    reg.add(0b0001_001_000_1_00001);

    try std.testing.expect(reg.reg[1] == 2);
}

// TODO: negative tests
