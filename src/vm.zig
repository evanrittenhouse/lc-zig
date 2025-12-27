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

    fn exec_instr(self: *Registers, instr: u16) void {
        const opcode = instr >> 12 & 0xF;

        const dst = switch (opcode) {
            // TODO: what dst should `branch` yield? Have to read spec.
            0b0000 => self.branch(instr),
            0b0001 => self.add(instr),
            0b0101 => self.b_and(instr),
            0b1010 => self.load_indirect(instr),
            0b1001 => self.b_not(instr),
            _ => {},
        };

        self.update_flags(dst);
    }

    // TODO: can refactor instruction handling:
    // 1. parse opcode
    // 2. switch on opcode, return dst register
    // 3. update_flags(dst)

    // Add
    // 15-12 | 11-9 | 8-6 | 5 | 4-3 | 2-0 |
    // ----------------------------------
    //  0001 |  DR  | SR1 | 0 | 00  | SR2 | = register mode.  store sum of numbers stored in SR1 and SR2 in DR
    //  0001 |  DR  | SR1 | 1 |   imm 5   | = immediate mode. same as register mode, but second number is embedded in the instruction
    //
    // in immediate mode, the second operand is obtained by sign-extending the imm5 field to 16 bits.
    fn add(self: *Registers, instr: u16) void {
        const dst = dst_register(instr);
        const src1 = (instr >> 6) & 0x7;
        const immediate_flag = (instr >> 5) & 0x1 == 1;

        if (immediate_flag) {
            // 0x1F == 31, and imm5 is 5 bits wide
            const imm5 = sign_extend(@as(u16, instr & 0x1F), 5);
            self.reg[dst] = self.reg[src1] + imm5;
        } else {
            const src2 = instr & 0x7;
            self.reg[dst] = self.reg[src1] + self.reg[src2];
        }
    }

    // Bitwise-AND
    // 15-12 | 11-9 | 8-6 | 5 | 4-3 | 2-0 |
    // ----------------------------------
    //  0101 |  DR  | SR1 | 0 | 00  | SR2 | = register mode.  store bitwise-AND of contents of SR1 and SR2 in DR
    //  0101 |  DR  | SR1 | 1 |   imm 5   | = immediate mode. same as register mode, but second operand is in imm5
    fn b_and(self: *Registers, instr: u16) void {
        const dst = dst_register(instr);
        const src1 = (instr >> 6) & 0x7;
        const immediate_flag = (instr >> 5) & 0x1 == 1;

        if (immediate_flag) {
            const imm5 = sign_extend(@as(u16, instr & 0x1F), 5);
            self.reg[dst] = self.reg[src1] & imm5;
        } else {
            const src2 = instr & 0x7;
            self.reg[dst] = self.reg[src1] & self.reg[src2];
        }
    }

    // Branch
    // 15-12 | 11 | 10 | 9 |   8-0  |
    // ------------------------------
    //  0000 | n  | z  | p |PCoffset9
    //
    // Check condition codes and branch.
    // If 11 is set, N is tested - if 11 is not set, N is not tested.
    // If 10 is set, Z is tested - if 10 is not set, Z is not tested.
    // EX: BRzp LOOP -> branch to LOOP if condition was zero or positive.
    // EX: BR   NEXT -> branch to NEXT, unconditionally.
    //
    // If any of the tested condition codes are set, branch to the location specified
    // by adding sign-extended PCoffset9 to the incremented PC.
    fn branch(self: *Registers, instr: u16) void {
        const test_pos = (instr >> 11) & 0x1 == 1;
        const test_zero = (instr >> 10) & 0x1 == 1;
        const test_neg = (instr >> 9) & 0x1 == 1;

        const cond = self.reg[@intFromEnum(Register.COND)];
    }

    // Jump

    // Jump to Subroutine

    // Load

    // LDI
    // 15-12 | 11-9 |    8-0  |
    // ------------------------
    //  1010 |  DR  | PCoffset9
    //
    // An address is computed by sign-extending PCoffset9 and adding this value to the incremented PC.
    // The value at this address is loaded into DR.
    fn load_indirect(_: *Registers, instr: u16) void {
        // const dst = dst_register(instr);

        _ = sign_extend(instr & 0x1FF, 9);
        // self.reg[dst] = mem_read(mem_read(self.reg[@intFromEnum(Register.PC) + pc_offset]));
    }

    // Load Base + offset

    // Load effective address

    // Not
    // 15-12 | 11-9 | 8-6 | 5 | 4-0  |
    // -------------------------------
    //  1001 |  DR  | SRC | 1 | 1111 |
    //  Bitwise-complement of contents of SRC are stored in DR
    fn b_not(self: *Registers, instr: u16) void {
        const dst = dst_register(instr);
        const src = (instr >> 6) & 0x7;

        self.reg[dst] = ~self.reg[src];
    }

    // Return from subroutine

    // Return from interrupt

    // Store

    // Store indirect

    // Store Base + offset

    // Trap

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

inline fn dst_register(instr: u16) u16 {
    return (instr >> 9) & 0x7;
}

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

const std = @import("std");

// --- TESTS ---
const expect = std.testing.expect;
test "add_stored" {
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
//
// test "add_negative_immediate" {}

test "add_immediate" {
    var reg = Registers.init();
    reg.reg[0] = 1;

    reg.add(0b0001_001_000_1_00001);

    try std.testing.expect(reg.reg[1] == 2);
}

// test "ldi"

test "b_and_immediate" {
    var reg = Registers.init();
    reg.reg[0] = 1;

    reg.b_and(0b0101_001_000_1_00001);

    try std.testing.expect(reg.reg[1] == 1);
}

test "b_and_stored" {
    var reg = Registers.init();
    reg.reg[0] = 1;
    // reg.reg[1] is implicitly 0

    reg.b_and(0b0101_010_000_0_00_001);

    try std.testing.expect(reg.reg[2] == 0);
}

test "b_not" {
    var reg = Registers.init();
    reg.reg[0] = 1;

    reg.b_not(0b1001_001_000_1_1111);

    try std.testing.expect(reg.reg[1] == 0);
}
