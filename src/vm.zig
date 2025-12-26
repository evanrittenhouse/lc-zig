pub const MEMORY_MAX = 1 << 16;
pub const MEMORY: [MEMORY_MAX]u16 = undefined;

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

pub const Condition = enum { POS, ZRO, NEG };

// TODO: can we clean this up?
pub const REGISTERS: [10]u16 = undefined;

const Opcode = enum {
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
