pub const MEMORY_MAX = 1 << 16;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub const Vm = struct {
    reg: [NUM_REGISTERS]u16,
    mem: [MEMORY_MAX]u16,
    running: bool,

    pub fn init() Vm {
        var reg = std.mem.zeroes([NUM_REGISTERS]u16);
        const mem = std.mem.zeroes([MEMORY_MAX]u16);

        // Initialize the condition flag and program counter. Set the condition
        // flag to 0 since it must be set to something.
        reg[Register.cond()] = @intFromEnum(Sign.ZRO);
        reg[Register.pc()] = 0x3000;

        return Vm{ .reg = reg, .mem = mem, .running = true };
    }

    pub fn run(self: *Vm, path: []const u8) !void {
        if (!(try self.load_file(path))) {
            std.debug.print("Couldn't load file at path: {s}\n", .{path});
            return;
        }

        while (self.running) {
            const instr = self.mem_read(self.reg[Register.pc()]);
            self.reg[Register.pc()] += 1;

            const opcode = instr >> 12;
            // std.debug.print("instr={d}, opcode = {d}, pc={d}\n", .{ instr, opcode, self.reg[Register.pc()] });
            const dst = switch (opcode) {
                0b0001 => self.add(instr),
                0b0101 => self.b_and(instr),
                0b0000 => self.branch(instr),
                0b1100 => self.jump(instr),
                0b0100 => self.jump_sr(instr),
                0b0010 => self.load(instr),
                0b1010 => self.load_indirect(instr),
                0b0110 => self.load_base_offset(instr),
                0b1110 => self.load_effective(instr),
                0b1001 => self.b_not(instr),
                0b0011 => self.store(instr),
                0b1011 => self.store_indirect(instr),
                0b0111 => self.store_base_offset(instr),
                0b1111 => self.trap(instr),
                // Illegal opcode.
                0b1101 => std.process.exit(1),
                else => null,
            };

            if (dst) |register| {
                self.update_flags(register);
            }
        }
    }

    fn load_file(self: *Vm, path: []const u8) !bool {
        const file = try std.fs.openFileAbsolute(path, .{
            .mode = std.fs.File.OpenMode.read_only,
        });
        defer file.close();

        // Thanks to https://github.com/mdaverde/lc3vm-zig/blob/main/src/main.zig!
        const reader = std.fs.File.reader(file);
        const u16_max = std.math.maxInt(u16);
        const origin = try reader.readInt(u16, .big);
        var mem_instr_index = origin;

        var i: usize = 0;
        reading: while (mem_instr_index < u16_max) {
            self.mem[mem_instr_index] = reader.readInt(u16, .big) catch |err| {
                switch (err) {
                    error.EndOfStream => break :reading,
                    else => return false,
                }
            };

            i += 1;
            mem_instr_index += 1;
        }

        return true;
    }

    // Add
    // 15-12 | 11-9 | 8-6 | 5 | 4-3 | 2-0 |
    // ----------------------------------
    //  0001 |  DR  | SR1 | 0 | 00  | SR2 | = register mode.  store sum of numbers stored in SR1 and SR2 in DR
    //  0001 |  DR  | SR1 | 1 |   imm 5   | = immediate mode. same as register mode, but second number is embedded in the instruction
    //
    // in immediate mode, the second operand is obtained by sign-extending the imm5 field to 16 bits.
    fn add(self: *Vm, instr: u16) ?u16 {
        const dst = dst_register(instr);
        const src1 = (instr >> 6) & 0x7;
        const immediate_flag = (instr >> 5) & 0x1 == 1;

        if (immediate_flag) {
            // 0x1F == 31, and imm5 is 5 bits wide
            const imm5 = sign_extend(@as(u16, instr & 0x1F), 5);
            self.reg[dst] = @addWithOverflow(self.reg[src1], imm5)[0];
        } else {
            const src2 = instr & 0x7;
            self.reg[dst] = @addWithOverflow(self.reg[src1], self.reg[src2])[0];
        }

        return dst;
    }

    // Bitwise-AND
    // 15-12 | 11-9 | 8-6 | 5 | 4-3 | 2-0 |
    // ----------------------------------
    //  0101 |  DR  | SR1 | 0 | 00  | SR2 | = register mode.  store bitwise-AND of contents of SR1 and SR2 in DR
    //  0101 |  DR  | SR1 | 1 |   imm 5   | = immediate mode. same as register mode, but second operand is in imm5
    fn b_and(self: *Vm, instr: u16) ?u16 {
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

        return dst;
    }

    // Branch
    // 15-12 | 11 | 10 | 9 |     8-0   |
    // ---------------------------------
    //  0000 | n  | z  | p | PCoffset9 |
    //
    // Check condition codes and branch.
    // If 11 is set, N is tested - if 11 is not set, N is not tested.
    // If 10 is set, Z is tested - if 10 is not set, Z is not tested.
    // EX: BRzp LOOP -> branch to LOOP if condition was zero or positive.
    // EX: BR   NEXT -> branch to NEXT, unconditionally.
    //
    // If any of the tested condition codes are set, branch to the location specified
    // by adding sign-extended PCoffset9 to the incremented PC.
    fn branch(self: *Vm, instr: u16) ?u16 {
        const test_pos = (instr >> 11) & 0x1 == 1;
        const test_zero = (instr >> 10) & 0x1 == 1;
        const test_neg = (instr >> 9) & 0x1 == 1;

        const cond = self.reg[Register.cond()];

        if ((test_pos and (cond & Sign.POS.val()) == 1) or (test_zero and (cond & Sign.ZRO.val()) == 1) or (test_neg and (cond & Sign.NEG.val()) == 1)) {
            self.reg[Register.pc()] += sign_extend(@as(u16, instr & 0x1FF), 9);
        }

        return null;
    }

    // Jump / Ret
    //  | 15-12 | 11-9 |   8-6 |  5-0  |
    //  --------------------------------
    // J: 1100  | 000  | BaseR | 00000 |
    // R: 1100  | 000  | 111   | 00000 |
    //
    // Jump: Program unconditionally jumps to the location specified by BaseR
    // Ret: Set PC to R7, which contains linkage to instruction following subroutine call
    fn jump(self: *Vm, instr: u16) ?u16 {
        const base_register = (instr >> 6) & 0x7;

        self.reg[Register.pc()] = self.reg[base_register];

        return null;
    }

    // Jump to Subroutine
    //     | 15-12 | 11| 10-9 |  8-6|  5-0  |
    //  ---------------------------------------
    //  JSR:  0100 | 1 | PCoffset11           |
    //  JSRR: 0100 | 0 |  00  | BaseR | 00000 |
    fn jump_sr(self: *Vm, instr: u16) ?u16 {
        self.reg[7] = self.reg[Register.pc()];

        if ((instr >> 11) & 0x1 == 0) {
            const subroutine_base_reg = (instr >> 6) & 0x7;
            self.reg[Register.pc()] = subroutine_base_reg;
        } else {
            const new_addr = sign_extend(instr & 0x7FF, 11);
            self.reg[Register.pc()] += new_addr;
        }

        return null;
    }

    // Load
    // 15-12 | 11-9 |    8-0    |
    // --------------------------
    //  0010 |  DR  | PCoffset9 |
    fn load(self: *Vm, instr: u16) ?u16 {
        const dst = dst_register(instr);
        const addr = @addWithOverflow(sign_extend(instr & 0x1FF, 9), self.reg[Register.pc()]);

        self.reg[dst] = self.mem_read(addr[0]);

        return dst;
    }

    // LDI
    // 15-12 | 11-9 |    8-0  |
    // ------------------------
    //  1010 |  DR  | PCoffset9
    //
    // An address is computed by sign-extending PCoffset9 and adding this value to the incremented PC.
    // The value at this address is loaded into DR.
    fn load_indirect(self: *Vm, instr: u16) ?u16 {
        const dst = dst_register(instr);

        const mem_loc = @addWithOverflow(sign_extend(instr & 0x1FF, 9), self.reg[Register.pc()])[0];
        self.reg[dst] = self.mem_read(self.mem_read(mem_loc));

        return dst;
    }

    // Load Base + offset
    // 15-12 | 11-9 | 8-6 |   5-0   |
    // ------------------------------
    //  0110 |  DR  |BaseR| offset6 |
    fn load_base_offset(self: *Vm, instr: u16) ?u16 {
        const dst = dst_register(instr);
        const base_reg = self.reg[(instr >> 6) & 0x7];

        const addr = @addWithOverflow(sign_extend(instr & 0x3F, 6), base_reg)[0];
        self.reg[dst] = self.mem[addr];

        return dst;
    }

    // Load effective address
    // 15-12 | 11-9 |    8-0  |
    // ------------------------
    //  1110 |  DR  | PCoffset9
    fn load_effective(self: *Vm, instr: u16) ?u16 {
        const dst = dst_register(instr);
        const addr = @addWithOverflow(sign_extend(instr & 0x1FF, 9), self.reg[Register.pc()]);

        self.reg[dst] = addr[0];

        return dst;
    }

    // Not
    // 15-12 | 11-9 | 8-6 | 5 | 4-0  |
    // -------------------------------
    //  1001 |  DR  | SRC | 1 | 1111 |
    //  Bitwise-complement of contents of SRC are stored in DR
    fn b_not(self: *Vm, instr: u16) ?u16 {
        const dst = dst_register(instr);

        const src = (instr >> 6) & 0x7;
        self.reg[dst] = ~self.reg[src];

        return dst;
    }

    // Store
    // 15-12 | 11-9 |    8-0    |
    // -------------------------------
    //  0011 |  SR  | PCoffset9 |
    fn store(self: *Vm, instr: u16) ?u16 {
        const src = (instr >> 9) & 0x7;
        const mem_loc = @addWithOverflow(sign_extend(instr & 0x1FF, 9), self.reg[Register.pc()]);
        self.mem_write(mem_loc[0], self.reg[src]);

        return null;
    }

    // Store indirect
    // 15-12 | 11-9 |    8-0    |
    // -------------------------------
    //  1011 |  SR  | PCoffset9 |
    // What is in memory at this address is the address of the location to which the
    // data in SR in stored.
    fn store_indirect(self: *Vm, instr: u16) ?u16 {
        const src = (instr >> 9) & 0x7;
        const mem_loc = @addWithOverflow(sign_extend(instr & 0x1FF, 9), self.reg[Register.pc()]);

        self.mem_write(self.mem[mem_loc[0]], self.reg[src]);

        return null;
    }

    // Store Base + offset
    // 15-12 | 11-9 |  8-6  |   5-0   |
    // -------------------------------
    //  0111 |  SR  | BaseR | offset6 |
    // The contents of the register specified by SR are stored in the memory location
    // whose address is computed by sign-extending bits [5:0] to 16 bits and adding this
    // value to the contents of the register specified by bits [8:6].
    fn store_base_offset(self: *Vm, instr: u16) ?u16 {
        const src = (instr >> 9) & 0x7;
        const base_reg = (instr >> 6) & 0x7;

        const mem_loc = @addWithOverflow(sign_extend(instr & 0x3F, 6), self.reg[base_reg]);

        self.mem_write(mem_loc[0], self.reg[src]);

        return null;
    }

    // Trap
    // 15-12 | 11-8 |    7-0    |
    //  1111 | 0000 | trapvect8 |
    fn trap(self: *Vm, instr: u16) ?u16 {
        self.reg[7] = self.reg[Register.pc()];

        switch (instr & 0xFF) {
            Trap.GETC.val() => self.trap_getc(),
            Trap.OUT.val() => self.trap_out(),
            Trap.PUTS.val() => self.trap_puts(),
            Trap.IN.val() => self.trap_in(),
            Trap.PUTSP.val() => {},
            Trap.HALT.val() => self.trap_halt(),
            else => {},
        }

        return null;
    }

    fn trap_getc(self: *Vm) void {
        const char = std.io.getStdIn().reader().readByte() catch unreachable;
        self.reg[0] = @as(u16, char);
    }

    // Write a character in R0[7:0] to the display.
    fn trap_out(self: *Vm) void {
        const char: u8 = @truncate(self.reg[0]);
        _ = stdout.write(&[1]u8{char}) catch unreachable;
    }

    fn trap_puts(self: *Vm) void {
        var buffered_writer = std.io.bufferedWriter(stdout);

        // Memory address of first character is stored in R0.
        // Printing stops when we receive 0x0000 in a location.
        var addr = self.reg[0];
        while (self.mem[addr] != 0) {
            const char: u8 = @intCast(self.mem[addr]);
            _ = buffered_writer.write(&[1]u8{char}) catch unreachable;
            addr += 1;
        }

        buffered_writer.flush() catch unreachable;
    }

    fn trap_in(self: *Vm) void {
        _ = stdout.write("Enter a character: ") catch unreachable;
        const input = stdin.readByte() catch unreachable;

        const char = @as(u16, input);
        self.reg[0] = char;
    }

    fn trap_putsp(self: *Vm) void {
        var buffered_writer = std.io.bufferedWriter(stdout);
        const shifts = [2]u8{ 0, 8 };
        const mem_addr = self.reg[0];

        while (mem_addr < self.mem.len) {
            const val = self.mem[mem_addr];
            if (val == 0) {
                break;
            }

            process_val: for (shifts) |shift| {
                const char = (val >> shift) & 0xFF;

                if (char == 0) {
                    break :process_val;
                }

                buffered_writer.write(&[1]u8{char}) catch unreachable;
            }

            mem_addr += 1;
        }

        buffered_writer.flush() catch unreachable;
    }

    fn trap_halt(self: *Vm) void {
        std.debug.print("HALT", .{});
        self.running = false;
    }

    // Update the COND register based on the number stored in index `r`.
    fn update_flags(self: *Vm, index: u16) void {
        var value = self.reg[Register.cond()];

        if (self.reg[index] == 0) {
            value = @intFromEnum(Sign.ZRO);
        } else if (((self.reg[index] >> 15) & 0x1) == 1) {
            value = @intFromEnum(Sign.NEG);
        } else {
            value = @intFromEnum(Sign.POS);
        }
    }

    fn mem_write(self: *Vm, addr: u16, val: u16) void {
        self.mem[addr] = val;
    }

    // TODO: read about MemMappedRegisters and how they work.
    fn mem_read(self: *Vm, addr: u16) u16 {
        if (addr == MemMappedRegisters.KeyboardStatus.val()) {
            if (check_keyboard()) {
                self.mem[MemMappedRegisters.KeyboardStatus.val()] = (1 << 15);
                self.mem[MemMappedRegisters.KeyboardData.val()] = @as(u16, std.io.getStdIn().reader().readByte() catch unreachable);
            } else {
                self.mem[MemMappedRegisters.KeyboardStatus.val()] = 0;
            }
        }

        return self.mem[addr];
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

fn check_keyboard() bool {
    var pollfd = c.pollfd{
        .fd = std.os.linux.STDIN_FILENO,
        .events = c.POLLIN,
        .revents = 0,
    };

    return c.poll(&pollfd, 1, -1) >= 0;
}

pub const NUM_REGISTERS = @typeInfo(Register).@"enum".fields.len;

// Register list.
const Register = enum {
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

    pub inline fn pc() u16 {
        return @intFromEnum(Register.PC);
    }

    pub inline fn cond() u16 {
        return @intFromEnum(Register.COND);
    }
};

// Sign of the previous calculation.
const Sign = enum(u16) {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,

    fn val(self: Sign) u16 {
        return @intFromEnum(self);
    }
};

// Instruction set for the LC-3 architecture.
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

const MemMappedRegisters = enum(u16) {
    KeyboardStatus = 0xFE00,
    KeyboardData = 0xFE02,

    pub fn val(self: MemMappedRegisters) u16 {
        return @intFromEnum(self);
    }
};

// Helpers for performing common tasks/interacting with I/O devices.
// When a trap code is called, PC is moved to that code's address (why we start at 0x3000).
const Trap = enum(u8) {
    GETC = 0x20, // get character from keyboard, not echoed to terminal
    OUT = 0x21, // output a character
    PUTS = 0x22, // output a word string
    IN = 0x23, // get character from keyboard, echoed to terminal
    PUTSP = 0x24, // output a byte string
    HALT = 0x25, // halt the program

    fn val(self: Trap) u16 {
        return @intFromEnum(self);
    }
};

const std = @import("std");
const c = @cImport({
    @cInclude("time.h");
    @cInclude("sys/select.h");
    @cInclude("poll.h");
});

// --- TESTS ---
const expect = std.testing.expect;
test "add_stored" {
    var reg = Vm.init();
    reg.reg[0] = 1;
    reg.reg[1] = 2;

    _ = reg.add(0b0001_010_000_0_00_001);
    try expect(reg.reg[2] == 3);
}

// test "add_negative" {
//      stored
//      immediate
// }

test "add_immediate" {
    var reg = Vm.init();
    reg.reg[0] = 1;

    _ = reg.add(0b0001_001_000_1_00001);
    try expect(reg.reg[1] == 2);
}

// test "ldi"

test "b_and_immediate" {
    var reg = Vm.init();
    reg.reg[0] = 1;

    _ = reg.b_and(0b0101_001_000_1_00001);
    try expect(reg.reg[1] == 1);
}

test "b_and_stored" {
    var reg = Vm.init();
    reg.reg[0] = 1;
    // reg.reg[1] is implicitly 0

    _ = reg.b_and(0b0101_010_000_0_00_001);
    try expect(reg.reg[2] == 0);
}

test "b_not" {
    var reg = Vm.init();
    reg.reg[0] = 1;

    _ = reg.b_not(0b1001_001_000_1_1111);
    try expect(reg.reg[1] == 0);
}

test "branch" {
    // TODO
}

test "jump" {
    var reg = Vm.init();
    reg.reg[1] = 1;

    const ret = reg.jump(0b1100_000_001_000000);
    try expect(ret == null);
    try expect(reg.reg[Register.pc()] == reg.reg[1]);
}

test "ret" {
    var reg = Vm.init();
    reg.reg[7] = 1;

    const ret = reg.jump(0b1100_000_111_000000);
    try expect(ret == null);
    try expect(reg.reg[Register.pc()] == 1);
}
