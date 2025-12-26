// Store program memory in an array.
const MEMORY: [vm.MEMORY_MAX]u16 = undefined;

// Store register values in an array.
const PC_START = 0x3000;

const RegisterList = [vm.NumRegisters]u16;

pub fn main() !void {
    // TODO: load CLI arguments here

    var reg = Registers.init();

    const running = true;
    while (running) {
        reg.inc_program_counter();

        // TODO: mem_read()
        const instr = 1;

        // Handle the instruction.
        _ = instr >> 12;
    }
}


test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "use other module" {
    try std.testing.expectEqual(@as(i32, 150), lib.add(100, 50));
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

const std = @import("std");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("vm_lib");
const vm = @import("vm");

const Opcode = vm.Opcode;
const Register = vm.Register;
const Registers = vm.Registers;
