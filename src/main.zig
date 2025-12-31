// TODO:
// 1. SIGINT handling doesn't work
// 2. move traps to separate struct that takes pointers (?)
// 3. tests
// --------------
// 4. make args processing better
// 5. propagate error handling better from VM code
// 6. make byte reading way better. see example for bit casting

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = std.process.argsAlloc(allocator) catch |err| {
        std.debug.print("Error getting arguments: {s}\n", .{@errorName(err)});
        return err;
    };
    defer std.process.argsFree(allocator, args);
    if (args.len == 0) {
        std.debug.print("lc3 [image-file1] ...\n", .{});
        std.process.exit(2);
    }

    var orig_tio: c.termios = undefined;
    disable_input_buffering(&orig_tio);
    defer restore_input_buffering(&orig_tio);
    register_sigint_handler();

    var vm = Vm.init();
    for (args[1..]) |arg| {
        try vm.run(arg);
    }
}

fn disable_input_buffering(orig_tio: *c.termios) void {
    const stdin_fd = std.os.linux.STDIN_FILENO;
    _ = c.tcgetattr(stdin_fd, orig_tio);

    var new_tio: c.termios = orig_tio.*;
    const c_lflag: c_uint = @bitCast(~c.ICANON & ~c.ECHO);
    new_tio.c_lflag &= c_lflag;
    _ = c.tcsetattr(stdin_fd, c.TCSANOW, &new_tio);
}

fn restore_input_buffering(orig_tio: *const c.termios) void {
    const stdin_fd = std.os.linux.STDIN_FILENO;
    _ = c.tcsetattr(stdin_fd, c.TCSANOW, orig_tio);
}

fn register_sigint_handler() void {
    const action = posix.Sigaction{
        .handler = .{ .handler = handle_sig_int },
        .mask = posix.empty_sigset,
        .flags = 0,
    };

    posix.sigaction(posix.SIG.INT, &action, null);
}

fn handle_sig_int(_: c_int) callconv(.C) void {
    std.debug.print("HALT\n", .{});
    std.process.exit(1);
}

const std = @import("std");
const posix = std.posix;

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("vm_lib");
const vm_lib = @import("vm");

const Opcode = vm_lib.Opcode;
const Register = vm_lib.Register;
const Vm = vm_lib.Vm;

const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("sys/termios.h");
    @cInclude("sys/select.h");
});
