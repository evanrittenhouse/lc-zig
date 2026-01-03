# CLAUDE.md - LC-3 VM in Zig

## Project Overview

This is a toy implementation of an LC-3 (Little Computer 3) Virtual Machine written in Zig, following [Justin Meiners' popular tutorial](https://www.jmeiners.com/lc3-vm). The LC-3 is a simple educational computer architecture designed to teach fundamental concepts of computer organization and assembly programming.

**Language**: Zig 0.15.2+
**Project Type**: Virtual Machine / Interpreter
**Main Purpose**: Educational implementation of LC-3 architecture

## Repository Structure

```
lc-zig/
├── src/
│   ├── vm.zig       # Core VM implementation (opcodes, traps, memory)
│   ├── main.zig     # Entry point and terminal I/O setup
│   └── root.zig     # Library entry point (mostly unused)
├── build.zig        # Zig build configuration
├── build.zig.zon    # Package metadata and dependencies
├── 2048.obj         # Sample LC-3 object file for testing
├── .gitignore
└── README.md
```

### Key Files

- **`src/vm.zig`** (23,738 bytes): The heart of the project containing:
  - VM struct with registers and memory
  - All 16 LC-3 opcodes implementation
  - Trap routines for I/O operations
  - Memory-mapped register handling
  - Comprehensive test suite (23 unit tests)

- **`src/main.zig`** (2,260 bytes): Application entry point with:
  - Argument parsing for LC-3 image files
  - Terminal input/output configuration (disabling buffering)
  - SIGINT signal handler
  - VM initialization and execution loop

- **`src/root.zig`** (382 bytes): Library module entry point (minimal usage, contains placeholder `add` function)

## Architecture & Design

### VM Structure

The VM uses Zig's `@This()` pattern for self-referential struct definition:

```zig
pub const Vm = @This();

// Fields:
reg: [NUM_REGISTERS]u16,  // 12 registers (R0-R8, PC, COND, COUNT)
mem: [MEMORY_MAX]u16,      // 65,536 memory locations
running: bool,             // Execution state
```

### LC-3 Components Implemented

**Registers** (11 total):
- R0-R8: General-purpose registers
- PC: Program Counter
- COND: Condition flags (POS, ZRO, NEG)

**Opcodes** (16 total, all implemented):
- Arithmetic: ADD, AND, NOT
- Memory: LD, LDI, LDR, LEA, ST, STI, STR
- Control: BR, JMP, JSR/JSRR, RTI
- System: TRAP

**Trap Routines**:
- GETC (0x20): Get character from keyboard (no echo)
- OUT (0x21): Output a character
- PUTS (0x22): Output a null-terminated string
- IN (0x23): Get character with echo
- PUTSP (0x24): Output a byte string (packed)
- HALT (0x25): Stop execution

**Memory-Mapped Registers**:
- KeyboardStatus (0xFE00): Keyboard ready status
- KeyboardData (0xFE02): Keyboard character data

### Platform-Specific Considerations

The codebase handles platform differences:
- **Unix/Linux**: Uses `poll()` for keyboard checking, termios for terminal control
- **Windows**: Disables keyboard polling (not supported), uses Windows API for stdin

## Build System

Built using Zig's modern build system:

```zig
// Two modules:
lib_mod  → src/root.zig (static library)
exe_mod  → src/main.zig (executable)
vm_helper → src/vm.zig (shared module)
```

**Build Commands**:
- `zig build` - Build the project
- `zig build run -- <image-file>` - Build and run with LC-3 image file
- `zig build test` - Run all unit tests

**Dependencies**:
- Links against system libc for terminal I/O operations
- No external Zig packages

## Development Workflows

### Making Changes

1. **Adding New Opcodes**: Extend the `run()` function's switch statement in `src/vm.zig`, implement the opcode function, and add corresponding tests.

2. **Modifying Trap Routines**: Update trap functions in `src/vm.zig`. Remember:
   - Trap routines must handle R7 (linkage register) correctly
   - Use buffered I/O for performance (see `trap_puts`)
   - Consider cross-platform compatibility

3. **Memory Operations**: All memory reads should use `mem_read()` (not direct access) to handle memory-mapped registers for keyboard input.

### Testing Strategy

The project has extensive unit tests in `src/vm.zig`:
- Test each opcode independently
- Test flag updates (POS, ZRO, NEG)
- Test edge cases (sign extension, branching, indirection)
- Run tests with: `zig build test`

**Test Naming Convention**: `test "operation_mode"` (e.g., `test "add_immediate"`, `test "branch_positive"`)

### Code Conventions

1. **Naming**:
   - Functions: snake_case (`sign_extend`, `mem_read`)
   - Types: PascalCase (`Vm`, `Opcode`, `Register`)
   - Constants: SCREAMING_SNAKE_CASE (`MEMORY_MAX`, `NUM_REGISTERS`)

2. **Bit Manipulation**:
   - Use binary literals for instruction encoding: `0b0001_010_000_1_00001`
   - Underscore grouping matches LC-3 instruction format
   - Document bit layouts in comments above functions

3. **Error Handling**:
   - Use Zig's error unions (`!void`, `!bool`)
   - Handle errors at call sites with `try` or explicit `catch`
   - Avoid `unreachable` except for truly impossible states

4. **Memory Safety**:
   - Use wrapping arithmetic (`+%`, `+=%`) for register/PC operations (intentional overflow)
   - Standard arithmetic for everything else
   - All array accesses are bounds-checked by Zig

5. **Comments**:
   - Document instruction bit layouts with ASCII tables
   - Include references to LC-3 specification behavior
   - Explain non-obvious operations (sign extension, two's complement)

## Common Patterns

### Instruction Decoding

```zig
const dst = (instr >> 9) & 0x7;          // Destination register (bits 11-9)
const src1 = (instr >> 6) & 0x7;         // Source register 1 (bits 8-6)
const immediate_flag = (instr >> 5) & 0x1 == 1;  // Immediate mode bit
const imm5 = sign_extend(instr & 0x1F, 5);       // 5-bit immediate value
```

### Flag Updates

After operations that modify registers, update condition flags:
```zig
fn add(self: *Vm, instr: u16) ?u16 {
    // ... operation ...
    return dst;  // Return register to update flags
}
```

The caller checks the return value and calls `update_flags()` if non-null.

### Sign Extension

For handling signed immediate values in instructions:
```zig
fn sign_extend(x: u16, bit_count: u4) u16 {
    // If MSB is 1 (negative), pad with 1s; otherwise pad with 0s
    if (((x >> (bit_count - 1)) & 1) == 1) {
        return x | (0xFFFF << bit_count);
    }
    return x;
}
```

## Known Issues & TODOs

From `src/main.zig`:
1. ✓ ~~Move traps to separate struct~~ (Current design is acceptable)
2. ✓ ~~Tests~~ (Comprehensive test suite added)
3. TODO: Better argument processing
4. TODO: Improve error propagation from VM code
5. TODO: Improve byte reading (consider bit casting approach)

From `src/vm.zig`:
- `mem_read()` needs better documentation about memory-mapped registers
- Windows keyboard support is incomplete (polling disabled)
- Consider refactoring trap routines for better testability

## Debugging Tips

1. **Enable instruction tracing**: Uncomment line 43 in `src/vm.zig`:
   ```zig
   std.debug.print("instr={d}, opcode = {d}, pc={d}\n", .{ instr, opcode, self.reg[Register.pc()] });
   ```

2. **Test with sample programs**: Use `2048.obj` or other LC-3 object files

3. **Register inspection**: Add debug prints in `update_flags()` to track register state

4. **Memory dumps**: Add helper to print memory regions during execution

## AI Assistant Guidelines

### When Making Changes

1. **Always read files before editing**: The codebase is well-structured but contains platform-specific code. Review context before changes.

2. **Maintain test coverage**: If you modify an opcode or add functionality, add corresponding tests. Follow existing test patterns.

3. **Preserve bit layout documentation**: The ASCII tables showing instruction formats are crucial for understanding. Keep them updated.

4. **Consider platform differences**: Test or note changes that might affect Windows vs Unix behavior (especially I/O operations).

5. **Use wrapping arithmetic correctly**:
   - PC and register arithmetic should use `+%` (intentional overflow)
   - Memory addressing should use `+%`
   - Test counters and normal logic should use standard `+`

### Code Review Checklist

- [ ] Does the change handle both immediate and register modes (if applicable)?
- [ ] Are condition flags updated correctly?
- [ ] Is sign extension applied where needed?
- [ ] Are bit masks correct for instruction decoding?
- [ ] Is there a corresponding test?
- [ ] Does it work with the existing `2048.obj` test file?
- [ ] Are there platform-specific considerations?

### Understanding the Instruction Format

LC-3 uses 16-bit instructions. Example for ADD:

```
Bit:  15  14  13  12 | 11  10  09 | 08  07  06 | 05 | 04  03 | 02  01  00
      ─────────────────────────────────────────────────────────────────
      0   0   0   1  |  DR (3b)   |  SR1 (3b)  | M  |  Mode-specific

M=0: Register mode → bits [2:0] = SR2 (source register 2)
M=1: Immediate mode → bits [4:0] = imm5 (signed immediate value)
```

### Common Pitfalls

1. **Forgetting sign extension**: Immediate values and offsets must be sign-extended
2. **PC increment timing**: PC is incremented before instruction execution
3. **Memory-mapped I/O**: Always use `mem_read()`, never direct `mem[]` access for reads
4. **Condition flag logic**: Flags use bit flags (0x1, 0x2, 0x4), not simple integers

## References

- [LC-3 VM Tutorial by Justin Meiners](https://www.jmeiners.com/lc3-vm) - Primary reference
- [LC-3 Instruction Set Architecture](https://en.wikipedia.org/wiki/Little_Computer_3) - Wikipedia reference
- [Zig Language Documentation](https://ziglang.org/documentation/) - For Zig-specific patterns
- [Two's Complement Arithmetic](https://en.wikipedia.org/wiki/Two%27s_complement) - Understanding sign extension

## Version History

- **0.15.2** (Latest): Updated to Zig 0.15.2, comprehensive test suite added
- Initial implementation with all LC-3 opcodes and trap routines
