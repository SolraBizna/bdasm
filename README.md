This repository contains a very barebones assembler for the BloodyDumb-101, and also serves as the central location for information on same.

# BloodyDumb-101

The BloodyDumb-101 is a very simple 16-bit single-instruction ISA. Its only instruction is a conditional move. It is 16-bit in every sense: 16-bit instructions, 16-bit data, 16-bit addresses, 16-bit registers...

## Registers

- `000/Q` = ALU output. See ALU section below for more information.
- `001/A` = ALU input A. Usually the left hand side of an operation.
- `010/B` = ALU input B. Usually the right hand side of an operation.
- `011/PC` = Program Counter. The address of the next instruction to execute. Automatically incremented by one after every instruction (unless that instruction writes to `PC`).
- `100/MA` = Memory Address. The address to read/write when reading/writing `MD`.
- `101/MD` = Memory Data. Reading/writing this register reads/writes memory. Having `MD` as both source and destination of an instruction is undefined behavior.
- `110/S` = Scratch register 1. Conventionally serves as a stack pointer.
- `111/X` = Scratch register 2.

### ALU output

When *writing* to the `Q` (ALU output) register, the value isn't written anywhere. It's simply discarded. One use of this is to update condition flags without clobbering any register.

When *reading* from the `Q` register, additional bits of the source field are used to denote which operation the ALU should perform:

- `00000/ADD`: `A + B`
- `00001/SUB`: `A - B`
- `00010/LSL`: `A << B` (logical shift left)
- `00011/LSR`: `A >> B` (logical shift right)
- `00100/ROL`: `A <<< B` (rotate left)
- `00101/ROR`: `A >>> B` (rotate right)
- `00110/AB`: `(A << 8) | (B & 255)` (combine the low 8 bits of A and B into a single 16-bit value)
- `00111/AINC`: `A + 1`
- `01000/BINC`: `B + 1`
- `01001/ADEC`: `A - 1`
- `01010/BDEC`: `B - 1`
- `01011/ASEX`: `(A & 0x80) ? (A | 0xFF00) : (A & 0x00FF)` (sign extend A from 8 to 16 bits)
- `01100/NOT`: `~A` (bitwise NOT)
- `01101/AND`: `A & B` (bitwise AND)
- `01110/OR`: `A | B` (bitwise OR)
- `01111/XOR`: `A ^ B` (bitwise XOR)
- `1xxxx`: Undefined.

Shifts with a greater magnitude than 16 are undefined.

## Instruction encoding

Instruction bits: `CCPUDDDTSSSSSSSS`

- `CC`: Condition flag to check.
    - `00`: No check; instruction always executed
    - `01`: Execute if Negative flag is set. (value had the high bit set)
    - `10`: Execute if Positive flag is set. (value had the high bit clear but was non-zero)
    - `11`: Execute if Zero flag is set. (value had no bits set)
- `P`: Whether to invert the result of the Condition flag check.
    - `0`: Check is not inverted
    - `1`: Check is inverted
- `U`: Whether to update the condition flags based on the moved value.
    - `0`: Condition flags are not changed.
    - `1`: Condition flags are changed. (Obviously, these flags can't take effect until the next instruction.)
- `DDD`: Destination register.
- `T`:
    - `0`: Source is a register. The upper five bits are ignored unless the lower three bits are 0.
    - `1`: Source is an immediate value, zero-extended to 16 bits.
- `SSSSSSSS`: Source register *or* value, depending on `T`

### Instruction notation

The only native instruction is a conditional move, creatively named `MOVE`.

`MOVE[.<cond>][?] <dest>, <src>`

Square brackets (`[]`) denote optional parts.

- `<cond>`: One of the following:
    - `AL`: Always execute. (Assumed if no condition is given.)
    - `NEVER`/`NAL`: Never execute. (Not very useful?)
    - `MI`: Execute if negative flag is set.
    - `NMI`: Execute if negative flag is NOT set.
    - `PL`: Execute if positive flag is set.
    - `NPL`: Execute if positive flag is NOT set.
    - `Z`: Execute if zero flag is set.
    - `NZ`: Execute if zero flag is NOT set.
- `?`: If present, the condition flags will be updated according to the moved value.
- `<dest>`: One of the registers given above.
- `<source>`: Either a literal 8-bit value, one of the registers given above, or one of the ALU operations.

Contrived example:

```asm
        MOVE A, 32               ; Unconditionally copy the literal value 32
                                 ; into A.
        MOVE? B, X               ; Unconditionally copy the current value of X
                                 ; into B, and update condition flags.
        MOVE.Z B, 3              ; If the value of X was zero, copy 3 into B.
        MOVE.MI? B, BDEC         ; If the value of X was negative, subtract 1
                                 ; from B and update condition flags.
        MOVE.PL B, BINC          ; If the value of X was positive and nonzero,
                                 ; OR if it was negative and decrementing it
                                 ; made it positive, add 1 to B.
        MOVE X, ADD              ; X ← A + B.
```

# bdasm

The name may be short for "BloodyDumb AsSeMbler" or for "BrainDead AsSeMbler", whichever the reader prefers. It doesn't support fancy features like multiple source files, complex linking, macros... Welcome to the 1970's, I guess.

Lua 5.3 is required, as well as the `lpeg` package. If you have Lua 5.3 and LuaRocks, you can install `lpeg` with:

```sh
$ luarocks install lpeg
```

Usage is very simple:

```sh
$ bdasm.lua input.s output.txt
or
$ bdasm.lua input.s output.txt listing.txt
```

It will either produce output, or some nice pretty error messages. Output is in the form of a LogiSim memory image (since that's what's used in the course).

The listing file is optional; if specified, you'll get a copy of the source file annotated with the actual generated machine code. Plugging in the contrived source example above will give you:

```asm
        MOVE A, 32               ; Unconditionally copy the literal value 32
;;0x8000: 0320 = MOVE A, 32
                                 ; into A.
        MOVE? B, X               ; Unconditionally copy the current value of X
;;0x8001: 1407 = MOVE? B, X
                                 ; into B, and update condition flags.
        MOVE.Z B, 3              ; If the value of X was zero, copy 3 into B.
;;0x8002: C503 = MOVE.Z B, 3
        MOVE.MI? B, BDEC         ; If the value of X was negative, subtract 1
;;0x8003: 5450 = MOVE.MI? B, BDEC
                                 ; from B and update condition flags.
        MOVE.PL B, BINC          ; If the value of X was positive and nonzero,
;;0x8004: 8440 = MOVE.PL B, BINC
                                 ; OR if it was negative and decrementing it
                                 ; made it positive, add 1 to B.
        MOVE X, ADD              ; X ← A + B.
;;0x8005: 0E00 = MOVE X, Q
```

Comments in the source file are designated with `;`. If a line has an identifier before any whitespace, it defines a label at the current position. A trailing colon is optional. (Instructions and directives without labels *must* be preceded by whitespace.)

Whether a line is a directive or an instruction depends on whether it starts with a `.` or not.

## Labels

Whenever a label is encountered, a symbol is defined at the current position. WLA-DX style anonymous labels are also supported: in addition to other normal identifiers, labels may consist of one or more hyphens (`-`) or pluses (`+`). When referencing a hyphen-label, the nearest match *before* the reference is used. When referencing a plus-label, the nearest match *after* the reference is used.

## Directives

### `.BASE` <addr>

Gives the address at which the program image is loaded. Defaults to 0x8000. Only allowed at the beginning of the file, before any code.

### `.SIZE` <addr>

Gives the maximum size that the program image may occupy. Also defaults to 0x8000. Only allowed at the beginning of the file, before any code.

### `.ORG <addr>`

Changes the position at which the next instructions/data will be emitted.

### `.DEFINE <name>, <value>`

Define the symbol `<name>` as having the given value. This value must be a literal integer that fits within 16 bits.

### `.WORDS <value>, ...`

Instead of instructions, places exact data values in the output. Each `<value>` is one of:

- A string literal delimited by double or single quotes, e.g. `"Apple Pie"` or `'Darth Disco'`. Double-quoted strings support backslash escapes, single-quoted strings do not. Each *byte* of the UTF-8 string is placed in its own memory cell (wasting 8 bits).
- An integer literal. The value is placed in its own memory cell.
- An identifier. The symbol value is placed in its own memory cell. (Not implemented yet)

As an example, a UTF-8-encoded string with a null terminator:

```asm
        .WORDS "Hello there, world!\n", 0
```

## Instructions

The BloodyDumb-101 has only a `MOVE` instruction. But phrasing everything as `MOVE`s is a huge pain. `bdasm` somewhat eases that pain by supporting pseudo-instructions implemented as one or more `MOVE` instructions. This partly replaces one form of pain with another, because many of the pseudo-instructions clobber registers. Oh well, the BloodyDumb-101 was designed to be easy to implement in hardware, not to program for...

### `WIDEMOVE register, bigvalue`

Move a full 16-bit value into the target register. Clobbers the `A` and `B` registers.

```asm
        MOVE A, bhi
        MOVE B, blo
        MOVE a, AB
```

### `PUSH value`

Pushes the given register (or 8-bit literal) onto the stack. `S` is the stack pointer. Clobbers the `B` and `MA` registers.

```asm
        MOVE B, S
        MOVE S, BDEC
        MOVE MA, S
        MOVE MD, a
```

### `WIDEPUSH bigvalue`

Pushes the given 16-bit literal onto the stack. `S` is the stack pointer. Clobbers the `A`, `B`, and `MA` registers.

```asm
        MOVE B, S
        MOVE S, BDEC
        MOVE MA, S
        WIDEMOVE MD, a
```

### `POP register`

Pops the topmost value on the stack into the given register. `S` is the stack pointer. Clobbers the `B` and `MA` registers.

```asm
        MOVE MA, S
        MOVE B, S
        MOVE S, BINC
        MOVE a, MD
```

### `RET`

Pops the topmost value on the stack into the PC. `S` is the stack pointer. Clobbers the `B` and `MA` registers.

```asm
        POP PC
```

### `JL bigvalue`

Jump and Link; push the address *following* this instruction onto the stack, then jump to the specified address. Subroutine call. Clobbers the `A` and `B` registers.

```asm
        WIDEPUSH _A
        WIDEMOVE PC, a
_A:
```

### `J bigvalue`

Jump to the specified address. Clobbers the `A` and `B` registers.

```asm
        WIDEMOVE PC, a
_A:
```

### `LOAD register, value`

Load a value from the specified address (from a register or as an 8-bit literal) and place it into the specified register. Clobbers the `MA` register.

Notice that unlike `WIDELOAD`, `LOAD` preserves the `A` and `B` registers. This makes the first 256 memory locations convenient for placing IO ports and temporary variables.

```asm
        MOVE MA, b
        MOVE a, MD
```

### `WIDELOAD register, bigvalue`

Load a value from the specified 16-bit address and place it into the specified register. Clobbers the `A`, `B`, and `MA` registers.

```asm
        WIDEMOVE MA, b
        MOVE a, MD
```

### `STORE value_dest, value`

Store the specified value (from a register or an 8-bit literal) into the specified destination address (from a register or an 8-bit literal). Clobbers the `MA` register.

Note that unlike `FARSTORE`, `WIDESTORE`, and `FARWIDESTORE`, `STORE` preserves the `A` and `B` registers. This makes the first 256 memory locations convenient for placing IO ports and temporary variables.

```asm
        MOVE MA, a
        MOVE MD, b
```

### `FARSTORE bigvalue_dest, value`

Store the specified value (from a register or an 8-bit literal) into the specified exact 16-bit destination address. Clobbers the `A`, `B`, and `MA` registers.

```asm
        WIDEMOVE MA, a
        MOVE MD, b
```

### `WIDESTORE value_dest, bigvalue`

Store the specified exact 16-bit value into the specified destination address (from a register or an 8-bit literal)

```asm
        MOVE MA, a
        WIDEMOVE MD, b
```

### `FARWIDESTORE bigvalue_dest, bigvalue`

Store the specified exact 16-bit value into the specified exact 16-bit destination address. Clobbers the `A`, `B`, and `MA` registers.

```asm
        WIDEMOVE MA, a
        WIDEMOVE MD, b
```

## Example code

This code runs on the canonical BloodyDumb-101 system, where 0x0000 is a TTY and 0x8000-0xFFFF is where the program is loaded. It is one of the files in the `examples/` directory, where you can also see its compiled image and its listing.

```asm
        ; These are the defaults, but we specify them explicitly for clarity
        .BASE 0x8000
        .SIZE 0x8000

        ; The location of the TTY port
        .DEFINE tty, 0x0000

        ; The entry point is 0x8000
        .ORG 0x8000
Entry:  ; The Scratch register will hold our pointer.
        WIDEMOVE X, _hello
-       ; (Temporarily holding the pointer in B)
        MOVE B, X
        ; Load the next byte into the Scratch register.
        LOAD? X, B
        ; If the byte was zero, the string is terminated. Stop the loop.
        J.Z +
        ; Write the byte to the TTY.
        STORE tty, X
        ; Increment the pointer and restore it to the proper place in the temp
        ; register
        MOVE X, BINC
        ; (If we didn't store it in the temp register, it would be clobbered by
        ; this jump)
        J -
+       ; Now echo everything that is typed.
-       LOAD? X, tty
        ; The sign bit being set signifies that no input was ready. Output only
        ; if the sign bit was clear.
        STORE.NMI tty, X
        ; Loop!
        J -

_hello: .WORDS "Hello World!\nReady for some echo?\n\n", 0
```

# Credits

I, Solra Bizna, hereby accept responsibility for designing this terrible ISA. I also wrote `bdasm`, and the "Hubris Compiler Compiler" (`hcc`) that produces it.

# License

`bdasm` is distributed under the zlib license. The (very brief) text of the license can be found in [`LICENSE.md`](LICENSE.md).

The BloodyDumb-101 ISA is in the public domain. Do whatever you want with it. (Why would you want to?)
