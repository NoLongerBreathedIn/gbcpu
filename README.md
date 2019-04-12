This is an implementation of a Game Boy Color, mostly (the DACs are missing,
and the screen will probably be different).

# CPU
The CPU is a Sharp LR35902. Not knowing the internals,
I decided to write my own microcode implementation.

The microinstruction consists of several fields:

| Field | Length | Purpose |
|--------|--------|---------------------------------------------------|
| `8w` | 4 | Select 8-bit register to write |
| `ALUr` | 4 | Select 8-bit register to read for right ALU input |
| `ALUl` | 2 | Select 8-bit register to read for left ALU input |
| `ALUc` | 8 | ALU mode select (see below) |
| `Fs` | 4 | Set flags |
| `Fr` | 4 | Reset flags |
| `Fm` | 4 | Extra set/reset of flags |
| `HL` | 2 | Set `HL` |
| `SP` | 2 | Set `SP` |
| `MA` | 2 | Set `MA` |
| `IP` | 2 | Set `IP` |
| `j` | 1 | Set jump register |
| `wt` | 1 | Write to memory |
| `di` | 1 | Clear interrupt-enable flag |
| `ei` | 1 | Set interrupt-enable flag (one-cycle delay) |
| `b16` | 2 | Source of 16-bit bus |
| `ID` | 2 | Source of 16-bit increment/decrement |
| `IDc` | 1 | Set to decrement 16-bit instead of increment |

## Internal registers
The internal registers consist of the LR35902 registers plus:
* `IP`, the instruction pointer (16 bits, split as `J` and `Q`)  
* `j`, the jump register (16 bits, split as `jH` and `jL`)  
* `MW`, the memory-write register (8 bits, write-only)  
* `MA`, the memory-address register (16 bits, split as `MAH` and `MAL`)
* `c`, the last-cycle-carry flag (set if the ALU's adder would have carried)
* `S` and `P`, the high and low halves of `SP`

There are also three pseudoregisters (read-only):
* `I`, the contents of the byte pointed to by `IP`
* `MR`, the contents of the byte pointed to by `MA`
* `DAA`, the amount to add or subtract to `A` to fix up BCD arithmetic

It is possible to simultaneously perform an 8-bit write to part of a 16-bit
register and a 16-bit write to the entire register.
Should this happen, the 16-bit write is performed first.

The registers `I`, `F`, and `c` are available to the instruction decoder,
as is a seven-bit state it keeps.

## `8w`
This selects the register to write the output of the ALU to, as follows:

| Value | Register |
|-------|----------|
| 0 | `B` |
| 1 | `C` |
| 2 | `D` |
| 3 | `E` |
| 4 | `H` |
| 5 | `L` |
| 6 | `MW` |
| 7 | `A` |
| 8 | `J` |
| 9 | `Q` |
| A | `jH` |
| B | `jL` |
| C | `MAH` |
| D | `MAL` |
| E | `S` |
| F | `P` |

## `ALUc`
The first two bits are general ALU control.

| Value | Operation |
|-------|-----------|
| 0 | XOR |
| 1 | Misc |
| 2 | AND |
| 3 | ADD |

If the ALU general control is Misc, the following operations are performed:

| Match | Operation |
|-|-|
| `****00` | Shift left (output first bit as carry, return bits 1-8) |
| `****10` | Shift right (output last bit as carry, return bits 7-F) |
| `***001` | Swap bytes of right input |
| `***011` | Arithmetic shift right of right input (output last bit as carry) |
| `def1r1` | Replace bit `def` of right input by `r`; also test it. |

If the operation is a shift, the left operand is immediately replaced by the
right operand before applying the next transformation.

The third and fourth bits are `kL` and `cL`; each bit `b` of the left operand
is replaced by `b & kL ^ cL`.

The fifth and sixth bits are `kR` and `cR`; they act exactly as above on the
right operand.

The last two bits are `cf` and `co`.
If `co` is on and the operation is AND, then the output is complemented.
`cf` is fed into the adder as the carry input.

## `ALUl`
This selects the left operand.

| Value | Operand |
|-|-|
| 0 | `A` |
| 1 | `I` |
| 2 | `H` |
| 3 | `L` |

## `ALUr`
This selects the right operand.

| Value | Operand |
|-------|---------|
| 0 | `B` |
| 1 | `C` |
| 2 | `D` |
| 3 | `E` |
| 4 | `H` |
| 5 | `L` |
| 6 | `MR` |
| 7 | `A` |
| 8 | `J` |
| 9 | `Q` |
| A | `I` |
| B | `DAA` |
| C | `F` |
| D | `F` |
| E | `S` |
| F | `P` |

Note that `F` appears twice. This is used to simplify logic.

## `Fs`, `Fr`, `Fm`
Every cycle, each flag in F
is set to the ALU's output if the corresponding bit of `Fm` is set,
then cleared if the corresponding bit of `Fr` is not set,
then set if the corresponding bit of `Fs` is set.
Then `F` is ored with `MR` if the bit of `Fm` corresponding to the
subtract flag is set.

## `HL`, `SP`

| Value | Operation |
|-|-|
| 0 | Leave alone |
| 1 | Leave alone |
| 2 | Set to output of incrementor/decrementor |
| 3 | Set to 16-bit bus |

## `IP`, `MA`
These act like `HL` and `SP` except for when the value is 1.
In this case, `IP` is set to `j`, while `MAL` is set to `C`, `MAH` to `0xFF`.

## `j`
If this is 1, then `j` is set to `I` bitwise anded with `0x38`
unless the interrupt request flag is on, in which case it is
set to the interrupt vector.

## `wt`
If this is 1, then the contents of MW are written to the location in MA.
New values written to these are ignored until next cycle.

## `b16`
This selects the source of the 16-bit bus.

| Value | Source |
|-|-|
| 0 | `BC` |
| 1 | `DE` |
| 2 | `HL` |
| 3 | `SP` |

## `ID`
This selects the source of the 16-bit incrementor.

| Value | Source |
|-|-|
| 0 | `IP` |
| 1 | `SP` |
| 2 | `HL` |
| 3 | `MA` |