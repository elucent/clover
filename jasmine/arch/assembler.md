# Jasmine Low-Level Assembler

After higher-level optimizations, Jasmine lowers its abstract instructions to target-specific machine code. In order
to maximize reusability, the interface Jasmine defines for the target is itself a set of low-level instructions, generally
correlating 1:1 with machine instructions, but not intended to be specific to any one particular architecture.

## Operands

#### General-Purpose Register `gpr`

A general-purpose register is a location (generally associated with a machine register) identified by an 8-bit unsigned integer id. It must be able to store up to a 64-bit integral or pointer value.

#### Floating-Point Register `fpr`

A floating-point register is a location (generally associated with a machine register) identified by an 8-bit unsigned integer id. It must be able to store either a 32-bit or 64-bit floating-point value.

#### 8/16/32-Bit Immediate Integer `imm8/16/32`

An immediate integer is a signed integer literal either 8, 16, or 32 bits wide.

#### 32/64-bit Immediate Float `fim32/64`

An immediate float is a literal floating-point number, either a 32-bit single precision or 64-bit double precision value.

#### Memory `mem`

A memory operand represents a location in memory, which can be either:
 - A pair of a general-purpose register and a 32-bit signed offset, in bytes.
 - A location, as a 32-bit function index, data index, static index, or label index.

## Instructions

### Arithmetic

#### `add8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Add N-bit integer registers.

#### `add8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Add N-bit integer immediate and register.

#### `sub8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Subtract N-bit integer registers.

#### `sub8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Subtract N-bit integer immediate from register.

#### `sub8/16/32/64 <dst:gpr> <immN> <regA:gpr>`
Subtract N-bit integer register from immediate.

#### `mul8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Multiply N-bit integer registers.

#### `mul8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Multiply N-bit integer register by immediate.

#### `div8/16/32/64s <dst:gpr> <regA:gpr> <regB:gpr>`
Divide signed N-bit integer registers.

#### `div8/16/32/64s <dst:gpr> <regA:gpr> <immN>`
Divide signed N-bit integer register by immediate.

#### `div8/16/32/64s <dst:gpr> <immN> <regA:gpr>`
Divide signed N-bit immediate by integer register.

#### `div8/16/32/64u <dst:gpr> <regA:gpr> <regB:gpr>`
Divide unsigned N-bit integer registers.

#### `div8/16/32/64u <dst:gpr> <regA:gpr> <immN>`
Divide unsigned N-bit integer register by immediate.

#### `div8/16/32/64u <dst:gpr> <immN> <regA:gpr>`
Divide unsigned N-bit immediate by integer register.

#### `rem8/16/32/64s <dst:gpr> <regA:gpr> <regB:gpr>`
Find remainder of signed N-bit integer registers.

#### `rem8/16/32/64s <dst:gpr> <regA:gpr> <immN>`
Find remainder of signed N-bit integer register by immediate.

#### `rem8/16/32/64s <dst:gpr> <immN> <regA:gpr>`
Find remainder of signed N-bit integer immediate by register.

#### `rem8/16/32/64u <dst:gpr> <regA:gpr> <regB:gpr>`
Find remainder of unsigned N-bit integer registers.

#### `rem8/16/32/64u <dst:gpr> <regA:gpr> <immN>`
Find remainder of unsigned N-bit integer register by immediate.

#### `rem8/16/32/64u <dst:gpr> <immN> <regA:gpr>`
Find remainder of unsigned N-bit integer immediate by register.

### Floating-Point Arithmetic

#### `fadd32/64 <dst:fpr> <regA:fpr> <regB:fpr>`
Add floating-point registers.

#### `fadd32/64 <dst:fpr> <regA:fpr> <fimN>`
Add floating-point immediate to register.

#### `fsub32/64 <dst:fpr> <regA:fpr> <regB:fpr>`
Subtract floating-point registers.

#### `fsub32/64 <dst:fpr> <regA:fpr> <fimN>`
Subtract floating-point immediate from register.

#### `fsub32/64 <dst:fpr> <fimN> <regA:fpr>`
Subtract floating-point register from immediate.

#### `fmul32/64 <dst:fpr> <regA:fpr> <regB:fpr>`
Multiply floating-point registers.

#### `fmul32/64 <dst:fpr> <regA:fpr> <fimN>`
Multiply floating-point register by immediate.

#### `fdiv32/64 <dst:fpr> <regA:fpr> <regB:fpr>`
Divide floating-point registers.

#### `fdiv32/64 <dst:fpr> <regA:fpr> <fimN>`
Divide floating-point register by immediate.

#### `fdiv32/64 <dst:fpr> <fimN> <regA:fpr>`
Divide floating-point immediate by register.

#### `frem32/64 <dst:fpr> <regA:fpr> <regB:fpr>`
Find remainder of floating-point registers.

#### `frem32/64 <dst:fpr> <regA:fpr> <fimN>`
Find remainder of floating-point register and immediate.

#### `frem32/64 <dst:fpr> <fimN> <regA:fpr>`
Find remainder of floating-point immediate and register.

### Bitwise Operations

#### `and8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Bitwise AND two integer registers.

#### `and8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Bitwise AND register and immediate.

#### `xor8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Bitwise XOR two integer registers.

#### `xor8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Bitwise XOR register and immediate.

#### `or8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Bitwise OR two integer registers.

#### `or8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Bitwise OR register and immediate.

#### `not8/16/32/64 <dst:gpr> <regA:gpr>`
Bitwise NOT integer register.

#### `shl8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Bitwise left-shift integer register.

#### `shl8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Bitwise left-shift integer register by immediate.

#### `shr8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Logical bitwise right-shift integer register.

#### `shr8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Logical bitwise right-shift integer register by immediate.

#### `sar8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Arithmetic bitwise right-shift integer register.

#### `sar8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Arithmetic bitwise right-shift integer register by immediate.

#### `rol8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Bitwise rotate-left integer register.

#### `rol8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Bitwise rotate-left integer register by immediate.

#### `ror8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Bitwise rotate-left integer register.

#### `ror8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
Bitwise rotate-left integer register by immediate.

#### `popc8/16/32/64 <dst:gpr> <src:gpr>`
Count set bits of integer register.

#### `lzc8/16/32/64 <dst:gpr> <src:gpr>`
Count leading zeroes (from most-significant bit) of integer register.

#### `tzc8/16/32/64 <dst:gpr> <src:gpr>`
Count trailing zeroes (from least-significant bit) of integer register.

### Comparisons

#### `isz <dst:gpr> <regA:gpr>`
Sets result to one if source register is zero, zero otherwise.

#### `isnz <dst:gpr> <regA:gpr>`
Sets result to one if source register is nonzero, zero otherwise.

#### `cCC8/16/32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Compare equality between N-bit integer registers.

#### `cCC8/16/32/64 <dst:gpr> <regA:gpr> <immN>`
#### `cCC8/16/32/64 <dst:gpr> <immN> <regA:gpr>`
Compare N-bit integer register and immediate.

CC can be any of:
 - Equal `eq`
 - Not equal `neq`
 - Less-than `lt`
 - Less-than or equal to `leq`
 - Greater-than `gt`
 - Greater-than or equal to `geq
 - Below (unsigned less-than) `b`
 - Below or equal to `beq`
 - Above (unsigned greater-than) `a`
 - Above or equal to `aeq`

#### `cfCC32/64 <dst:gpr> <regA:gpr> <regB:gpr>`
Compare equality between N-bit floating-point registers.

#### `cfCC32/64 <dst:gpr> <regA:gpr> <immN>`
#### `cfCC32/64 <dst:gpr> <immN> <regA:gpr>`
Compare N-bit floating-point register and immediate.

CC can be any of:
 - Equal `eq`
 - Not equal `neq`
 - Less-than `lt`
 - Less-than or equal to `leq`
 - Greater-than `gt`
 - Greater-than or equal to `geq`

### Memory

#### `push8/16/32/64 <src:gpr>`
Push register to the stack.

#### `pop8/16/32/64 <dst:gpr>`
Pop register from the stack.

#### `fpush32/64 <src:fpr>`
Push floating-point register to the stack.

#### `fpop32/64 <dst:fpr>`
Pop floating-point register from the stack.

#### `mov8/16/32/64 <dst:gpr> <src:gpr>`
Integer register-to-register move.

#### `fmov32/64 <dst:fpr> <src:fpr>`
Floating-point register-to-register move.

#### `ld8/16/32/64 <dst:gpr> <src:mem>`
Load N-bit integer value from memory into register.

#### `st8/16/32/64 <dst:mem> <src:gpr>`
Store N-bit integer value from register into memory.

#### `st8/16/32/64 <dst:mem> <immN>`
Store N-bit integer immediate into memory.

#### `fld32/64 <dst:fpr> <src:mem>`
Load N-bit floating-point value from memory into register.

#### `fst32/64 <dst:mem> <src:fpr>`
Store N-bit floating-point value from register into memory.

#### `fst32/64 <dst:mem> <fimN>`
Store N-bit floating-point immediate into memory.

#### `lda <dst:gpr> <src:mem>`
Compute address of memory operand into register.

#### `ldi8/16/32/64 <dst:gpr> <src:mem> <idx:gpr>`
Load memory address plus N-scaled index into register.

#### `ldai8/16/32/64 <dst:gpr> <src:mem> <idx:gpr>`
Compute memory address plus N-scaled index into register.

#### `sti8/16/32/64 <dst:mem> <idx:gpr> <src:fpr>`
Store N-bit integer value from register into memory index.

#### `fldi32/64 <dst:fpr> <src:mem> <idx:gpr>`
Load memory index plus N-scaled index into floating-point register.

#### `fsti32/64 <dst:mem> <idx:gpr> <src:fpr>`
Store N-bit floating-point register into memory index.

#### `fsti32/64 <dst:mem> <idx:gpr> <src:fimN>`
Store N-bit floating-point immediate into memory index.

#### `ldc64 <dst:gpr> <imm64>`
Load 64-bit integer constant into register.

### Jumps

#### `j <dst:label>`
Unconditional jump to label.

#### `jz <dst:label> <cond:gpr>`
Jump to label if zero.

#### `jnz <dst:label> <cond:gpr>`
Jump to label if not zero.

#### `jCC8/16/32/64 <dst:label> <regA:gpr> <regB:gpr>`
Jump to label if comparison is true between integer registers.
CC can be any of the conditions valid in `cmp_CC`.

#### `jCC8/16/32/64 <dst:label> <regA:gpr> <immN>`
#### `jCC8/16/32/64 <dst:label> <immN> <regA:gpr>`
Jump to label if comparison is true between integer register and immediate.
CC can be any of the conditions valid in `cmp_CC`.

#### `jfCC32/64 <dst:label> <regA:fpr> <regB:fpr>`
Jump to label if comparison is true between floating-point registers.
CC can be any of the conditions valid in `fcmp_CC`.

#### `jfCC32/64 <dst:label> <regA:fpr> <fimN>`
#### `jfCC32/64 <dst:label> <fimN> <regA:fpr>`
Jump to label if comparison is true between floating-point register and immediate.
CC can be any of the conditions valid in `fcmp_CC`.

#### `j <dst:gpr>`
Jump to address contained in integer register.

### Functions

#### `enter`
Saves previous stack pointer.

#### `stack <size:imm32>`
Allocates size bytes by lowering the stack pointer.

#### `alloca <dst:gpr> <size:gpr>`
#### `alloca <dst:gpr> <size:imm32>`
Allocates size bytes by lowering the stack pointer, storing the resulting stack pointer in the destination.

#### `unstack <size:gpr>`
#### `unstack <size:imm32>`
Releases size bytes from the tip of the stack.

#### `leave`
Restores previous stack pointer.

#### `call <dst:label>`
Call labelled function.

#### `call <dst:gpr>`
Call address contained in integer register.

#### `ret`
Return from current function.

### Conversions

#### `sxt8/16/32 <dst:gpr> <src:gpr>`
Sign-extends source register into 64-bit destination.

#### `zxt8/16/32 <dst:gpr> <src:gpr>`
Zero-extends source register into 64-bit destination.

#### `i8/16/32/64tof32/64 <dst:fpr> <src:gpr>`
Converts source register to N-bit floating-point register.

#### `f32/64toi8/16/32/64 <dst:gpr> <src:fpr>`
Converts N-bit source register to integer register.

#### `f32to64 <dst:fpr> <src:fpr>`
Convert single-precision float to double-precision.

#### `f64to32 <dst:fpr> <src:fpr>`
Convert double-precision float to single-precision.

#### `f32frombits <dst:fpr> <src:gpr>`
Reinterpret source integer register as 32-bit floating-point number.

#### `f64frombits <dst:fpr> <src:gpr>`
Reinterpret source integer register as 64-bit floating-point number.

#### `f32tobits <dst:gpr> <src:fpr>`
Reinterpret 32-bit float as integer.

#### `f64tobits <dst:gpr> <src:fpr>`
Reinterpret 64-bit float as integer.

## Advanced Operations

#### `mcpy <dst:mem> <src:mem> <size:gpr>`
#### `mcpy <dst:mem> <src:mem> <size:immN>`
Copy size bytes of memory from source to destination.

#### `mmov <dst:mem> <src:mem> <size:gpr>`
#### `mmov <dst:mem> <src:mem> <size:immN>`
Move size bytes of memory from source to destination.

#### `mset <dst:mem> <src:gpr> <size:gpr>`
#### `mset <dst:mem> <src:gpr> <size:immN>`
Set size bytes of destination memory to the provided source value.

#### `mcmp <dst:gpr> <a:mem> <b:mem> <size:gpr>`
#### `mcmp <dst:gpr> <a:mem> <b:mem> <size:immN>`
Bytewise compare size bytes between a and b and store the result in the destination.
Result is either:
 - A negative number, if a is lexicographically less than b.
 - Zero, if a and b are the same.
 - A positive number, if a is lexicographically greater than b. 