# Jasmine

Jasmine is a portable, general-purpose, low-level intermediate representation for programs, libraries, and other code artifacts. Its principal goals are:

 - **Fast compilation**. Current widely-used IRs such as LLVM successfully achieve fairly high levels of optimization, but often this is at the cost of
   compilation speed. For rapid retargeting, JIT compilers, or simply building large software projects, this trade-off can make LLVM an unfortunately
   poor fit. Jasmine aims to be translatable to native as quickly as possible to minimize the impact of all of these aspects.

 - **Optimizability**. Some existing low-level IRs and higher-level bytecode formats, notably including WASM, require transformations to and from
   different representations in order to be optimizable. This can also make these formats poor stores of optimizations, that is to say that it may be
   difficult to optimize a WASM program in advance and avoid doing that work when lowering to native. Jasmine instructions generally take the form of
   non-destructive three-address code, which must be in static single assignment form. This makes it very easy to traverse the dataflow of a function
   or compute its control-flow graph.

 - **Easy to distribute**. Most existing object file formats and binary artifacts are inherently locked to a particular OS, and as such most tooling for
   manipulating their contents is limiting. Often due to OS-specific loader requirements, hard lines are drawn between relocatable objects, dynamic libraries,
   and executables. Jasmine objects, in contrast, should function as the one distributable format. An object can contain a single relocatable module, or
   an entire program with all of its dependencies statically included. Jasmine objects should have an OS-agnostic format, and an OS-agnostic loader,
   sidestepping proprietary loaders and file formats.

 - **Portability**. Perhaps the most important benefit of using any IR is being able to retarget programs to a variety of different platforms and
   architectures. Jasmine IR should be able to support a wide variety of backends simultaneously, with minimal - if any - platform-dependence within
   a Jasmine module. Platform-dependent code - when necessary - should be clearly labeled, and support for multiple platforms should be able to be
   bundled within a Jasmine object.

## 1 Types

Jasmine programs must be well-typed within the Jasmine type system. To support low-level programming, Jasmine's type system is fairly flexible, containing
relatively few high-level type system features and some low-level types such as untyped pointers. In general, Jasmine types are split into two categories,
_primitive_ and _compound_ types.

Jasmine types describe both values in a Jasmine program _and_ aspects of Jasmine's binary encoding. We will be referring to certain value encodings in
future sections using Jasmine types. When _materialized_ as a value in a Jasmine program at runtime, types may have runtime layout requirements, including
a size in 8-bit bytes and an _alignment interval_, the number by which the lowest address of a value in memory must be evenly divisible. The alignments, sizes,
and encoding descriptions for all Jasmine types are detailed below.

#### 1.1 Primitive Types

Primitive types represent simple scalar values with a small fixed size and no substructure. A general rule of thumb is that primitive types should fit within
a CPU register on most architectures. Jasmine currently defines the following primitive types:

| Type | Description |
|---|---|
| `i8` | 8-bit signed integers. |
| `i16` | 16-bit signed integers. |
| `i32` | 32-bit signed integers. |
| `i64` | 64-bit signed integers. |
| `iword` | Word-size signed integers. Precision varies system-to-system, but `iword`s are generally equal in width to a general-purpose register. |
| `ptr` | Untyped generic memory addresses. |
| `f32` | 32-bit single-precision floating-point numbers. |
| `f64` | 64-bit double-precision floating-point numbers. |
| `u8` | 8-bit unsigned integers. |
| `u16` | 16-bit unsigned integers. |
| `u32` | 32-bit unsigned integers. |
| `u64` | 64-bit unsigned integers. |
| `uword` | Word-size unsigned integers. Precision varies system-to-system, but `uword`s are generally equal in width to a general-purpose register. |
| `ref` | Memory address marked as a reference for GC purposes. |
| `void` | Uninhabited type, represents the absence of a value. Only permitted in function types. |

Primitive types must be aligned to the nearest power-of-two greater or equal to their size in bytes.

Signed integers are encoded using [LEB128](https://en.wikipedia.org/wiki/LEB128), while unsigned integers, pointers, and references are encoded using [ULEB128](https://en.wikipedia.org/wiki/LEB128#Unsigned_LEB128).
Encoding any of these types using more bytes than is necessary to encode the maximum or minimum value of that type is invalid, although implementations are not 
required to refuse this encoding or report an error. Floating-point numbers of `f32` and `f64` are encoded using 4 or 8 bytes respectively, storing the literal
value of the number using little-endian. `void` is not materializable, has no values, and thus has no value encoding.

#### 1.2 Array Types

The simplest compound type in Jasmine is the _array_ type. Array types represent contiguous, homogeneous sequences of an _element type_. The element type of
an array may be any type, including another array type. Array types have a fixed integer _length_, specifying the number of elements contained by the array. 
The overall size of an array in bytes is the size in bytes of its element type, multiplied by its length.

All elements of an array may be _indexed_ - uniquely identified by their ordinal position within the array, starting from zero. Elements must also be able to be
read and written in constant time by index. The memory address of an element of an array must be equal to the lowest address of the array plus the element's index 
multiplied by the size of the element type in bytes.

Arrays must be aligned to the same interval as their element type.

Array values are encoded by encoding each of their elements in order of increasing index.

#### 1.3 Tuple Types

Tuples, which have tuple types, are heterogeneous sequences of a fixed number of typed elements. These elements are formally called _members_, and their types
are the _member types_ of a given tuple type. Member types may be any other type, including another compound type. The number of members in a tuple is known as 
its length.

All members of a tuple may be accessed by their ordinal index, starting from zero. Tuple elements must be able to be read and written in constant time by their
index. Unlike an array, the index used to access must be a constant integer value, known in advance by the compiler.

Members within the tuple are laid out sequentially, and must be aligned within the tuple. For example, a tuple of type `{i8, i32}` will first contain an 8-bit 
signed integer, followed by three bytes of padding, followed by a 32-bit signed integer. The size of the tuple overall in bytes is the sum of the sizes in bytes
of its members, plus however many bytes of padding are necessary to align each member within the tuple. Overall, the tuple must be aligned to the highest of
its members' alignment intervals.

Tuple values are encoded by encoding each of their members in order of increasing index.

#### 1.4 Function Types

Function types describe transformations from a sequence of _parameter types_ to a single _return type_. Function types may have any number of parameters, including
zero. A return type must always be specified, but may uniquely be declared `void`.

Functions cannot currently be materialized as values, and so cannot be used within other types such as arrays or tuples. Correspondingly they have no alignemnt,
and cannot be encoded as values.

#### 1.5 Vector Types

Vector types, like array types, represent contiguous, homogeneous sequences of elements. Compared to arrays, vectors have two main differences. First is that vectors
may only contain numeric types - primitive types other than `ptr`, `ref`, and `void`. Second is that while all elements of a vector have an index relative to the start
of the vector, there is no requirement that individual elements be accessible by that index.

Also unlike arrays, vectors must be aligned to the nearest power of two larger or equal to their overall size. The overall size of a vector type is the size in bytes 
of the element type multiplied by the vector's length.

Vector values are encoded by encoding each of their elements in order of increasing index.

#### 1.5 Type Encoding

In general, types themselves are encoded as _type indices_. The grammar for a type index is as follows:

```
typeidx     :=  -1:i8    (i8)
            |   -2:i8    (i16)
            |   -3:i8    (i32)
            |   -4:i8    (i64)
            |   -5:i8    (iword)
            |   -6:i8    (ptr)
            |   -7:i8    (f32)
            |   -8:i8    (f64)
            |   -9:i8    (u8)
            |   -10:i8   (u16)
            |   -11:i8   (u32)
            |   -12:i8   (u64)
            |   -13:i8   (uword)
            |   -14:i8   (ref)
            |   -15:i8   (void)
            |   id:i64
```

Negative type indices are reserved for primitive types, while nonnegative indices represent compound types. Specifically, they index into the module's type table,
where compound type definitions are stored. The grammar for type definitions is as follows:

```
typedef     :=  0:i8 arraydef
            |   1:i8 tupledef
            |   2:i8 funcdef
            |   3:i8 vectordef

arraydef    := element:typeidx length:u64 

tupledef    := length:u64 members...:typeidx

funcdef     := return:typeidx length:u64 parameters...:typeidx

vectordef   := element:typeidx length:u64
```

## 2 Functions

Functions are the basic structure of executable Jasmine code. Every function is declared with a text name and a function type. Functions may be implemented directly
in a Jasmine source, or imported from another module.

#### 2.1 Instructions

When a function is implemented, that implementation is made up of _instructions_. Instructions are the fundamental unit of executable Jasmine code, representing 
operations over values and memory. The behavior of an instruction is defined by its _opcode_, a number representing a unique operation Jasmine supports. This behavior
is further refined by the instruction's _type_, which defines the type of operands the function operates over. Instructions take zero or more _source operands_, 
depending on the _arity_ of the opcode, and may write the operation's result to a _destination_. Instead of the destination being encoded in the instruction directly, 
the index of the instruction within the containing function may be used to identify the value produced by that instruction, referred to as a _local_.

Source operands can take several distinct forms within Jasmine instructions:

| Name | Example | Description |
|---|---|---|
| None | | The absence of an operand, used to encode unary instructions. |
| Register | `%0` | A local virtual register, referring to the result of another instruction inside the containing function by index. |
| Immediate | `42`, `1.3`, `'a'` | An integer or floating-point constant. |
| Label | `.0` | The address of a label within the current function. |
| Data | `foo` | The address of a constant data entry defined within the current module. |
| Static | `bar` | The address of a non-constant data entry defined within the current module. |
| Function | `baz` | The address of a function defined within the current module. |
| Variadic | `(1, %0)` | Multiple source operands packed together after the instruction. Only permitted by certain arities. |

Depending on the opcode, instructions also have one of several arities, which must be one of the following:

| Name | # Operands | Description |
|---|---|---|
| Nullary | 0 | The instruction takes no operands. |
| Unary | 1 | The instruction takes one operand. |
| Binary | 2 | The instruction takes two operands. |
| Unary* | Variable | The instruction's first operand is variadic, followed by zero or more additional operands. |
| Binary* | Variable | The instruction has two operands, where the second operand is variadic, followed by zero or more additional operands. |

#### 2.2 Opcodes

| Opcode | Arity | Type | Description |
|---|---|---|---|
| `nop` | Nullary | `nop : () -> void` | Does nothing. |
| `var` | Nullary | `var a : () -> a` | Declares a new local of the specified type. |
| `neg` | Unary | `neg a : a -> a` where `a` is numeric or a vector | Negates its operand and returns the result. |
| `not` | Unary | `not a : a -> a` where `a` is an integer | Performs a bitwise NOT on the bits of its operand and returns the result. |
| `addr` | Unary | `addr a : a -> ptr` | Computes the address of its operand. |
| `load` | Unary | `load a : ptr -> a` | Loads the value at the address provided by the first operand. |
| `new` | Unary | `new a : a -> ref` | Allocates a value on the heap and returns its reference. |
| `cast` | Unary | `cast a : b -> a` | Reinterprets the bits of its operand as the desired type. |
| `conv` | Unary | `conv a : b -> a` | Converts its operand to the desired type. |
| `zxt` | Unary | `zxt a : b -> a` | Zero-extends its operand to the desired type. |
| `sxt` | Unary | `sxt a : b -> a` | Sign-extends its operand to the desired type. |
| `ret` | Unary | `ret a : a -> void` | Returns from the current function with the first operand as a return value. |
| `par` | Unary | `par a : iword -> a` | Loads the nth parameter into the result. |
| `jump` | Unary | `jump : ptr -> void` | Jumps unconditionally to the provided address. |
| `label` | Unary | `label : iword -> void` | Defines the label with the provided index. |
| `mov` | Unary | `mov a : a -> a` | Defines the result as equal to the operand. |
| `phi` | Unary* | `phi a : (a...) -> a` | Selects between the variadic operands based on preceding control flow. |
| `add` | Binary | `add a : (a, a) -> a` where `a` is numeric, an address, or a vector | Adds its operands together and returns the sum. |
| `sub` | Binary | `sub a : (a, a) -> a` where `a` is numeric, an address, or a vector | Subtracts its second operand from the first and returns the difference. |
| `mul` | Binary | `mul a : (a, a) -> a` where `a` is numeric or a vector | Multiplies its operands and returns the product. |
| `div` | Binary | `div a : (a, a) -> a` where `a` is numeric or a vector | Divides its first operand by the second and returns the quotient. |
| `rem` | Binary | `rem a : (a, a) -> a` where `a` is numeric or a vector | Divides its first operand by the second and returns the remainder. |
| `and` | Binary | `and a : (a, a) -> a` where `a` is an integer | Returns the bitwise AND of the bits of its operands. |
| `or` | Binary | `or a : (a, a) -> a` where `a` is an integer | Returns the bitwise OR of the bits of its operands. |
| `xor` | Binary | `xor a : (a, a) -> a` where `a` is an integer | Returns the bitwise XOR of the bits of its operands. |
| `shl` | Binary | `shl a : (a, a) -> a` where `a` is an integer | Returns the first operand shifted to the left by the second operand. |
| `shr` | Binary | `shr a : (a, a) -> a` where `a` is an integer | Returns the first operand shifted to the right by the second operand. Signed integers are shifted arithmetically, while unsigned integers are shifted logically. |
| `store` | Binary | `store a : (ptr, a) -> void` | Writes the second operand into memory at the address provided by the first operand. |
| `elem` | Binary | `elem a: (a, b) -> ptr` where `a` is an array or tuple and `b` is an integer | Computes the address of the nth element or member of its first operand. |
| `newa` | Binary | `newa a : (a, b) -> ref` where `b` is an integer | Allocates an array of values on the heap and returns its reference. |
| `eq` | Binary | `eq a : (a, a) -> i8` where `a` is a primitive or vector | Returns nonzero if its operands are equal. |
| `neq` | Binary | `neq a : (a, a) -> i8` where `a` is a primitive or vector | Returns nonzero if its operands are not equal. |
| `lt` | Binary | `lt a : (a, a) -> i8` where `a` is a primitive or vector | Returns nonzero if the first operand is less than the second operand. |
| `leq` | Binary | `leq a : (a, a) -> i8` where `a` is a primitive or vector | Returns nonzero if the first operand is less than or equal to the second operand. |
| `gt` | Binary | `gt a : (a, a) -> i8` where `a` is a primitive or vector | Returns nonzero if the first operand is greater than the second operand. |
| `geq` | Binary | `geq a : (a, a) -> i8` where `a` is a primitive or vector | Returns nonzero if the first operand is greater than or equal to the second operand. |
| `jz` | Binary | `jz : (ptr, i8) -> void` | Jumps to the first operand if the second operand is zero. |
| `jnz` | Binary | `jnz : (ptr, i8) -> void` | Jumps to the first operand if the second operand is not zero. |
| `call` | Binary* | `call (a...) -> b : (ptr, a...) -> b` | Invokes the first operand on the provided variadic operands and returns the result. |

#### 2.3 Encoding

Operands to instructions are encoded using indices or raw constants. These are encoded the same as their specified types: 32-bit and 64-bit floating
point numbers are written literally, while all other numbers are encoded using LEB128.

```
localidx := i64
labelidx := i64
dataidx := i64
statidx := i64
funcidx := i64

operand :=  local:localidx              (Register)
        |   imm:i64                     (Immediate integer or pointer)
        |   imm:f32                     (Immediate 32-bit float)
        |   imm:f64                     (Immediate 64-bit float)
        |   label:labelidx              (Label)
        |   data:dataidx                (Constant data)
        |   stat:statidx                (Non-constant data)
        |   func:funcidx                (Function)
```

Opcodes are encoded as single bytes.

```
opcode  :=  0:i8    (nop)
        |   1:i8    (var)
        |   2:i8    (neg)   
        |   3:i8    (not)
        |   4:i8    (find)
        |   5:i8    (load)
        |   6:i8    (new)
        |   7:i8    (cast)
        |   8:i8    (conv)
        |   9:i8    (zxt)
        |   10:i8   (sxt)
        |   11:i8   (ret)
        |   12:i8   (par)
        |   13:i8   (jump)
        |   14:i8   (label)
        |   15:i8   (mov)
        |   16:i8   (phi)
        |   17:i8   (add)
        |   18:i8   (sub)
        |   19:i8   (mul)
        |   20:i8   (div)
        |   21:i8   (rem)
        |   22:i8   (and)
        |   23:i8   (or)
        |   24:i8   (xor)
        |   25:i8   (shl)
        |   26:i8   (shr)
        |   27:i8   (store)
        |   28:i8   (findf)
        |   29:i8   (newa)
        |   30:i8   (eq)
        |   31:i8   (neq)
        |   32:i8   (lt)
        |   33:i8   (leq)
        |   34:i8   (gt)
        |   35:i8   (geq)
        |   36:i8   (jz)
        |   37:i8   (jnz)
        |   38:i8   (call)
```

All instructions start with an opcode and a type, the latter encoded as a type index in LEB128. For non-nullary instructions, this
is followed by a single "argdesc" byte, which encodes the kinds of operands that follow the instruction header. Each of these kinds
is a 4-bit number, as follows:

```
argkind :=  0:i8    (None)
        |   1:i8    (Register)
        |   2:i8    (Immediate)
        |   3:i8    (Label)
        |   4:i8    (Constant data)
        |   5:i8    (Non-constant data)
        |   6:i8    (Function)
        |   7:i8    (Variadic)
```

For an instruction with two source operands, the "argdesc" byte is computed by shifting the left operand's kind number left by 4 bits, then computing
the bitwise OR between the two kind numbers. For unary and unary* instructions, the right operand kind is always None. For unary* instructions, the left
operand is always Variadic. For binary* instructions, the right operand is always Variadic.

Once the "argdesc" byte is computed, the instruction is encoded as follows:

```
insn    :=  nullary | unary | unary* | binary | binary*

nullary :=  op:opcode type:typeidx

unary   :=  op:opcode type:typeidx argdesc:i8 arg:operand

unary*  :=  op:opcode type:typeidx argdesc:i8 count:u16 varargs...:operand

binary  :=  op:opcode type:typeidx argdesc:i8 lhs:operand rhs:operand

binary* :=  op:opcode type:typeidx argdesc:i8 lhs:operand count:u16 varargs...:operand
```

Instructions can only occur within function implementations. Functions, as mentioned earlier, can either be implemented in the module, or imported from
elsewhere. The name of the function is encoded as an index into the module's string table (see 3.2). If the function is implemented locally, its instructions
are encoded directly following the function header. If the function is imported from elsewhere, the module name to import it from follows the function
header, encoded as an index into the string table.

```
stridx  :=  idx:i64

funcdef :=  name:stridx type:typeidx 00:i8 n_insns:u64 code...:insn (Implemented function)
        |   name:stridx type:typeidx 01:i8 module:stridx            (Imported function)
```

## 3 Object Format

#### 3.1 Module Layout 

The fundamental unit of a Jasmine project is the _module_. Modules define _symbols_, which may name constant data, non-constant data, or functions.
Symbols are either locally defined, with a value or implementation encoded in the module binary, or may be imported from another module by name. In addition
to code and data sections, Jasmine modules are also semantically versions, and can contain arbitrary text metadata describing or annotating the module.

The contents of a Jasmine module are divided into _sections_. Each Jasmine module has exactly one of each section type, even if some sections are empty of
definitions or meaningful information. Sections are laid out within a Jasmine module like so:

| Section | Description |
|---|---|
| String table | Contains all text names referenced in the module's sections. |
| Meta table | Contains metadata about the module. |
| Type table | Contains definitions for all of the compound types in the module. |
| Data table | Defines constant data entries. |
| Static table | Defines non-constant data entries, which may be initialized or uninitialized. |
| Code table | Defines the functions referenced by the module, which are either implemented in the module or imported externally. |

#### 3.2 String Table

The string table contains a list of text strings. Other sections of a Jasmine module refer to these strings by index, simplifying encoding elsewhere in the
module, and allowing the reuse of identical strings. Strings in the string table should be valid UTF-8, but it's not required that an implementation detect
or refuse to use strings that are not encoded in this format.

Strings in the string table are encoded as an integer length (in bytes), followed by that number of bytes. The string table overall consists of these strings
laid out consecutively, via the following grammar:

```
string  :=  length:u64 bytes...:i8

strtbl  :=  n_strings:u64 strings...:string
```

#### 3.3 Meta Table

The meta table contains descriptive information about a Jasmine module. All meta tables are required to contain at least the following:

 - A semantic version (major.minor.patch).
 - A Jasmine encoding version (currently `1`, but may increase in the future if breaking changes to the encoding occur).
 - The name of the module, as an index into the string table.
 - The entry point of the module, as an index into the code table (see 3.7). Use a negative index to indicate that the module has no entry point.

In addition to these properties, the meta table can store an arbitrary amount of auxiliary information in the form of key-value string pairs. This information
is stored directly in the meta table section, rather than as indices into the string table.

```
version :=  major:u16 minor:u16 patch:u16

keyval  :=  key:string value:string

metatbl :=  ver:version encoding:u16 name:stridx entry:funcidx n_entries:u64 info...:keyval
```

#### 3.4 Type Table

The type table contains definitions for all types referenced in a Jasmine module.

\*\* to be continued \*\*