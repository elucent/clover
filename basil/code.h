#ifndef BASIL_BASIL_CODE_H
#define BASIL_BASIL_CODE_H

#include "basil/basil.h"
#include "core/def.h"

MODULE(basil)

enum Bytecode {
    TRAP        = 0x00, // Crash
    
    // Constants
    NIL         = 0x01, // 0x01                 [ -> nil]   Push nil value
    INT8        = 0x02, // 0x02     <i8>        [ -> int]   Push integer value
    INT16       = 0x03, // 0x03     <i16>       [ -> int]   Push integer value
    INT32       = 0x04, // 0x04     <i32>       [ -> int]   Push integer value
    INT         = 0x05, // 0x05     <i64>       [ -> int]   Push integer value
    NUM         = 0x06, // 0x06     <f64>       [ -> num]   Push number value
    SYM         = 0x07, // 0x07     <i32>       [ -> sym]   Push symbol value
    ZEROINT     = 0x08, // 0x08                 [ -> int]   Push integer zero
    ZERONUM     = 0x09, // 0x09                 [ -> num]   Push number zero
    ONEINT      = 0x0a, // 0x0a                 [ -> int]   Push integer one
    ONENUM      = 0x0b, // 0x0b                 [ -> num]   Push number one
    MINUSONEINT = 0x0c, // 0x0c                 [ -> int]   Push integer minus one
    MINUSONENUM = 0x0d, // 0x0d                 [ -> num]   Push number minus one

    // List Manipulation
    CONS        = 0x10, // 0x10                 [any nil|list -> list]    Create cons cell
    LISTOFN     = 0x11, // 0x11     <i32>       [any{n} -> list]          Create list of top N elements 
    LISTOF      = 0x12, // 0x12                 [any{n} int -> list]      Pop count, create list of top count elements below that
    CAR         = 0x13, // 0x13                 [list -> any]             Get head of list
    CDR         = 0x14, // 0x14                 [list -> nil|list]        Get tail of list
    CADR        = 0x15, // 0x15                 [list -> any]             Get head of tail of list
    CDDR        = 0x16, // 0x16                 [list -> nil|list]        Get tail of tail of list
    CADDR       = 0x17, // 0x17                 [list -> any]             Get head of tail of tail of list
    MAP         = 0x18, // 0x18                 [function list -> list]   Apply function to each element of list and return result
    FMAP        = 0x19, // 0x19                 [function list -> list]   Apply function to each non-null element of list and return result
    FILTER      = 0x1a, // 0x1a                 [function list -> list]   Remove all elements of list that fail predicate
    REDUCE      = 0x1b, // 0x1b                 [reduce list -> any]      Reduce elements of list pairwise from the left

    // Record Manipulation
    NEWREC      = 0x20, // 0x20                 [ -> rec]                 Push new empty record
    DERIVE      = 0x21, // 0x21                 [rec -> rec]              Push new empty record descending from top record
    GETREC      = 0x22, // 0x22                 [rec sym -> any]          Pop symbol, look it up in record, push associated value
    GETKNOWNREC = 0x23, // 0x23     <i32>       [rec -> any]              Look up known symbol in record, push value
    SETREC      = 0x24, // 0x24                 [rec any sym -> ]         Set symbol in record
    SETKNOWNREC = 0x25, // 0x25     <i32>       [rec any -> ]             Set known symbol in record
    CURENV      = 0x26, // 0x26                 [ -> rec]                 Load current lexical environment
    UNIFY       = 0x27, // 0x27                 [rec rec -> rec]          Unify top record down onto below record
    COPYREC     = 0x28, // 0x28                 [rec -> rec rec]          Duplicate top record value
    GETENV      = 0x29, // 0x29                 [sym -> any]              Look up symbol in lexical environment
    GETKNOWNENV = 0x2a, // 0x2a     <i32>       [ -> any]                 Look up known symbol in lexical environment
    SETENV      = 0x2b, // 0x2b                 [sym any -> any]          Set symbol in lexical environment
    SETKNOWNENV = 0x2c, // 0x2c     <i32>       [any -> ]                 Set known symbol in lexical environment

    // Numerics
    ADD         = 0x30, // 0x30                 [any any -> any]          Add top two values and push sum
    ADDINT      = 0x31, // 0x31                 [int int -> int]          Add top two integers and push sum
    ADDNUM      = 0x32, // 0x32                 [num num -> num]          Add top two numbers and push sum
    ADDICONST   = 0x33, // 0x33     <i64>       [int -> int]              Add integer constant to top integer
    ADDFCONST   = 0x34, // 0x34     <f64>       [num -> num]              Add number constant to top integer
    SUMN        = 0x35, // 0x35     <i32>       [any{n} -> any]           Add top N values
    INTSUMN     = 0x36, // 0x36     <i32>       [int{n} -> int]           Add top N integers
    NUMSUMN     = 0x37, // 0x37     <i32>       [num{n} -> int]           Add top N numbers
    SUMOF       = 0x38, // 0x38                 [any{n} int -> any]       Add top N values
    INTSUMOF    = 0x39, // 0x39                 [int{n} int -> int]       Add top N numbers
    NUMSUMOF    = 0x3a, // 0x3a                 [num{n} int -> num]       Add top N numbers
    SUB         = 0x3b, // 0x3b                 [any any -> any]          Subtract top value from value beneath and push difference
    SUBINT      = 0x3c, // 0x3c                 [int int -> int]          Subtract integers
    SUBNUM      = 0x3d, // 0x3d                 [num num -> num]          Subtract numbers
    SUBICONSTL  = 0x3e, // 0x3e     <i64>       [int -> int]              Subtract top integer from constant
    SUBICONSTR  = 0x3f, // 0x3f     <i64>       [int -> int]              Subtract constant from top integer
    SUBFCONSTL  = 0x40, // 0x40     <f64>       [num -> num]              Subtract top number from constant
    SUBFCONSTR  = 0x41, // 0x41     <f64>       [num -> num]              Subtract constant from top number
    MINUS       = 0x42, // 0x42                 [any -> any]              Negate top value
    MINUSINT    = 0x43, // 0x43                 [int -> int]              Negate top integer
    MINUSNUM    = 0x44, // 0x44                 [num -> num]              Negate top number
    DIFFN       = 0x45, // 0x45     <i32>       [any{n} -> any]           Find left-associative difference of top N values
    INTDIFFN    = 0x46, // 0x46     <i32>       [int{n} -> int]           Find left-associative difference of top N integers
    NUMDIFFN    = 0x47, // 0x47     <i32>       [num{n} -> num]           Find left-associative difference of top N numbers
    DIFFOF      = 0x48, // 0x48                 [any{n} int -> any]       Find left-associative difference of top N numbers
    INTDIFFOF   = 0x49, // 0x49                 [int{n} int -> any]       Find left-associative difference of top N numbers
    NUMDIFFOF   = 0x4a, // 0x4a                 [num{n} int -> any]       Find left-associative difference of top N numbers
    MUL         = 0x4b, // 0x4b                 [any any -> any]          Multiply top two values and push product
    MULINT      = 0x4c, // 0x4c                 [int int -> int]          Multiply top two integers
    MULNUM      = 0x4d, // 0x4d                 [num num -> num]          Multiply top two numbers
    MULICONST   = 0x4e, // 0x4e     <i64>       [int -> int]              Multiply top integer by constant
    MULFCONST   = 0x4f, // 0x4f     <f64>       [num -> num]              Multiply top number by constant
    PRODN       = 0x50, // 0x50     <i32>       [any{n} -> any]           Multiply top N values
    INTPRODN    = 0x51, // 0x51     <i32>       [int{n} -> int]           Multiply top N integers
    NUMPRODN    = 0x52, // 0x52     <i32>       [num{n} -> num]           Multiply top N numbers
    PRODOF      = 0x53, // 0x53                 [any{n} int -> any]       Multiply top N values
    INTPRODOF   = 0x54, // 0x54                 [int{n} int -> any]       Multiply top N integers
    NUMPRODOF   = 0x55, // 0x55                 [num{n} int -> any]       Multiply top N numbers
    DIV         = 0x56, // 0x56                 [any any -> any]          Divide top two values
    DIVINT      = 0x57, // 0x57                 [int int -> int]          Divide top two integers
    DIVNUM      = 0x58, // 0x58                 [num num -> num]          Divide top two numbers
    DIVICONSTL  = 0x59, // 0x59     <i64>       [int -> int]              Divide integer constant by top integer
    DIVICONSTR  = 0x5a, // 0x5a     <i64>       [int -> int]              Divide top integer by constant
    DIVFCONSTL  = 0x5b, // 0x5b     <f64>       [num -> num]              Divide top number by constant
    DIVFCONSTR  = 0x5c, // 0x5c     <f64>       [num -> num]              Divide top number by constant
    RECIP       = 0x5d, // 0x5d                 [any -> any]              Reciprocal top value
    RECIPINT    = 0x5e, // 0x5e                 [int -> int]              Reciprocal top integer
    RECIPNUM    = 0x5f, // 0x5f                 [num -> num]              Reciprocal top number
    QUOTN       = 0x60, // 0x60     <i32>       [any{n} -> any]           Find left-associative quotient of top N values
    INTQUOTN    = 0x61, // 0x61     <i32>       [int{n} -> int]           Find left-associative quotient of top N integers
    NUMQUOTN    = 0x62, // 0x62     <i32>       [num{n} -> num]           Find left-associative quotient of top N numbers
    QUOTOF      = 0x63, // 0x63                 [any{n} int -> any]       Find left-associative quotient of top N values
    INTQUOTOF   = 0x64, // 0x64                 [int{n} int -> int]       Find left-associative quotient of top N integers
    NUMQUOTOF   = 0x65, // 0x65                 [num{n} int -> num]       Find left-associative quotient of top N numbers
    MOD         = 0x66, // 0x66                 [any any -> any]          Find modulus of top two values
    REM         = 0x67, // 0x67                 [any any -> any]          Find remainder of top two values
    MODINT      = 0x68, // 0x68                 [int int -> int]          Find integer modulus of top two values
    MODNUM      = 0x69, // 0x69                 [num num -> num]          Find modulus of top two numbers
    REMINT      = 0x6a, // 0x6a                 [int int -> int]          Find remainder of top two integers
    REMNUM      = 0x6b, // 0x6b                 [num num -> num]          Find remainder of top two numbers
    MODICONSTR  = 0x6c, // 0x6c     <i64>       [int -> int]              Find top integer modulo constant
    MODFCONSTR  = 0x6d, // 0x6d     <f64>       [num -> num]              Find top number modulo constant
    REMICONSTR  = 0x6e, // 0x6e     <i64>       [int -> int]              Find remainder of top integer divded by constant
    REMFCONSTR  = 0x6f, // 0x6f     <f64>       [num -> num]              Find remainder of top number divided by constant

    // Stack Manipulation
    DUP         = 0x70, // 0x20                 [any -> any any]          Duplicate top value
    DROP        = 0x71, // 0x21                 [any -> ]                 Delete top value
    SWAP        = 0x72, // 0x22                 [any any -> any any]      Swap two top values

    // Comparisons and control flow
    EQUAL       = 0x80, // 0x80                 [any any -> any]          Compare top two values for equality
    NOTEQUAL    = 0x81, // 0x81                 [any any -> any]          Compare top two values for inequality
    LESS        = 0x82, // 0x82                 [any any -> any]          Compare top two values by strict less than
    LEQUAL      = 0x83, // 0x83                 [any any -> any]          Compare top two values by less or equal
    GREATER     = 0x84, // 0x84                 [any any -> any]          Compare top two values by strict greater
    GEQUAL      = 0x85, // 0x85                 [any any -> any]          Compare top two values by greater or equal
    INTLESS     = 0x86, // 0x86                 [int int -> int]          Compare top two integers by strict less than
    INTLEQUAL   = 0x87, // 0x87                 [int int -> int]          Compare top two integers by less or equal
    INTGREATER  = 0x88, // 0x88                 [int int -> int]          Compare top two integers by strict greater
    INTGEQUAL   = 0x89, // 0x89                 [int int -> int]          Compare top two integers by greater or equal
    NUMLESS     = 0x8a, // 0x8a                 [num num -> num]          Compare top two numbers by strict less than
    NUMLEQUAL   = 0x8b, // 0x8b                 [num num -> num]          Compare top two numbers by less or equal
    NUMGREATER  = 0x8c, // 0x8c                 [num num -> num]          Compare top two numbers by strict greater
    NUMGEQUAL   = 0x8d, // 0x8d                 [num num -> num]          Compare top two numbers by greater or equal
    JUMPKNOWN   = 0x8e, // 0x8e     <i32>       [ -> ]                    
};

ENDMODULE()

#endif