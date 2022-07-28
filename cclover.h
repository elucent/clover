#ifndef CLOVER_C_PRELUDE_H
#define CLOVER_C_PRELUDE_H

#include "stdint.h"
#include "core/def.h"

typedef intptr_t iconst;
typedef double fconst;
typedef int8_t unit;
typedef int8_t bool_t;
typedef float f32;
typedef double f64;

#ifdef __cplusplus
#define CLINKAGE "C"
#else
#define CLINKAGE
#endif

extern CLINKAGE void* mcpy(void*, const void*, uintptr_t);
extern CLINKAGE void* mset(void*, u8, uintptr_t);
extern CLINKAGE intptr_t mcmp(const void*, const void*, uintptr_t);

typedef struct {
    const i8* data;
    intptr_t size;
} string;

extern CLINKAGE iptr $strlen(string s);

inline i64 $strcmp(string a, string b) {
    i64 smaller = a.size < b.size ? a.size : b.size;
    iptr result = mcmp(a.data, b.data, smaller); // Valid because strings are still null-terminated for now.
    if (!result && a.size != b.size) return $strlen(a) - $strlen(b);
    else return result;
}

#define $new(T, x) T ## $_pnew((T*)$malloc(sizeof(T)), x)
#define $newarray(T, x, n) T ## $_anew(x, n)
#define $clover_array_def(T, CT, n) typedef struct { CT ptr[n]; } CT ## $A$ ## n ;
#define $clover_slice_def(T, CT) typedef struct { CT* ptr; intptr_t size; } CT ## $S ; \
inline CT ## $S T ## $S ## $iter(CT ## $S sl) { return sl; } \
inline bool_t T ## $S ## $empty(CT ## $S sl) { return sl.size == 0; } \
inline CT T ## $S ## $read(CT ## $S sl) { return sl.ptr[0]; } \
inline CT ## $S T ## $S ## $next(CT ## $S sl) { return (CT ## $S){sl.ptr + 1, sl.size - 1}; }

$clover_slice_def(i8, i8)
inline string $bytes_to_str(i8$S bytes) { return (string){ bytes.ptr, bytes.size }; }
inline i8$S $str_to_bytes(string str) { return (i8$S){ (i8*)str.data, str.size }; }

// Init and deinit of libcore.
extern CLINKAGE void $core_init();
extern CLINKAGE void $core_deinit();

// Formatting for various values.
extern CLINKAGE void int$print(iword);
extern CLINKAGE void float$print(double);
extern CLINKAGE void unit$print(unit);
extern CLINKAGE void bool$print(bool_t);
extern CLINKAGE void char$print(i32);
extern CLINKAGE void string$print(string);

// Exponent function.
extern CLINKAGE i8 i8$$pow(i8, i8);
extern CLINKAGE i16 i16$$pow(i16, i16);
extern CLINKAGE i32 i32$$pow(i32, i32);
extern CLINKAGE i64 i64$$pow(i64, i64);
extern CLINKAGE f32 f32$$pow(f32, f32);
extern CLINKAGE f64 f64$$pow(f64, f64);

// Control and memory management.
extern CLINKAGE void pexit(iword);
extern CLINKAGE iword $malloc(iword);
extern CLINKAGE void $del(void*);

#endif