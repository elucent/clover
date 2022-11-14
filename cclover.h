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
typedef int32_t char_t;

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

extern CLINKAGE iptr __clover__strlen(string s);

inline i64 __clover__strcmp(string a, string b) {
    i64 smaller = a.size < b.size ? a.size : b.size;
    iptr result = mcmp(a.data, b.data, smaller); // Valid because strings are still null-terminated for now.
    if (!result && a.size != b.size) return __clover__strlen(a) - __clover__strlen(b);
    else return result;
}

#define __clover__new(T, x) T ## __pnew((T*)__clover__malloc(sizeof(T)), x)
#define __clover__newarray(T, x, n) T ## __anew(x, n)
#define __clover__array_def(T, CT, n) typedef struct { CT ptr[n]; } CT ## __A__ ## n ;
#define __clover__slice_def(T, CT) typedef struct { CT* ptr; intptr_t size; } CT ## __S ; \
inline CT ## __S T ## __S ## __iter(CT ## __S sl) { return sl; } \
inline bool_t T ## __S ## __empty(CT ## __S sl) { return sl.size == 0; } \
inline CT T ## __S ## __read(CT ## __S sl) { return sl.ptr[0]; } \
inline CT ## __S T ## __S ## __next(CT ## __S sl) { return (CT ## __S){sl.ptr + 1, sl.size - 1}; }

__clover__slice_def(i8, i8)
inline string __clover__bytes_to_str(i8__S bytes) { return (string){ bytes.ptr, bytes.size }; }
inline i8__S __clover__str_to_bytes(string str) { return (i8__S){ (i8*)str.data, str.size }; }

// Init and deinit of libcore.
extern CLINKAGE void __clover__core_init();
extern CLINKAGE void __clover__core_deinit();

// Formatting for various values.
extern CLINKAGE void int__print(iword);
extern CLINKAGE void float__print(double);
extern CLINKAGE void unit__print(unit);
extern CLINKAGE void bool__print(bool_t);
extern CLINKAGE void char__print(i32);
extern CLINKAGE void string__print(string);

// Exponent function.
extern CLINKAGE i8 i8__pow(i8, i8);
extern CLINKAGE i16 i16__pow(i16, i16);
extern CLINKAGE i32 i32__pow(i32, i32);
extern CLINKAGE i64 i64__pow(i64, i64);
extern CLINKAGE f32 f32__pow(f32, f32);
extern CLINKAGE f64 f64__pow(f64, f64);

extern CLINKAGE i16 i8__u16(i8);
extern CLINKAGE i32 i8__u32(i8);
extern CLINKAGE i64 i8__u64(i8);
extern CLINKAGE i32 i16__u32(i16);
extern CLINKAGE i64 i16__u64(i16);
extern CLINKAGE i64 i32__u64(i32);

// Control and memory management.
extern CLINKAGE void pexit(iword);
extern CLINKAGE iword __clover__malloc(iword);
extern CLINKAGE void __clover__del(void*);

#endif