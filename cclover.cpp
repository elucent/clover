#include "cclover.h"
#include "core/sys.h"
#include "lib/gc.h"
#include "lib/malloc.h"
#include "lib/math.h"
#include "lib/io.h"
#include "lib/utf.h"

extern "C" i8 i8__pow(i8 n, i8 p) {
    return i8(ipow(n, p));
}

extern "C" i16 i16__pow(i16 n, i16 p) {
    return i16(ipow(n, p));
}

extern "C" i32 i32__pow(i32 n, i32 p) {
    return i32(ipow(n, p));
}

extern "C" i64 i64__pow(i64 n, i64 p) {
    return i64(ipow(n, p));
}

extern "C" f32 f32__pow(f32 n, f32 p) {
    return f32(fpow(n, p));
}

extern "C" f64 f64__pow(f64 n, f64 p) {
    return f64(fpow(n, p));
}

extern "C" void int__print(iword i) {
    print(i, '\n');
}

extern "C" void float__print(double i) {
    print(i, '\n');
}

extern "C" void unit__print(unit i) {
    print("()\n");
}

extern "C" void bool__print(bool_t i) {
    print(i ? "true\n" : "false\n");
}

extern "C" void char__print(i32 i) {
    print(rune(i), '\n');
}

extern "C" void string__print(string s) {
    print(const_slice<i8>{s.data, s.size}, '\n');
}

extern "C" iptr __clover__strlen(string s) {
    return utf8_length(s.data, s.size);
}

extern "C" void __clover__core_init() {
}

extern "C" void __clover__core_deinit() {
}

extern "C" iword __clover__malloc(iword size) {
    return (iptr)gc_alloc_untyped(size);
}

extern "C" void __clover__del(void* ptr) {
    gc_free_untyped(ptr);
}

// Libcore bindings.

extern CLINKAGE slice<page> memory__map(i64 n) {
    return mreq(n);
}

extern CLINKAGE void memory__unmap(slice<page> pages) {
    mfree(pages);
}

extern CLINKAGE void memory__tag(slice<page> pages, i8 flags) {
    mpermit(pages, flags);
}