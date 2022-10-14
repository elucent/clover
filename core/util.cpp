#include "core/util.h"
#include "core/sys.h"

extern "C" iptr cidx(const i8* str, i8 val) {
    auto p = str;
    while (*p != val) p ++;
    return p - str;
}

extern "C" void panic(const i8* msg) {
    fdwrite(1, {msg, cidx(msg, '\0')});
    fdwrite(1, {"\n", 1});
    pexit(42);
}