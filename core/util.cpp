#include "core/util.h"
#include "core/sys.h"

extern "C" iptr cidx(const i8* str, i8 val) {
    auto p = str;
    while (*p != val) p ++;
    return p - str;
}

extern "C" void panic(const i8* msg) {
    file_write(1, {msg, cidx(msg, '\0')});
    file_write(1, {"\n", 1});
    process_exit(42);
}