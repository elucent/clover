#include "lib/gc.h"

struct foo {
    u8 bytes[64];
};

int main(int argc, char** argv) {
    foo* foos[65536];
    foo* ptr;
    i64 i = 0;
    while (true) {
        (i % 128 ? ptr : foos[i / 128 % 65536]) = (foo*)gc_alloc_untyped(64);
        i ++;
        if (i == 838860800) break;
    }
    return 0;
}