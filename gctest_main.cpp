#include "lib/gc.h"
#include "lib/io.h"

int main(int argc, char** argv) {
    iptr sizes[] = {
        8, 8, 16, 16, 16, 16, 24, 24, 32, 32, 48, 48, 64, 64, 96, 128,
        8, 8, 16, 16, 16, 16, 24, 24, 32, 32, 48, 48, 64, 64, 256, 512
    };

    void** foos = (void**)gc_alloc_untyped(sizeof(iptr) * 524288);
    //void* foos[524288];
    i64 i = 0;
    while (true) {
        foos[i % 524288] = gc_alloc_untyped(sizes[i % 32]);
        i ++;
        if (i == 838860800) break;
    }
    return 0;
}