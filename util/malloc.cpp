#include "util/malloc.h"
#include "util/io.h"

#if !USE_MIMALLOC
void* operator new(size_t bytes) {
    return malloc(bytes);
}

void operator delete(void* ptr) noexcept {
    free(ptr);
}

void operator delete(void* ptr, size_t) noexcept {
    free(ptr);
}

void* operator new[](size_t bytes) {
    return malloc(bytes);
}

void operator delete[](void* ptr) noexcept {
    free(ptr);
}

void operator delete[](void* ptr, size_t) noexcept {
    free(ptr);
}

#if SLOW_GUARD_MALLOC
namespace MallocLCG {
    uword x = 3353408407ull;
}
#endif

#endif