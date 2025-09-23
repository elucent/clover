#ifndef UTILITY_ALLOC_H
#define UTILITY_ALLOC_H

#include "rt/def.h"
#include "rt/sys.h"
#include "util/io.h"
#include "util/math.h"

#define SLOW_GUARD_MALLOC 0
#define USE_ELUMALLOC 1

#if SLOW_GUARD_MALLOC

// Optional malloc hardening mode: every allocation is independently mapped,
// with guard pages. Allocations are randomly placed against the right or left
// edges of the allocated page, so we can try and catch out-of-bounds reads or
// writes in either direction.

namespace MallocLCG {
    constexpr uword
        m = 0x100000000u,
        a = 16854749770870987091ull,
        c = 15284698627294678789ull;
    extern uword x;

    inline uword next() {
        return x = (a * x + c) % m;
    }
};

inline ALWAYSINLINE void* malloc(uword bytes) {
    uword pages = divideRoundingUp<uword>(bytes + 16, memory::pagesize()) + 2;
    auto alloc = memory::map(pages * memory::pagesize());
    if (!alloc.data())
        return nullptr;

    // Guard pages.
    memory::tag(alloc.take(1), 0);
    memory::tag({ alloc.data() + 1, (iword)pages }, memory::READ | memory::WRITE);
    memory::tag(alloc.drop(alloc.size() - 1), 0);

    u8* base = (u8*)&alloc[1];
    if (MallocLCG::next() % 2) {
        base = (u8*)(alloc.end() - 1) - (bytes + 16);
        base = (u8*)(uptr(base) & ~15ull); // Align to 16 bytes.
    }
    *(void**)base = alloc.data();
    *((iword*)base + 1) = alloc.size();
    return base + 16;
}

inline ALWAYSINLINE void free(void* ptr) {
    memory::page* alloc_base = *((memory::page**)ptr - 2);
    iword size = *((iword*)ptr - 1);
    slice<memory::page> alloc = { alloc_base, size };
    memory::unmap(alloc);
}
#elif USE_ELUMALLOC
#include "util/elumalloc.h"
inline ALWAYSINLINE void* malloc(uword bytes) {
    return elumalloc::allocate(bytes);
}

inline ALWAYSINLINE void free(void* ptr) {
    return elumalloc::free(ptr);
}
#endif

void* operator new(size_t bytes);
void operator delete(void* ptr) noexcept;
void operator delete(void* ptr, size_t) noexcept;
void* operator new[](size_t bytes);
void operator delete[](void* ptr) noexcept;
void operator delete[](void* ptr, size_t) noexcept;

#endif