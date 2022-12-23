#ifndef BASIL_LIB_MALLOC_H
#define BASIL_LIB_MALLOC_H

#include "core/def.h"
#include "core/sys.h"
#include "lib/array.h"
#include "lib/gc.h"
#include "lib/io.h"
#include "lib/meta.h"
#include "lib/slice.h"
#include "lib/meta.h"

/*
 * arena
 *
 * Growable bump-allocator. Allows objects of any type to be allocated, but can
 * only be deallocated all at once.
 */
struct arena {
    static arena* instance;

    slice<page> pages;
    u8* top;
    iptr size;

    arena(iptr size = 256);
    ~arena();
    arena(const arena& other) = delete;
    arena& operator=(const arena& other) = delete;

    inline void free(void*) {} // free(void*) is a no-op for arenas

    inline void free() {
        page* prev = *(page**)pages.ptr;
        while (prev) {
            memory_free(pages);
            pages.ptr = prev;
            prev = *(page**)pages.ptr;
        }
        memory_free(pages);
    }

    iptr alloc_block(iptr bytes);
    iptr alloc_huge(iptr bytes);

    inline iptr alloc(iptr bytes) {
        bytes = (bytes + 7) & ~7;
        top += bytes;
        if (top <= get_top()) {
            return iptr(top - bytes);
        }
        else if (bytes >= size * PAGESIZE / 2) { // Big alloc
            return alloc_huge(bytes);
        }
        else return alloc_block(bytes);
    }

    inline u8* get_top() {
        return (u8*)pages.ptr + size * PAGESIZE;
    }

    inline void empty() {
        free();
        pages = memory_map(size);
        top = (u8*)pages.ptr;
        *(page**)top = nullptr;
        *((u8**)top + 1) = nullptr;
        top += sizeof(iptr) * 2;
    }
};

/*
 * gc_allocator
 *
 * Uses the libcore GC (gc.h/cpp) as a backend for alloc/free calls.
 */
struct gc_allocator {
    static gc_allocator* instance;

    inline void free(void* ptr) {
        gc_free_untyped(ptr);
    }

    inline void* alloc(iptr size) {
        return gc_alloc_untyped(size);
    }
};

using allocator = gc_allocator;

void* operator new(size_t bytes);
void operator delete(void* ptr) noexcept;
void* operator new[](size_t bytes);
void operator delete[](void* ptr) noexcept;

void* operator new(size_t, void* ptr) noexcept;

inline void* operator new(size_t bytes, arena& a) {
    return (void*)a.alloc(bytes);
}

inline void operator delete(void* ptr, arena& a) {
    //
}

inline void* operator new[](size_t bytes, arena& a) {
    return (void*)a.alloc(bytes);
}

inline void operator delete[](void* ptr, arena& a) {
    //
}

inline void* operator new(size_t bytes, allocator& a) {
    return (void*)a.alloc(bytes);
}

inline void operator delete(void* ptr, allocator& a) {
    a.free(ptr);
}

inline void* operator new[](size_t bytes, allocator& a) {
    return (void*)a.alloc(bytes);
}

inline void operator delete[](void* ptr, allocator& a) {
    a.free(ptr);
}

template<typename Alloc>
struct bindlocal {
    iptr old;
    bool isptr;
    u8 buf[sizeof(Alloc)];

    bindlocal(Alloc* alloc) {
        old = (iptr)Alloc::instance;
        isptr = true;
        Alloc::instance = alloc;
    }

    template<typename... Args>
    bindlocal(Args... args) {
        old = (iptr)Alloc::instance;
        isptr = false;
        Alloc::instance = new(buf) Alloc(args...);
    }

    ~bindlocal() {
        if (!isptr) Alloc::instance->~Alloc();
        Alloc::instance = (Alloc*)old;
    }
};

#endif