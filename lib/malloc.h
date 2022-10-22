#ifndef BASIL_LIB_MALLOC_H
#define BASIL_LIB_MALLOC_H

#include "core/def.h"
#include "core/sys.h"
#include "lib/array.h"
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
            mfree(pages);
            pages.ptr = prev;
            prev = *(page**)pages.ptr;
        }
        mfree(pages);
    }

    iptr alloc_block(iptr bytes);
    iptr alloc_huge(iptr bytes);

    inline iptr alloc(iptr bytes) {
        bytes = bytes + 7 & ~7;
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
        pages = mreq(size);
        top = (u8*)pages.ptr;
        *(page**)top = nullptr;
        *((u8**)top + 1) = nullptr;
        top += sizeof(iptr) * 2;
    }   
};

/*
 * page_freeing_ptr
 *
 * Pointer wrapper that automatically unmaps a slice of pages at the pointed-to address.
 */
struct page_freeing_ptr {
    slice<page> ptr;

    ~page_freeing_ptr();
};

/*
 * allocator
 *
 * Random-access allocator/deallocator for arbitrary-lifetime data. Data allocated
 * by this allocator persists as long as the allocator is alive, but data can be
 * allocated or freed anywhere within the heap regions managed by this allocator.
 */
struct allocator {
    static allocator* instance;

    static iptr lock;
    static constexpr const iptr N_SIZES = 11; // small objects can be between 16 and 32768 bytes 
    iptr* free_lists[N_SIZES];
    iptr block_size; // size of arena blocks
    arena blocks; // blocks that have been allocated

    allocator(iptr block_size = 1048576);
    allocator(const allocator& other) = delete;
    allocator& operator=(const allocator& other) = delete;
    ~allocator();

    inline iptr* new_block(uptr size) {
        iptr* list = nullptr;
        slice<page> region = mreq(block_size / PAGESIZE);
        u8* uptr_region = (u8*)region.ptr;
        for (u8* p = (u8*)region.ptr + block_size - size; p >= (u8*)region.ptr; p -= size) {
            *(iptr**)p = list;
            list = (iptr*)p;
        }
        ((page_freeing_ptr*)blocks.alloc(sizeof(page_freeing_ptr)))->ptr = region;
        return list;
    }

    inline iptr alloc(iptr bytes) {
        bytes += sizeof(iptr);
        iptr size = 16, i = 0;
        while (size < bytes) size *= 2, i ++;
        iptr* val;
        if (i >= N_SIZES) {
            slice<page> block = mreq(size / PAGESIZE);
            val = (iptr*)block.ptr;
        }
        else {
            val = free_lists[i];
            if (!val) val = new_block(size);
            free_lists[i] = (iptr*)(*val);
        }

        *val = bytes;
        return iptr(val + 1);
    }

    inline void free(void* ptr){
        iptr bytes = *((iptr*)ptr - 1);
        iptr size = 16, i = 0;
        while (size < bytes) size *= 2, i ++;
        if (i >= N_SIZES) return mfree({ (page*)((iptr*)ptr - 1), size / PAGESIZE });
        
        iptr* list = free_lists[i];
        *((iptr**)ptr - 1) = list;
        free_lists[i] = ((iptr*)ptr - 1);
    }
};

void* operator new(size_t bytes);
void operator delete(void* ptr);
void* operator new[](size_t bytes);
void operator delete[](void* ptr);

void* operator new(size_t, void* ptr);

void* operator new(size_t bytes, arena& a);
void operator delete(void* ptr, arena& a);
void* operator new[](size_t bytes, arena& a);
void operator delete[](void* ptr, arena& a);

void* operator new(size_t bytes, allocator& a);
void operator delete(void* ptr, allocator& a);
void* operator new[](size_t bytes, allocator& a);
void operator delete[](void* ptr, allocator& a);

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