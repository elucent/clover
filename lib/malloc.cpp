#include "malloc.h"
#include "core/sys.h"
#include "lib/gc.h"

arena::arena(iptr size_in): pages(mreq(size_in)), top((u8*)pages.ptr), size(size_in) { 
    *(page**)top = nullptr;
    *((u8**)top + 1) = nullptr;
    top += sizeof(iptr) * 2; 
}

arena::~arena() {
    free();
}

arena* arena::instance = nullptr;

iptr arena::alloc_block(iptr bytes) {
    slice<page> old_pages = pages;
    u8* old_top = top - bytes;
    pages = mreq(size);
    top = (u8*)pages.ptr;
    *((page**)top) = old_pages.ptr;
    *((u8**)top + 1) = old_top;
    top += sizeof(iptr) * 2 + bytes;
    return iptr(top - bytes);
}

iptr arena::alloc_huge(iptr bytes) {
    iptr n_pages = (bytes + sizeof(iptr) * 2 + PAGESIZE - 1) / PAGESIZE;
    slice<page> old_pages = pages;
    u8* old_top = top - bytes;
    pages = mreq(n_pages);
    top = (u8*)pages.ptr;
    *((page**)top) = old_pages.ptr;
    *((u8**)top + 1) = old_top;
    top += sizeof(iptr) * 2 + bytes;
    iptr result = iptr(top - bytes);

    bytes = 0;  // Allocate new empty arena after big block.
    old_pages = pages;
    old_top = top;
    pages = mreq(size);
    top = (u8*)pages.ptr;
    *((page**)top) = old_pages.ptr;
    *((u8**)top + 1) = old_top;
    top += sizeof(iptr) * 2;

    return result;
}

gc_allocator* gc_allocator::instance;

void* operator new(size_t bytes) {
    return gc_alloc_untyped(bytes);
}

void operator delete(void* ptr) noexcept {
    gc_free_untyped(ptr);
}

void* operator new[](size_t bytes) {
    return gc_alloc_untyped(bytes);
}

void operator delete[](void* ptr) noexcept {
    gc_free_untyped(ptr);
}

void* operator new(size_t, void* ptr) noexcept {
    return ptr;
}