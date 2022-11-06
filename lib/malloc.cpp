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

page_freeing_ptr::~page_freeing_ptr() {
    if (ptr.ptr) mfree(ptr);
}

allocator::allocator(iptr block_size_in): block_size(block_size_in), blocks(1) {
    for (iptr i = 0; i < N_SIZES; i ++) free_lists[i] = nullptr;
}

allocator::~allocator() {
    slice<page> pages = blocks.pages;
    page* prev = *(page**)pages.ptr;
    u8* prev_top = *((u8**)pages.ptr + 1);
    page_freeing_ptr* bot = (page_freeing_ptr*)((u8*)pages.ptr + sizeof(iptr) * 2);
    page_freeing_ptr* top = (page_freeing_ptr*)(blocks.top);
    while (prev) {
        while (bot < top) bot->~page_freeing_ptr(), bot ++;
        bot = (page_freeing_ptr*)((u8*)prev + sizeof(iptr) * 2);
        top = (page_freeing_ptr*)prev_top;
        prev_top = *((u8**)prev + 1);
        prev = *(page**)prev;
    }
    while (bot < top) bot->~page_freeing_ptr(), bot ++;
}

iptr allocator::lock = 0;
allocator* allocator::instance = nullptr;

void* operator new(size_t bytes) {
    return gc_alloc_untyped(bytes);
}

void operator delete(void* ptr) {
    gc_free_untyped(ptr);
}

void* operator new[](size_t bytes) {
    return gc_alloc_untyped(bytes);
}

void operator delete[](void* ptr) {
    gc_free_untyped(ptr);
}

void* operator new(size_t bytes, arena& a) {
    return (void*)a.alloc(bytes);
}

void operator delete(void* ptr, arena& a) {
    //
}

void* operator new[](size_t bytes, arena& a) {
    return (void*)a.alloc(bytes);
}

void operator delete[](void* ptr, arena& a) {
    //
}

void* operator new(size_t bytes, allocator& a) {
    return (void*)a.alloc(bytes);
}

void operator delete(void* ptr, allocator& a) {
    a.free(ptr);
}

void* operator new[](size_t bytes, allocator& a) {
    return (void*)a.alloc(bytes);
}

void operator delete[](void* ptr, allocator& a) {
    a.free(ptr);
}

void* operator new(size_t, void* ptr) {
    return ptr;
}