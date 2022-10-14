#include "lib/gc.h"
#include "lib/io.h"
#include "core/sys.h"
#include "core/util.h"

static iptr spbase;
gc_heap THE_HEAP;

extern "C" iptr _initial_sp;

void gc_init() {
    spbase = _initial_sp;
    new(&THE_HEAP) gc_heap();
}

struct gc_bitmap {
    i64* bits;
    iptr* base;

    inline gc_bitmap(): 
        base((iptr*)THE_HEAP.pages) {
        i64 bitmap_size = THE_HEAP.page_limit * WORDS_PER_PAGE * 2 / 8;
        bits = (i64*)mreq(bitmap_size / PAGESIZE).ptr;
        mset(bits, bitmap_size, 0);
    }

    inline void destroy() {
        i64 bitmap_size = THE_HEAP.page_limit * WORDS_PER_PAGE * 2 / 8;
        mfree({ (page*)bits, bitmap_size / PAGESIZE });
    }

    inline void mark(iptr ptr) {
        if (!ptr) return;
        if (ptr & 15) return;
        iptr offset = (iptr*)ptr - base;
        if (offset < 0 || offset >= THE_HEAP.page_limit * WORDS_PER_PAGE * 2ul)
            return;
        // print("    Marking "), write_hex(stdout, ptr), print(" at offset ", offset, '\n');
        bits[offset / 64] |= 1ul << offset % 64;
    }

    inline bool get(iptr ptr) const {
        iptr offset = (iptr*)ptr - base;
        return bits[offset / 64] & 1ul << offset % 64;
    }

    inline bool get(iptr ptr, i64 mask) const {
        uptr offset = (iptr*)ptr - base;
        if (!bits[offset / 64]) return false;
        uptr low = mask << offset % 64, high = mask >> (64 - offset % 64);
        bool isThere = false;
        if (low) isThere = isThere || bits[offset / 64] & low;
        if (offset % 64 && high) isThere = isThere || bits[offset / 64 + 1] & high;
        return isThere;
    }
};

inline void scan_region(gc_bitmap bitmap, iptr* start, iptr* end) {
    iptr* iter = start;
    while (iter != end)
        bitmap.mark(*iter ++);
}

void scan_page(gc_bitmap bitmap, iptr* start, iptr* end, i32 size_class, gc_stats& stats) {
    iptr i = 0;
    iptr mask = SIZE_CLASS_MASKS[size_class];
    iptr stride = SIZE_CLASSES[size_class] * 2;
    bool live = false;
    while (start + i < end) {
        if (bitmap.get((iptr)(start + i), mask)) {
            live = true;
            scan_region(bitmap, start + i, start + i + stride);
        }
        i += stride;
    }
    THE_HEAP.info[(gc_page*)start - THE_HEAP.pages].marked = true;
}

void scan_huge_page(gc_bitmap bitmap, gc_page* page_start, i32 n_pages, gc_stats& stats) {
    i32 idx = i32(page_start - THE_HEAP.pages);
    i32 wordidx = idx * WORDS_PER_PAGE * 2 / 8;
    bool live = false;
    for (i32 i = 0; i < WORDS_PER_PAGE * PTRS_PER_WORD * n_pages; i += 64) {
        if (bitmap.bits[(wordidx + i) / 64]) {
            live = true;
            break;
        }
    }
    if (live) {
        scan_region(bitmap, (iptr*)page_start, (iptr*)(page_start + n_pages));
        THE_HEAP.info[idx].marked = true;
    }
}

void free_page(gc_bitmap bitmap, iptr* start, iptr* end, i32 size_class, u32 idx, gc_stats& stats) {
    iptr i = 0;
    iptr mask = SIZE_CLASS_MASKS[size_class];
    iptr stride = SIZE_CLASSES[size_class] * 2;
    gc_page_info& info = THE_HEAP.info[idx];
    
    while (start + i < end) {
        if (!bitmap.get((iptr)(start + i), mask)) {
            // print("  Freeing "), write_hex(stdout, (iptr)(start + i)), print('\n');
            // if (!(info.occupancy -= stride / 2)) {
            //     *THE_HEAP.pages[idx].as_ptr<i16>() = THE_HEAP.page_list;
            //     THE_HEAP.page_list = idx;
            // }
            // else {
            *(pair<u32, i32>*)(start + i) = THE_HEAP.free_lists[size_class];
            THE_HEAP.free_lists[size_class] = { u16(idx), i16((word*)(start + i) - THE_HEAP.pages[idx].words) };
            stats.freed_bytes += stride * BYTES_PER_WORD / 2;
            // }
        }

        i += stride;
    }
}

gc_stats stats;

void gc() {
    stats.since_gc_ns = nanotime() - stats.last_gc_ns;
    stats.freed_bytes = 0;

    i64 timer = nanotime();
    gc_bitmap bitmap;
    iptr* stack_top = (iptr*)spbase;
    iptr* stack_bottom = (iptr*)getsp();
    // print("Marking stack...\n");
    // print("  Stack starts at "), write_hex(stdout, (iptr)stack_bottom), print('\n');
    // print("  Stack ends at "), write_hex(stdout, (iptr)stack_top), print('\n');
    scan_region(bitmap, stack_bottom, stack_top);
    // print("Done marking stack.\n");

    // print("Marking free lists...\n");
    for (i32 i = 0; i < N_SIZE_CLASSES; i ++) {
        pair<u32, i32> list = THE_HEAP.free_lists[i];
        iptr p = (iptr)(THE_HEAP.pages[list.first].words + list.second);
        while (list.second >= 0) {
            bitmap.mark(p);
            list = *(pair<u32, i32>*)p;
            p = (iptr)(THE_HEAP.pages[list.first].words + list.second);
        }
    }
    // print("Done marking free lists.\n");

    // print("Marking heap...\n");
    for (u32 i = 0; i < THE_HEAP.page_limit; i ++) {
        i32 size_class = THE_HEAP.info[i].size_class;
        // print("  Examining page ", i, '\n');
        THE_HEAP.info[i].marked = false;
        if (THE_HEAP.info[i].huge) {
            scan_huge_page(bitmap, THE_HEAP.pages + i, THE_HEAP.info[i].n_huge_pages, stats);
            i += THE_HEAP.info[i].n_huge_pages - 1;
        }
        else if (size_class >= 0)
            scan_page(bitmap, (iptr*)(THE_HEAP.pages + i), (iptr*)(THE_HEAP.pages + i + 1), size_class, stats);
    }
    // print("Done marking heap.\n");

    for (u32 i = 0; i < THE_HEAP.page_limit; i ++) {
        i32 size_class = THE_HEAP.info[i].size_class;
        
        if (THE_HEAP.info[i].huge && !THE_HEAP.info[i].marked) {
            println("  Freed huge page ", i);
            THE_HEAP.clear_pages(i, THE_HEAP.info[i].n_huge_pages);
            stats.freed_bytes += THE_HEAP.info[i].n_huge_pages * BYTES_PER_PAGE;
            i += THE_HEAP.info[i].n_huge_pages - 1;
            THE_HEAP.info[i].n_huge_pages = 0;
            THE_HEAP.info[i].huge = false;
        }
        else if (THE_HEAP.info[i].huge) {
            i += THE_HEAP.info[i].n_huge_pages - 1;
        }
        else if (!THE_HEAP.info[i].marked) {
            THE_HEAP.clear_page_bitmap(i);
            THE_HEAP.info[i].size_class = -1;
            stats.freed_bytes += BYTES_PER_PAGE;
        }
        else if (size_class >= 0)
            free_page(bitmap, (iptr*)(THE_HEAP.pages + i), (iptr*)(THE_HEAP.pages + i + 1), size_class, i, stats);
    }

    bitmap.destroy();

    stats.n_gcs ++;
    stats.last_gc_ns = nanotime();
    stats.gc_time_ns = stats.last_gc_ns - timer;
    // print("GC cycle took ", double(stats.gc_time_ns) / 1000000.0, "ms\n");

    // for (i32 i = 0; i < N_SIZE_CLASSES; i ++) {
    //     pair<u16, i16> list = THE_HEAP.free_lists[i];
    //     iptr p = (iptr)(THE_HEAP.pages[list.first].words + list.second);
    //     iptr n = 0;
    //     while (list.second >= 0) {
    //         list = *(pair<u16, i16>*)p;
    //         p = (iptr)(THE_HEAP.pages[list.first].words + list.second);
    //         n ++;
    //     }
    //     println("  Free list for class ", SIZE_CLASSES[i] * BYTES_PER_WORD, " has length ", n);
    // }
}

void gc_deinit() {
    
}