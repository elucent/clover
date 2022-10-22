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
    i64* pagebits;
    iptr* base;

    inline gc_bitmap(): 
        base((iptr*)THE_HEAP.pages) {
        i64 bitmap_size = THE_HEAP.page_limit * WORDS_PER_PAGE * 2 / 8;
        bits = (i64*)mreq(bitmap_size / PAGESIZE).ptr;
        pagebits = (i64*)mreq(THE_HEAP.page_limit / 8).ptr;
        mset(bits, bitmap_size, 0);
    }

    inline void destroy() {
        i64 bitmap_size = THE_HEAP.page_limit * WORDS_PER_PAGE * 2 / 8;
        mfree({ (page*)bits, bitmap_size / PAGESIZE });
        mfree({ (page*)pagebits, THE_HEAP.page_limit / 8 });
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

    bool get(iptr ptr, i64 mask) const {
        uptr offset = (iptr*)ptr - base;
        uptr lbits = bits[offset / 64], hbits = bits[offset / 64 + 1];
        if (!lbits) return false;
        
        uptr low = mask << offset % 64, high = mask >> (64 - offset % 64);
        uptr isThere = 0;
        if (low) isThere |= lbits & low;
        if (offset % 64 && high) isThere |= hbits & high;
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
    while (start + i < end) {
        if (bitmap.get((iptr)(start + i), mask)) {
            scan_region(bitmap, start + i, start + i + stride);
        }
        i += stride;
    }
}

void scan_huge_page(gc_bitmap bitmap, gc_page* page_start, i32 n_pages, gc_stats& stats) {
    i32 idx = i32(page_start - THE_HEAP.pages);
    i32 stride = BYTES_PER_PAGE / (64 * sizeof(iptr));
    bool live = false;
    for (i32 i = idx * stride; i < (idx + n_pages) * stride; i ++) {
        if (bitmap.bits[i]) {
            live = true;
            break;
        }
    }
    if (live) {
        scan_region(bitmap, (iptr*)page_start, (iptr*)(page_start + n_pages));
    }
}

void free_page(gc_bitmap bitmap, iptr* start, iptr* end, i32 size_class, u32 idx, gc_stats& stats) {
    iptr mask = SIZE_CLASS_MASKS[size_class];
    iptr stride = SIZE_CLASSES[size_class] * 2;

    // Grab metadata from THE_HEAP to local frame.
    gc_page_info& info = THE_HEAP.info[idx];
    word* page_words = THE_HEAP.pages[idx].words;

    iptr bytes_freed = 0;
    
    for (i32 i = 0; start + i < end; i += stride) {
        if (!bitmap.get((iptr)(start + i), mask)) {
            *(pair<u32, i32>*)(start + i) = THE_HEAP.free_lists[size_class];
            THE_HEAP.free_lists[size_class] = { u32(idx), i32((word*)(start + i) - page_words) };
            bytes_freed += stride * BYTES_PER_WORD / 2;
        }
    }

    // Write back free-lists and stats.
    stats.freed_bytes += bytes_freed;
}

gc_stats stats;

void gc() {
    stats.since_gc_ns = nanotime() - stats.last_gc_ns;
    stats.freed_bytes = 0;

    i64 timer = nanotime();
    gc_bitmap bitmap;
    i64 marked_pages_a_buf[MAX_PAGES_PER_REGION / 64], marked_pages_b_buf[MAX_PAGES_PER_REGION / 64];
    mset(marked_pages_a_buf, 0, sizeof(marked_pages_a_buf));
    mset(marked_pages_b_buf, 0, sizeof(marked_pages_b_buf));
    i64* marked_pages_a = marked_pages_a_buf, *marked_pages_b = marked_pages_b_buf;

    iptr* stack_top = (iptr*)spbase;
    iptr* stack_bottom = (iptr*)getsp();
    println("Doing GC");
    // print("Marking stack...\n");
    // print("  Stack starts at "), write_hex(stdout, (iptr)stack_bottom), print('\n');
    // print("  Stack ends at "), write_hex(stdout, (iptr)stack_top), print('\n');
    scan_region(bitmap, stack_bottom, stack_top);
    println(" * Marking stack took ", (nanotime() - timer) / 1000000.0, " ms");
    // print("Done marking stack.\n");

    // print("Marking free lists...\n");
    i64 premarklist = nanotime();
    for (i32 i = 0; i < N_SIZE_CLASSES; i ++) {
        pair<u32, i32> list = THE_HEAP.free_lists[i];
        iptr p = (iptr)(THE_HEAP.pages[list.first].words + list.second);
        while (list.second >= 0) {
            bitmap.mark(p);
            list = *(pair<u32, i32>*)p;
            p = (iptr)(THE_HEAP.pages[list.first].words + list.second);
        }
    }
    println(" * Marking free lists took ", (nanotime() - premarklist) / 1000000.0, " ms");
    // print("Done marking free lists.\n");

    // print("Marking heap...\n");
    i64 premarkheap = nanotime();

    while (true) {
        bool still_marking_heap = false;
        mset(marked_pages_b, 0, sizeof(marked_pages_b_buf));
        for (u32 i = 0; i < THE_HEAP.page_limit; i ++) {
            iptr start = i * BYTES_PER_PAGE, end = (i + 1) * BYTES_PER_PAGE;
            iptr stride = sizeof(iptr) * 64; // Bytes marked by each 64-bit bitmap word.
            bool live = false;
            for (u32 j = start / stride; j < end / stride; j ++)
                if (bitmap.bits[j]) live = true;
            if (live) marked_pages_b[i / 64] |= 1ul << (i % 64);
        }
        for (u32 i = 0; i < THE_HEAP.page_limit / 64; i ++) // Exclude all references marked so far.
            marked_pages_b[i] &= ~marked_pages_a[i];
        i32 n_marked = 0;
        for (u32 i = 0; i < THE_HEAP.page_limit / 64; i ++) // Check if we still have work to do.
            if (marked_pages_b[i]) still_marking_heap = true, n_marked ++;
        if (!still_marking_heap) break;

        for (u32 i = 0; i < THE_HEAP.page_limit; i ++) {
            i32 size_class = THE_HEAP.info[i].size_class;
            if (THE_HEAP.info[i].huge) {
                scan_huge_page(bitmap, THE_HEAP.pages + i, THE_HEAP.info[i].n_huge_pages, stats);
                i += THE_HEAP.info[i].n_huge_pages - 1;
            }
            else if (size_class >= 0 && (marked_pages_b[i / 64] & 1ul << (i % 64)))
                scan_page(bitmap, (iptr*)(THE_HEAP.pages + i), (iptr*)(THE_HEAP.pages + i + 1), size_class, stats);
        }
        
        for (u32 i = 0; i < THE_HEAP.page_limit / 64; i ++) // Add scanned pages to excluded set.
            marked_pages_a[i] |= marked_pages_b[i];
    }
    for (u32 i = 0; i < THE_HEAP.page_limit; i ++) // Add scanned pages to excluded set.
        THE_HEAP.info[i].marked = marked_pages_a[i / 64] & 1ul << (i % 64);
    println(" * Marking heap took ", (nanotime() - premarkheap) / 1000000.0, " ms");
    // print("Done marking heap.\n");

    i64 prefree = nanotime();
    for (u32 i = 0; i < THE_HEAP.page_limit; i ++) {
        i32 size_class = THE_HEAP.info[i].size_class;
        
        if (THE_HEAP.info[i].huge && !THE_HEAP.info[i].marked) {
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
    println(" * Freeing took ", (nanotime() - prefree) / 1000000.0, " ms");

    bitmap.destroy();

    stats.n_gcs ++;
    stats.last_gc_ns = nanotime();
    stats.gc_time_ns = stats.last_gc_ns - timer;
    println(" * GC cycle took ", double(stats.gc_time_ns) / 1000000.0, "ms and freed ", stats.freed_bytes / 1000000.0, "mb");

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
    // println("freed ", stats.freed_bytes);
}

void gc_deinit() {
    
}