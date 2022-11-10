#include "lib/gc.h"
#include "lib/io.h"
#include "core/sys.h"
#include "core/util.h"

static iptr spbase;
gc_heap THE_HEAP;

extern "C" iptr _initial_sp;

/*
 * We grab the initial stack pointer from the runtime during gc_init. Assumedly
 * this is good enough, since _initial_sp is set right after program entry, it'd be
 * difficult if not impossible to store a pointer before it.
 */

void gc_init() {
    spbase = _initial_sp;
    new(&THE_HEAP) gc_heap();
}

/*
 * Memory-mapped bitmap used to track live objects. We store one bit per pointer-size
 * word in memory, across the whole heap.
 */

struct gc_bitmap {
    i64* bits;
    i64* pagebits;
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

    /*
     * The core of our conservative marking. We consider any pointer-sized value to be
     * a heap reference if it is:
     *  - Non-null.
     *  - Pointer-aligned.
     *  - Within the bounds of THE_HEAP.
     */

    inline bool mark(iptr ptr) {
        if (!ptr) return false;
        if (ptr & 7) return false;
        iptr offset = (iptr*)ptr - base;
        if (offset < 0 || offset >= (iptr)THE_HEAP.page_limit * (iptr)WORDS_PER_PAGE * 2)
            return false;
        // print("    Marking "), write_hex(stdout, ptr), print(" at offset ", offset, '\n');
        i64 old = bits[offset / 64];
        bits[offset / 64] |= 1ul << offset % 64;
        return bits[offset / 64] > old;
    }

    inline bool get(iptr ptr) const {
        iptr offset = (iptr*)ptr - base;
        return bits[offset / 64] & 1ul << offset % 64;
    }

    /*
     * Checks a group of adjacent bits, and returns true if any are set. We use this
     * during scanning to scan an entire object-size value at once. The mask might overlap
     * between one bitmap word and another, so we check both if necessary.
     */

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

/*
 * The GC marking stack is a data structure that stores any number of references that
 * are waiting to be scanned. We use this to recursively traverse the object graph and
 * scan deeply-nested structures like trees or linked lists. Both a FIFO and LIFO version
 * of this structure is implemented, since they may have different characteristics for
 * different workloads. FIFO order (i.e. BFS) has pathological behavior when objects have
 * a very high branching factor, while LIFO order (i.e. DFS) has pathological behavior when
 * objects have very deep recursion with some branches.
 */

struct gc_mark_stack_fifo {
    iptr* refs;
    iptr start, end, capacity;

    gc_mark_stack_fifo(iptr capacity_in): start(0), end(0), capacity(capacity_in) {
        refs = (iptr*)mreq(capacity * sizeof(iptr) / PAGESIZE).ptr;
    }

    ~gc_mark_stack_fifo() {
        mfree({ (page*)refs, iptr(capacity * sizeof(iptr) / PAGESIZE) });
    }

    gc_mark_stack_fifo(const gc_mark_stack_fifo&) = delete;
    gc_mark_stack_fifo& operator=(const gc_mark_stack_fifo&) = delete;

    void grow() {
        iptr* old = refs;
        refs = (iptr*)mreq(capacity * 2 * sizeof(iptr) / PAGESIZE).ptr;
        if (end > start)
            mcpy(refs, old + start, (end - start) * sizeof(iptr));
        else {
            mcpy(refs, old + start, (capacity - start) * sizeof(iptr));
            mcpy(refs + capacity - start, old, end * sizeof(iptr));
        }
        mfree({ (page*)old, iptr(capacity * sizeof(iptr) / PAGESIZE) });
        start = 0, end = capacity - 1;
        capacity *= 2;
    }

    void push(iptr p) {
        if (((end + 1) & (capacity - 1)) == start) grow();
        refs[end] = p;
        end = (end + 1) & (capacity - 1);
    }

    iptr pop() {
        iptr p = refs[start];
        start = (start + 1) & (capacity - 1);
        return p;
    }

    operator bool() const {
        return start != end;
    }

    void clear() {
        end = start;
    }

    iptr size() const {
        return (end - start) & (capacity - 1);
    }
};

struct gc_mark_stack_lifo {
    iptr* refs;
    iptr n_refs, capacity;

    gc_mark_stack_lifo(iptr capacity_in): n_refs(0), capacity(capacity_in) {
        refs = (iptr*)mreq(capacity * sizeof(iptr) / PAGESIZE).ptr;
    }

    ~gc_mark_stack_lifo() {
        mfree({ (page*)refs, iptr(capacity * sizeof(iptr) / PAGESIZE) });
    }

    gc_mark_stack_lifo(const gc_mark_stack_lifo&) = delete;
    gc_mark_stack_lifo& operator=(const gc_mark_stack_lifo&) = delete;

    void grow() {
        iptr* old = refs;
        refs = (iptr*)mreq(capacity * 2 * sizeof(iptr) / PAGESIZE).ptr;
        mcpy(refs, old, n_refs * sizeof(iptr));
        mfree({ (page*)old, iptr(capacity * sizeof(iptr) / PAGESIZE) });
        capacity *= 2;
    }

    void push(iptr p) {
        if (n_refs == capacity) grow();
        refs[n_refs ++] = p;
    }

    iptr pop() {
        return refs[-- n_refs];
    }

    operator bool() const {
        return n_refs;
    }

    void clear() {
        n_refs = 0;
    }

    iptr size() const {
        return n_refs;
    }
};

using gc_mark_stack = gc_mark_stack_lifo;

/*
 * These functions are used to scan a region of pointer-size words, marking
 * possible heap references in the bitmap. scan_heap_object() is the same as scan_region(),
 * but also adds newly-found references to the marking stack.
 */

inline void scan_region(gc_bitmap bitmap, iptr* start, iptr* end) {
    while (start != end)
        bitmap.mark(*start ++);
}

inline void scan_heap_object(gc_bitmap bitmap, gc_mark_stack& mark_stack, iptr* start, iptr* end) {
    while (start != end) {
        if (bitmap.mark(*start)) 
            mark_stack.push(*start);
        start ++;
    }
}

/*
 * We use two different kinds of page traversal when scanning the heap. 
 *
 * We first scan without using the marking stack (although it is still passed to provide a
 * consistent static interface) to minimize use of the stack and avoid memory overhead.
 * In this mode, for each object within a page, if any of its contents are marked in the bitmap,
 * we scan the object as a root region - conservatively scanning all of its possible pointer
 * members.
 * 
 * Notably, this is insufficient to reach all live objects - scanning in this mode is pretty efficient,
 * but we only scan one recursion level at a time. It's possible we scan a live object that would have
 * marked a previous object in the same block as live. To handle cases where many objects are chained
 * within the same block, taking advantage of possible locality, we do two sweeps: a forward sweep, where
 * children of lower-addressed objects will be caught, and a backward sweep to do the reverse. This way,
 * even if a block contains a maximally-recursive structure like a linked list, it's possible the entire
 * list may be scanned at once if it happens to have been allocated left-to-right or right-to-left.
 */

void scan_page_no_stack(gc_bitmap bitmap, gc_mark_stack&, iptr* start, iptr* end, i32 size_class, gc_stats& stats) {
    iptr i = 0;
    iptr mask = SIZE_CLASS_MASKS[size_class];
    iptr stride = SIZE_CLASSES[size_class] * 2;
    iptr* last = start;
    while (start + i + stride < end) {
        if (bitmap.get((iptr)(start + i), mask)) {
            scan_region(bitmap, start + i, start + i + stride);
        }
        last = start + i;
        i += stride;
    }
    i = 0;
    while (last - i >= start) {
        if (bitmap.get((iptr)(last - i), mask)) {
            scan_region(bitmap, last - i, last - i + stride);
        }
        i += stride;
    }
}

/*
 * Since scanning without the marking stack is insufficient to handle data structures with unpredictable
 * nesting or indirection between heap pages, we use the scan_page() function to traverse all live objects 
 * in a page, but instead of merely marking them in the bitmap, we also add them to the marking stack,
 * where we can explore them as roots of the object graph in a later loop.
 */

void scan_page(gc_bitmap bitmap, gc_mark_stack& mark_stack, iptr* start, iptr* end, i32 size_class, gc_stats& stats) {
    iptr i = 0;
    iptr mask = SIZE_CLASS_MASKS[size_class];
    iptr stride = SIZE_CLASSES[size_class] * 2;
    while (start + i < end) {
        if (bitmap.get((iptr)(start + i), mask))
            scan_heap_object(bitmap, mark_stack, start + i, start + i + stride);
        i += stride;
    }
}

/*
 * Unlike finer-grained pages, to scan a huge page, we just scan the entire thing as a region. This
 * assumes we already found the huge page was marked live, and that we are starting from the first
 * page in the huge allocation. Both of these conditions must be guaranteed by callers during the GC.
 */

void scan_huge_page(gc_bitmap bitmap, gc_page* page_start, i32 n_pages, gc_stats& stats) {
    scan_region(bitmap, (iptr*)page_start, (iptr*)(page_start + n_pages));
}

/*
 * Once marking is complete, we use this function to traverse a page and free unmarked objects,
 * adding them to the corresponding free-list.
 */

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

/*
 * LOG_GC and these printing functions are for use debugging the GC. Maybe in the future
 * these can be exposed via an environment variable or other command-line interface?
 */

#define LOG_GC 1

void print_info(gc_heap& heap) {
    print(" * Heap blocks: [");
    for (i64 i = 0; i < heap.page_limit; i ++) {
        if (i) print(' ');
        print("-abcdefghijABCDEFGHIJ"[heap.info[i].size_class + 1]);
        if (heap.info[i].huge) print("H");
        else print("-");
        if (heap.info[i].visited) print("V");
        else if (heap.info[i].marked) print("X");
        else print("-");
    }
    println("]");
}

void print_page_bitmap(gc_heap& heap) {
    i64 bitmap_size = THE_HEAP.page_limit * WORDS_PER_PAGE * 2 / 64;
    print(" * Heap page bitmap: ");
    for (i64 i = 0; i < (heap.page_limit + 63) / 64; i ++) {
        i64 h = heap.live_page_bitmap[i];
        i64 shift = 60;
        while (shift) {
            print("0123456789abcdef"[(h >> shift) & 0xf]);
            shift -= 4;
        }
    }
    println();
}

void print_bitmap(gc_bitmap& bitmap) {
    i64 bitmap_size = THE_HEAP.page_limit * WORDS_PER_PAGE * 2 / 64;
    print("BITMAP: ");
    for (i64 i = 0; i < bitmap_size; i ++) {
        i64 h = bitmap.bits[i];
        i64 shift = 60;
        while (shift) {
            print("0123456789abcdef"[(h >> shift) & 0xf]);
            shift -= 4;
        }
    }
    println();
}

/*
 * When popping pointers from the marking stack, it's possible we may visit a pointer
 * that doesn't point to the beginning of an object. We use this function to round a pointer
 * down to the nearest multiple of its size class. Notably, for non-power-of-two size classes,
 * this involves a potentially costly division by three.
 */

iptr round_to_size_class(iptr p, i8 size_class) {
    if (size_class == 0) return p & ~u64(0xf); // 1-word alignment
    else if (size_class & 1) return p & ~(SIZE_CLASSES[size_class] * BYTES_PER_WORD - u64(1)); // Power of two
    else {
        uptr base = p & ~(BYTES_PER_PAGE - 1);
        uptr diff = p - base;
        diff = uptr(diff) >> (4 + max<i64>(0, size_class / 2 - 1)); // Divide by largest power-of-two factor.
        diff /= u64(3); // Divide by constant 3.
        diff *= SIZE_CLASSES[size_class] * BYTES_PER_WORD;
        return base + diff;
    }
}

/*
 * Scans the bitmap and marks any unmarked pages if they contain at least one set bit.
 */

void mark_pages(gc_bitmap bitmap) {
    for (u32 i = 0; i < THE_HEAP.page_limit; i ++) {
        if (THE_HEAP.info[i].visited) continue;
        constexpr i64 BITS_PER_PAGE = BYTES_PER_PAGE / sizeof(iptr) / 64;
        for (i64 j = 0; j < BITS_PER_PAGE; j ++) {
            if (bitmap.bits[i * BITS_PER_PAGE + j]) {
                THE_HEAP.info[i].marked = true;
                break;
            }
        }
    }
}

/*
 * Scans all marked pages, visiting non-huge pages with the templated function.
 * After a page is scanned, it's marked as visited so we don't re-scan it on any 
 * later iterations.
 */

template<void(*scan_page_func)(gc_bitmap, gc_mark_stack&, iptr*, iptr*, i32, gc_stats&)>
void scan_pages(gc_bitmap bitmap, gc_mark_stack& mark_stack) {
    for (u32 i = 0; i < THE_HEAP.page_limit; i ++) {
        auto& info = THE_HEAP.info[i];
        i32 size_class = info.size_class;
        if (info.huge && info.marked && !info.visited) {
            if (info.n_huge_pages < 0)
                i += info.n_huge_pages;
            auto& huge_info = THE_HEAP.info[i];
            for (u32 j = i; j < i + huge_info.n_huge_pages; j ++) 
                THE_HEAP.info[j].marked = THE_HEAP.info[i].marked;
            scan_huge_page(bitmap, THE_HEAP.pages + i, huge_info.n_huge_pages, stats);
            i += THE_HEAP.info[i].n_huge_pages - 1;
            for (u32 j = i; j < i + huge_info.n_huge_pages; j ++) 
                THE_HEAP.info[j].visited = true;
        }
        else if (info.huge && !info.marked)
            i += THE_HEAP.info[i].n_huge_pages - 1;
        else if (info.marked && !info.visited && size_class >= 0) {
            scan_page_func(bitmap, mark_stack, (iptr*)(THE_HEAP.pages + i), (iptr*)(THE_HEAP.pages + i + 1), size_class, stats);
            info.visited = true;
        }
    }
}

/*
 * Performs a full mark-and-sweep GC of the heap.
 */

void gc() {
    stats.since_gc_ns = nanotime() - stats.last_gc_ns;
    stats.freed_bytes = 0;

    i64 timer = nanotime();
    gc_bitmap bitmap;

    /*
     * Mark objects we find referenced on the stack.
     */
    // Reset page state.
    for (u32 i = 0; i < THE_HEAP.page_limit; i ++)
        THE_HEAP.info[i].marked = THE_HEAP.info[i].visited = false;
    // Scan stack.
    iptr* stack_top = (iptr*)spbase;
    iptr* stack_bottom = (iptr*)getsp();
    if (LOG_GC) println("Doing GC");
    scan_region(bitmap, stack_bottom, stack_top);
    if (LOG_GC) println(" * Marking stack took ", (nanotime() - timer) / 1000000.0, " ms");

    /*
     * Mark freelists to ensure we don't break them.
     */
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
    if (LOG_GC) println(" * Marking free lists took ", (nanotime() - premarklist) / 1000000.0, " ms");

    /* 
     * Mark additional, non-stack, non-freelist roots.
     */ 
    i64 premarkothers = nanotime();
    for (i64 i = 0; i < THE_HEAP.n_extra_roots; i ++) {
        gc_root root = THE_HEAP.extra_roots[i];
        scan_region(bitmap, (iptr*)root.start, (iptr*)root.end);
    }
    if (LOG_GC) println(" * Marking extra roots took ", (nanotime() - premarkothers) / 1000000.0, " ms");

    /*
     * Transitively mark objects visible through other heap objects.
     */
    i64 premarkheap = nanotime();
    gc_mark_stack mark_stack(16777216 / sizeof(iptr));
    // Spend a few iterations detecting objects with low nesting.
    for (u32 k = 0; k < 2; k ++) {
        mark_pages(bitmap);
        scan_pages<scan_page_no_stack>(bitmap, mark_stack);
    }
    // Initialize mark stack with already-marked objects.
    mark_pages(bitmap);
    scan_pages<scan_page>(bitmap, mark_stack);
    if (LOG_GC) println(" * Initial heap marking took ", (nanotime() - premarkheap) / 1000000.0, " ms");
    // Consume mark stack until no more objects can be reached.
    i64 predrainstack = nanotime();
    while (mark_stack) {
        iptr p = mark_stack.pop();
        auto info = THE_HEAP.info[(p - (iptr)THE_HEAP.pages) / BYTES_PER_PAGE];
        if (info.huge) {
            if (info.n_huge_pages < 0) {
                p += BYTES_PER_PAGE * info.n_huge_pages;
                info = THE_HEAP.info[(gc_page*)p - THE_HEAP.pages];
            }
            scan_huge_page(bitmap, (gc_page*)(p & (BYTES_PER_PAGE - 1)), info.n_huge_pages, stats);
            continue;
        }
        p = round_to_size_class(p, info.size_class);
        scan_heap_object(bitmap, mark_stack, (iptr*)p, (iptr*)p + SIZE_CLASSES[info.size_class] * PTRS_PER_WORD);
    }
    if (LOG_GC) println(" * Draining mark stack took ", (nanotime() - predrainstack) / 1000000.0, " ms");

    /*
     * Free all unmarked objects.
     */
    i64 prefree = nanotime();
    // Update which pages are marked.
    mark_pages(bitmap);
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
    if (LOG_GC) println(" * Freeing took ", (nanotime() - prefree) / 1000000.0, " ms");

    bitmap.destroy();

    stats.n_gcs ++;
    stats.last_gc_ns = nanotime();
    stats.gc_time_ns = stats.last_gc_ns - timer;
    if (LOG_GC) println(" * GC cycle took ", double(stats.gc_time_ns) / 1000000.0, "ms and freed ", stats.freed_bytes / 1000000.0, "mb out of ", THE_HEAP.page_limit * BYTES_PER_PAGE / 1000000.0, "mb heap (", double(100 * stats.freed_bytes) / (THE_HEAP.page_limit * BYTES_PER_PAGE), "%)");
}

void gc_deinit() {
    
}