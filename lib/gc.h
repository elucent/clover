#ifndef BASIL_LIB_GC_H
#define BASIL_LIB_GC_H

#include "core/def.h"
#include "core/sys.h"
#include "lib/tuple.h"
#include "lib/math.h"

/*
 * GC Sizes
 * 
 * Memory in the GC is organized into the following units:
 *  - Bytes
 *  - Words: Granularity of allocation sizes in the heap. 16 bytes 
 *  - Pages: Not system pages, but blocks of shared size class. Currently 65kiB
 *  - Default Heaps: The number of pages in a heap by default, and the growth factor when expanding the heap. Currently 1MiB
 *  - Max Heaps: Maximum heap size for entire heap instance. Mapped in virtual memory from heap construction but not committed. Currently 4GiB
 */

constexpr uptr BYTES_PER_WORD = 16;
constexpr uptr PTRS_PER_WORD = BYTES_PER_WORD / sizeof(iptr);
constexpr uptr WORDS_PER_PAGE = 4096;
constexpr uptr BYTES_PER_PAGE = BYTES_PER_WORD * WORDS_PER_PAGE;
constexpr uptr DEFAULT_PAGES_PER_REGION = 65536;
static_assert(DEFAULT_PAGES_PER_REGION >= 16, "Minimum heap size is 1MB."); // Otherwise the live page bitmap won't work. Kinda not ideal.
constexpr uptr MAX_PAGES_PER_REGION = 65536;
constexpr uptr BYTES_PER_REGION = DEFAULT_PAGES_PER_REGION * BYTES_PER_PAGE;
constexpr uptr MAX_BYTES_PER_REGION = MAX_PAGES_PER_REGION * BYTES_PER_PAGE;

/*
 * Size Classes
 * 
 * In order to efficiently support both GC and a malloc/free-style interface, we use
 * a free-list allocator with fixed size classes per block. Size classes are specified
 * in heap words, so all allocations are rounded up to at least 16 bytes. Size classes
 * are also either powers of two, or halfway between powers of two, so that determining
 * the size class for a given allocation size in bytes is relatively efficient.
 * Currently, the maximum size class is half a page.
 */

constexpr u32 SIZE_CLASSES[] = {
    1, 2, 3, 4, 6, 8, 12, 16,
    24, 32, 48, 64, 96, 128, 192, 256,
    384, 512, 768, 1024, 1536, 2048
};

/*
 * These masks are used to efficiently scan the heap bitmap in units of each allocation size.
 */

constexpr u64 SIZE_CLASS_MASKS[] = {
    0x3, 0xf, 0x3f, 0xff, 0x3ff, 0xfff, 0x3fff, 0xffff, 
    0x3ffff, 0xfffff, 0x3fffff, 0xffffff, 0x3ffffff, 0xfffffff, 0x3fffffff, 0xffffffff,
    0x3ffffffff, 0xfffffffff, 0x3fffffffff, 0xffffffffff, 0x3ffffffffff, 0xfffffffffff
};

constexpr u16 N_SIZE_CLASSES = 22;

/*
 * If we know the size of an allocation at compile-time, for instance if we know the type to
 * be allocated, we can use this function to determine its size class ahead-of-time.
 */

constexpr i16 size_class_for_const(u32 bytes) {
    u32 words = (bytes + BYTES_PER_WORD - 1) / BYTES_PER_WORD;
    switch (words) {
        case 0 ... 1: return 0;
        case 2: return 1;
        case 3: return 2;
        case 4: return 3;
        case 5 ... 6: return 4;
        case 7 ... 8: return 5;
        case 9 ... 12: return 6;
        case 13 ... 16: return 7;
        case 17 ... 24: return 8;
        case 25 ... 32: return 9;
        case 33 ... 48: return 10;
        case 49 ... 64: return 11;
        case 65 ... 96: return 12;
        case 97 ... 128: return 13;
        case 129 ... 192: return 14;
        case 193 ... 256: return 15;
        case 257 ... 384: return 16;
        case 385 ... 512: return 17;
        case 513 ... 768: return 18;
        case 769 ... 1024: return 19;
        case 1025 ... 1536: return 20;
        case 1537 ... 2048: return 21;
        default: return -1;
    }
}

/*
 * Otherwise, we defer to this function, which determines the size class of a given allocation
 * size in bytes. We first determine the fast log2 of the requested size by counting the leading
 * zeroes (efficient on modern architectures) and check if the remainder is more or less than the
 * next power of two down. We use this check to distinguish between either a power-of-two size
 * class or any of our halfway size classes. The hope is that this strikes a good balance between
 * efficient size class resolution and relatively small (max 25%) memory overhead.
 */

inline i16 size_class_for(u32 bytes) {
    u32 words = (bytes + BYTES_PER_WORD - 1) / BYTES_PER_WORD;
    if (words > SIZE_CLASSES[N_SIZE_CLASSES - 1]) return -1;
    i16 log2 = 32 - __builtin_clz(words - 1);
    if (words - 1 == 0) log2 = 1;
    u32 basewords = 1 << log2;
    u32 shift = basewords < 4 ? basewords >> 1 : basewords >> 2;
    i16 base = log2 * 2 - 1;
    if (words <= basewords - shift)
        base --;
    return base;
}

struct word {
    i8 bytes[BYTES_PER_WORD];

    template<typename T>
    inline T* as_ptr() {
        return (T*)bytes;
    }
};

struct gc_page {
    word words[WORDS_PER_PAGE];

    template<typename T>
    inline T* as_ptr() {
        return words[0].as_ptr<T>();
    }
};

/*
 * Metadata stored per GC page. For each 65kiB page, we store one eight-byte info structure, less than 0.1% overhead.
 * The fields of the info structure outline all the different states a GC page can be in:
 *  - size_class specifies the size class of the page's elements.
 *  - marked indicates if the page contains any references marked during a GC.
 *  - visited indicates the page was marked and scanned during a GC.
 *  - huge marks this page as part of a huge allocation (greater than a page-size).
 *  - n_huge_pages stores one of two things:
 *     - The number of pages in this huge allocation, if positive. Only set for the first page in the allocation.
 *     - The page offset of the first huge page from this one, i.e. -1 for the second page. Set for all pages but the first.
 *    Using n_huge_pages, we can find the length and start of any huge allocation from anywhere in that allocation.
 */

struct gc_page_info {
    i32 n_huge_pages;
    i8 size_class;
    bool huge, marked, visited;

    inline gc_page_info():
        n_huge_pages(0), huge(false), marked(false), visited(false) {}
};

/*
 * Various metadata about past GCs. freed_bytes is used to assess whether we should
 * try to expand the heap after a GC.
 */

struct gc_stats {
    iptr freed_bytes = 0, n_gcs = 0;
    i64 last_gc_ns, since_gc_ns, gc_time_ns;
};

/*
 * Represents a user-defined range of memory addresses that should be considered roots
 * for the GC. Useful for static memory or memory allocated by another allocator that
 * still needs to be scanned. Currently, all non-stack roots must be manually declared
 * to the GC, but maybe this is an area of improvement?
 */

struct gc_root {
    void* start, *end;
};

extern gc_stats stats;

/*
 * Uses the libcore mreq and mfree functions to map a virtual memory region aligned
 * to a certain alignment.
 */

inline slice<page> map_aligned(uptr bytes, uptr align) {
    iptr padded = bytes + align + PAGESIZE - 1;
    iptr rounded = bytes + PAGESIZE - 1;
    auto mapped = mreq(padded / PAGESIZE);
    uptr addr = (uptr)mapped.ptr, aligned_addr = ((addr + align - 1ul) & ~(align - 1ul));
    if (addr != aligned_addr) {
        iptr diff = padded / PAGESIZE - rounded / PAGESIZE;
        iptr prepages = (aligned_addr - addr) / PAGESIZE;
        
        mfree({ mapped.ptr, prepages });
        mfree({ (page*)aligned_addr + rounded / PAGESIZE, diff - prepages });
        mapped = { (page*)aligned_addr, rounded / PAGESIZE };
    }
    return mapped;
}

void gc();

/*
 * Overall GC heap structure. Currently, there is only one of these per program,
 * called THE_HEAP.
 *
 * Each GC heap contains the following:
 *  - pages points to a contiguous block of mapped pages in virtual memory, aligned to the default heap size, large enough to store the max heap size.
 *  - info points to a mapped region of gc_page_info structs, large enough to store MAX_PAGES_PER_REGION pages.
 *  - free_lists[] stores a free-list pair for each size class. Each free-list element is a pair of {page index, word offset in page}.
 *  - top_page specifies the highest address not currently bound to a page. Used to quickly bump-allocate new pages on startup or after expansion.
 *  - page_limit is the number of pages in this heap.
 *  - live_page_bitmap stores one bit per page, up to MAX_PAGES_PER_REGION pages. A bit is set if its corresponding page is occupied, unset otherwise.
 *  - lock can be clobbered for use in a spinlock. Currently unused, as we evaluate the prospect of making this GC thread-safe.
 *  - extra_roots points to a mapped region storing n_extra_roots additional root regions. root_capacity tracks the number of roots that can be stored before reallocation. 
 */

struct gc_heap {
    gc_page* pages;
    gc_page_info* info;
    pair<u32, i32> free_lists[N_SIZE_CLASSES];
    u32 top_page, page_limit;
    i64 live_page_bitmap[MAX_PAGES_PER_REGION / 64];
    i64 lock = 0;
    gc_root* extra_roots;
    i32 n_extra_roots, root_capacity;

    inline gc_heap(): top_page(0), page_limit(DEFAULT_PAGES_PER_REGION) {
        for (u32 i = 0; i < N_SIZE_CLASSES; i ++)
            free_lists[i] = { 0, -1 };
        constexpr uptr initial_size = DEFAULT_PAGES_PER_REGION * WORDS_PER_PAGE * BYTES_PER_WORD;
        auto aligned_pages = map_aligned(MAX_BYTES_PER_REGION, initial_size);
        auto aligned_info = map_aligned(MAX_PAGES_PER_REGION * sizeof(gc_page_info), PAGESIZE);
        pages = (gc_page*)aligned_pages.ptr;
        info = (gc_page_info*)aligned_info.ptr;
        for (u32 i = 0; i < page_limit; i ++) 
            info[i].size_class = -1;
        mset(live_page_bitmap, 0, sizeof(live_page_bitmap));

        extra_roots = (gc_root*)mreq(1).ptr;
        n_extra_roots = 0;
        root_capacity = PAGESIZE / sizeof(gc_root);
    }

    /*
     * Adds a user-defined root region to this heap's root list.
     */

    inline void add_root(void* start, void* end) {
        if (n_extra_roots >= root_capacity) {
            int n_pages = root_capacity * sizeof(gc_root) / PAGESIZE;
            gc_root* new_pages = (gc_root*)mreq((root_capacity *= 2) / PAGESIZE).ptr;
            mcpy(new_pages, extra_roots, n_pages * PAGESIZE);
            mfree({(page*)extra_roots, n_pages});
            extra_roots = new_pages;
        }
        extra_roots[n_extra_roots ++] = {start, end};
    }

    /*
     * These functions manipulate the live page bitmap to allocate new pages. When pages are
     * allocated to certain size classes, they must be marked in the bitmap; when they are
     * freed, their corresponding bits must be cleared.
     * 
     * The main use case for this bitmap specifically is finding contiguous regions of free
     * pages, to the end of allocating huge objects. Unlike a traditional free list, we can
     * utilize fast bitmap scanning to find contiguous sections quickly, at the cost of linear
     * complexity and minor memory overhead (though at 1 bit per page this should be negligible).
     * 
     * The main weakness of this approach is fragmentation - we can't move pages once they contain
     * any allocation, so in a long-running program we might exhaust all possible spaces for large
     * objects. If this happens, and a GC fails to free enough space to allocate a large object,
     * the program will panic with an out-of-memory message.
     */

    inline void mark_page_bitmap(u32 idx) {
        live_page_bitmap[idx / 64] |= 1ul << (idx % 64);
    }

    inline void clear_page_bitmap(u32 idx) {
        live_page_bitmap[idx / 64] &= ~(1ul << (idx % 64));
    }

    inline i32 next_free_page() {
        for (i64 i = 0; i < (page_limit + 63) / 64; i ++) if ((u64)live_page_bitmap[i] != 0xfffffffffffffffful) {
            return i * 64 + __builtin_ctzll(~live_page_bitmap[i]);
        }
        return -1;
    }

    inline void mark_pages(u32 idx, u32 n_pages) {
        for (u32 i = idx; i < idx + n_pages; i ++) 
            live_page_bitmap[i / 64] |= 1ul << (i % 64);
    }

    inline void clear_pages(u32 idx, u32 n_pages) {
        for (u32 i = idx; i < idx + n_pages; i ++) 
            live_page_bitmap[i / 64] &= ~(1ul << (i % 64));
    }

    inline i32 find_free_pages(u32 n_pages) {
        if (n_pages >= page_limit) return -1;
        for (i64 i = 0; i < (page_limit + 63) / 64; i ++) if ((u64)live_page_bitmap[i] != 0xfffffffffffffffful) {
            u64 start = i * 64 + __builtin_ctzll(~live_page_bitmap[i]);
            bool big_enough = true;
            for (u64 j = 1; j < n_pages; j ++) {
                if (live_page_bitmap[(start + j) / 64] & 1ul << ((start + j) % 64)) {
                    big_enough = false;
                    break;
                }
            }
            if (big_enough)
                return start;
        }
        return -1;
    }

    /*
     * This heuristic is called after GC to assess whether the heap should try to expand.
     * Currently, we try to expand any time the occupancy of the heap is greater than 87.5%. 
     */

    inline bool should_grow_heap_heuristic() {
        return stats.freed_bytes < (iptr)page_limit * (iptr)BYTES_PER_PAGE / 8
            ; // || stats.since_gc_ns < 2 * stats.gc_time_ns; <-- Should we use a temporal heuristic too?
    }

    /*
     * Grows the heap by the requested page count up to the maximum page limit, and returns if
     * any new pages were successfully claimed.
     */

    inline bool try_grow(u32 pages) {
        if (page_limit == MAX_PAGES_PER_REGION)
            return false;
        page_limit += pages;
        if (page_limit > MAX_PAGES_PER_REGION)
            page_limit = MAX_PAGES_PER_REGION;
        return true;
    }

    /*
     * alloc_page_for is responsible for finding new pages from the page array to use for
     * sized allocations. First, we try to bump top_page to claim a page from the end. If
     * that's not possible, we scan the live page bitmap for the next free page. Otherwise,
     * this triggers a GC, after which we try again to find a free page. If that fails, we
     * try to grow the heap by up to the default heap size. If we fail to allocate any new
     * page, an out-of-memory crash occurs.
     */

    inline __attribute((noinline)) pair<gc_page*, gc_page_info> alloc_page_for(i32 size_class) {
        i32 result;
        if (top_page < page_limit) // If there are bump-allocatable pages in the arena.
            result = top_page ++;
        else if ((result = next_free_page()) >= 0) {
            // fall through to end
        }
        else {
            pushrframe(gc);
            i32 next_page = find_free_pages(1);
            if (should_grow_heap_heuristic() || next_page == -1)
                if (!try_grow(max<u32>(1, DEFAULT_PAGES_PER_REGION)) && next_page == -1)
                    fatal("Out of memory!");
            return { nullptr, {} };
        }

        info[result].size_class = size_class;
        mark_page_bitmap(result);
        return { pages + result, info[result] };
    }
    
    /*
     * alloc_huge is the entrypoint for huge allocations. This also makes it one of two
     * functions, along with alloc_page_for, that can be responsible for triggering a GC.
     * Like alloc_page_for, we first try to bump allocate the requested number of pages,
     * then try to find a contiguous page region in the live page bitmap. If that fails,
     * we try to GC, and if that fails, we try to grow the heap by up to the requested
     * page count. If we still can't find a valid page, we crash by out-of-memory.
     *
     * If the page allocation succeeds, we mark all pages as huge, store the size in pages
     * in the first page's info, and the offsets of all other pages relative to the first.
     */

    inline __attribute((noinline)) void* alloc_huge(i32 size) {
        i32 n_pages = (size + BYTES_PER_PAGE - 1) / BYTES_PER_PAGE;
        if (top_page + n_pages <= page_limit) {
            void* result = pages + top_page;
            mark_pages(top_page, n_pages);
            for (u32 i = top_page; i < top_page + n_pages; i ++) {
                info[i].huge = true;
                info[i].n_huge_pages = i == top_page ? n_pages : top_page - i;
            }
            top_page += n_pages;
            return result;
        }
        else {
            i32 next_page = find_free_pages(n_pages);
            if (next_page == -1) {
                pushrframe(gc);
                next_page = find_free_pages(n_pages);
                if (should_grow_heap_heuristic() || next_page == -1)
                    if (!try_grow(max<u32>(n_pages, DEFAULT_PAGES_PER_REGION)) && next_page == -1)
                        fatal("Out of memory!");
                return alloc_huge(size);
            }
            mark_pages(next_page, n_pages);
            for (i32 i = next_page; i < next_page + n_pages; i ++) {
                info[i].huge = true;
                info[i].n_huge_pages = i == next_page ? n_pages : next_page - i;
            }
            return pages + next_page;
        }
    }

    /*
     * Eventually, the entry point for thread-safe huge allocations.
     */
    
    inline void* alloc_huge_locked(i32 size) {
        void* result = alloc_huge(size);
        return result;
    }

    /*
     * Called to free all pages in the huge allocation.
     */

    inline void free_huge(i32 idx) {
        i32 n_pages = info[idx].n_huge_pages;
        clear_pages(idx, n_pages);
        for (i32 i = 0; i < n_pages; i ++) 
            info[idx + i].huge = false;
    }

    inline void free_huge(void* ptr) {
        i8* byte_ptr = (i8*)ptr;
        iptr offset = byte_ptr - (i8*)pages;
        i32 idx = offset / (BYTES_PER_WORD * WORDS_PER_PAGE);
        free_huge(idx);
    }

    /*
     * This is the slow path for allocating objects in a particular size class, called
     * when there are no available entries in that size's free-list. We try to grab a new
     * page, potentially invoking a GC. If a GC occurs, we call alloc() again, so that we
     * take advantage of any new free-list entries that may have opened up. If we truly
     * need a new page, however, we populate that page with new free-list entries, multiples
     * of the size class from the start of the page, and prepend those entries to that
     * free-list in the heap.
     */

    void* alloc_in_new_page(i32 size_class) {
        auto page = alloc_page_for(size_class);

        if (!page.first) // Did GC
            return alloc(size_class);

        u32 idx = page.first - pages;
        pair<u32, i32> list = free_lists[size_class];
        for (i32 i = i32(WORDS_PER_PAGE) - SIZE_CLASSES[size_class] - i32(WORDS_PER_PAGE) % SIZE_CLASSES[size_class]; i >= 0; i -= SIZE_CLASSES[size_class]) {
            *page.first->words[i].as_ptr<pair<u32, i32>>() = list;
            list = { idx, i };
        }
        void* result = pages[list.first].words + list.second;
        free_lists[size_class] = *(pair<u32, i32>*)result;
        return result;
    }

    /*
     * alloc() is the main entry point and fast path for allocating objects using the GC.
     * Once we have established the size class we want, we assume the fast path will be that
     * a free list entry exists for that size class. If this is the case, we take that entry
     * off the free-list and return the address it describes. If that free-list is exhausted,
     * represented by the terminal value {0, -1}, we try to allocate a new page for our size
     * class and potentially invoke a GC.
     */

    void* alloc(i32 size_class) {
        auto list_entry = free_lists[size_class];
        void* result;
        if (list_entry.second < 0) {
            result = alloc_in_new_page(size_class);
        }
        else { 
            result = pages[list_entry.first].words + list_entry.second;
            free_lists[size_class] = *(pair<u32, i32>*)result;
        }
        return result;
    }

    /*
     * These functions eagerly (as in, without waiting for a GC cycle) free up space in the heap.
     * Using either a provided size class, or by reading the size class for the containing page,
     * we add the pointed-to object to the free list for that size class so it may be used in future
     * allocations.
     * 
     * Through the use of free(), it's entirely possible to use this heap as a traditional malloc-style
     * allocator, without ever needing a GC cycle to clean up memory. It should perform pretty good here!
     * Some tentative tests show that we outperform most other mallocs on simple benchmarks. Of course,
     * a lot of that is due to the fact we don't bother protecting the heap from data races yet.
     */

    inline void free_sized(void* ptr, i32 size_class) {
        i8* byte_ptr = (i8*)ptr;
        iptr offset = byte_ptr - (i8*)pages;
        i32 idx = offset / (BYTES_PER_WORD * WORDS_PER_PAGE);
        *(pair<u32, i32>*)byte_ptr = free_lists[size_class];
        free_lists[size_class] = { (u32)idx, i32((word*)byte_ptr - pages[idx].words) };
    }

    inline void free(void* ptr) {
        i8* byte_ptr = (i8*)ptr;
        iptr offset = byte_ptr - (i8*)pages;
        i32 idx = offset / (BYTES_PER_WORD * WORDS_PER_PAGE);
        if (info[idx].huge) return free_huge(idx);
        i32 size_class = info[idx].size_class;
        *(pair<u32, i32>*)byte_ptr = free_lists[size_class];
        free_lists[size_class] = { (u32)idx, i32((word*)byte_ptr - pages[idx].words) };
    }
};

/*
 * THE_HEAP is the single instance of heap, used by all allocation and deallocation functions. In the
 * future, it might be nice to allow multiple of these. gc_init() and gc_deinit() are called by the
 * runtime to construct and destruct THE_HEAP.
 */

extern gc_heap THE_HEAP;

void gc_init();
void gc_deinit();

/*
 * gc_alloc is the intended user-facing entrypoint into allocating using the heap. The template version
 * should be used when type is known, calculating the size class automatically at compile time. The untyped
 * version can be used to allocate arbitrarily sized objects, with the slight overhead of needing to compute
 * the size class at runtime.
 */

template<typename T>
T* gc_alloc() {
    if (sizeof(T) > SIZE_CLASSES[N_SIZE_CLASSES - 1] * BYTES_PER_WORD) return (T*)THE_HEAP.alloc_huge_locked(sizeof(T));
    return (T*)THE_HEAP.alloc(size_class_for_const(sizeof(T)));
}

inline void* gc_alloc_untyped(iptr size) {
    if ((uptr)size > SIZE_CLASSES[N_SIZE_CLASSES - 1] * BYTES_PER_WORD) return THE_HEAP.alloc_huge_locked(size);
    return THE_HEAP.alloc(size_class_for(size));
}

/*
 * gc_free is, correspondingly, the intended user-facing entrypoint into eagerly freeing memory using the
 * heap. Like gc_alloc, gc_free() can be used to take advantage of type information to compute the size
 * class ahead of time, while gc_free_untyped() will need to compute this at runtime.
 */

template<typename T>
inline void gc_free(T*& ptr) {
    if (sizeof(T) > SIZE_CLASSES[N_SIZE_CLASSES - 1] * BYTES_PER_WORD) (T*)THE_HEAP.free_huge(ptr);
    else THE_HEAP.free_sized(ptr, size_class_for_const(sizeof(T)));
    ptr = nullptr; // Help reduce false positives.
}

inline void gc_free_untyped(void*& ptr) {
    THE_HEAP.free(ptr);
    ptr = nullptr; // Help reduce false positives.
}

template<typename T>
inline void gc_free(T* const& ptr) {
    if (sizeof(T) > SIZE_CLASSES[N_SIZE_CLASSES - 1] * BYTES_PER_WORD) (T*)THE_HEAP.free_huge(ptr);
    else THE_HEAP.free_sized(ptr, size_class_for_const(sizeof(T)));
}

inline void gc_free_untyped(void* const& ptr) {
    THE_HEAP.free(ptr);
}

/*
 * gc_new and gc_delete are intended to offer a C++-style interface, constructing and destructing
 * objects appropriately, while using the GC as a memory manager.
 */

void* operator new(size_t, void* ptr) noexcept;

template<typename T, typename... Args>
inline T* gc_new(Args&&... args) {
    return new(gc_alloc<T>()) T(args...);    
}

template<typename T>
void gc_delete(const T* ptr) {
    if (!ptr) return;
    ptr->~T();
    gc_free<T>(ptr);
}

#endif