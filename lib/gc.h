#ifndef BASIL_LIB_GC_H
#define BASIL_LIB_GC_H

#include "core/def.h"
#include "core/sys.h"
#include "lib/tuple.h"

constexpr uptr BYTES_PER_WORD = 16;
constexpr uptr PTRS_PER_WORD = BYTES_PER_WORD / sizeof(iptr);
constexpr uptr WORDS_PER_PAGE = 4096;
constexpr uptr BYTES_PER_PAGE = BYTES_PER_WORD * WORDS_PER_PAGE;
constexpr uptr DEFAULT_PAGES_PER_REGION = 16384;
static_assert(DEFAULT_PAGES_PER_REGION >= 64, "Minimum heap size is 1MB."); // Otherwise the live page bitmap won't work. Kinda dumb.
constexpr uptr MAX_PAGES_PER_REGION = 65536;
constexpr uptr BYTES_PER_REGION = DEFAULT_PAGES_PER_REGION * BYTES_PER_PAGE;
constexpr uptr MAX_BYTES_PER_REGION = MAX_PAGES_PER_REGION * BYTES_PER_PAGE;

constexpr u32 SIZE_CLASSES[] = {
    1, 2, 3, 4, 6, 8, 12, 16,
    24, 32, 48, 64, 96, 128, 192, 256,
    384, 512, 768, 1024
};

constexpr u64 SIZE_CLASS_MASKS[] = {
    0x3, 0xf, 0x3f, 0xff, 0x3ff, 0xfff, 0x3fff, 0xffff, 
    0x3ffff, 0xfffff, 0x3fffff, 0xffffff, 0x3ffffff, 0xfffffff, 0x3fffffff, 0xffffffff,
    0x3ffffffff, 0xfffffffff, 0x3fffffffff, 0xffffffffff
};

constexpr u16 N_SIZE_CLASSES = 20;

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
        default: return -1;
    }
}

inline i16 size_class_for(u32 bytes) {
    u32 words = (bytes + BYTES_PER_WORD - 1) / BYTES_PER_WORD;
    if (words > 1024 * BYTES_PER_WORD) return -1;
    i16 log2 = 32 - __builtin_clz(words - 1);
    i16 base = log2 * 2 - 1;
    if (words <= (1 << log2) - ((1 << log2) - 2))
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

struct gc_page_info {
    u32 n_huge_pages;
    i8 size_class;
    bool bump, huge, marked;

    inline gc_page_info():
        n_huge_pages(0), bump(false), huge(false), marked(false) {}
};

struct gc_stats {
    iptr freed_bytes = 0, n_gcs = 0;
    i64 last_gc_ns, since_gc_ns, gc_time_ns;
};

extern gc_stats stats;

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

struct gc_heap {
    gc_page* pages;
    gc_page_info* info;
    pair<u32, i32> free_lists[N_SIZE_CLASSES];
    u32 top_page, page_limit;
    i64 live_page_bitmap[MAX_PAGES_PER_REGION / 64];
    gc_page* max_pages;
    bool just_did_gc = false;

    inline gc_heap(): top_page(0), page_limit(DEFAULT_PAGES_PER_REGION) {
        for (u32 i = 0; i < N_SIZE_CLASSES; i ++)
            free_lists[i] = { 0, -1 };
        constexpr u32 initial_size = DEFAULT_PAGES_PER_REGION * WORDS_PER_PAGE * BYTES_PER_WORD;
        auto aligned_pages = map_aligned(MAX_BYTES_PER_REGION, initial_size);
        auto aligned_info = map_aligned(MAX_PAGES_PER_REGION * sizeof(gc_page_info), PAGESIZE);
        pages = (gc_page*)aligned_pages.ptr;
        info = (gc_page_info*)aligned_info.ptr;
        for (u32 i = 0; i < page_limit; i ++) 
            info[i].size_class = -1;
        mset(live_page_bitmap, 0, sizeof(live_page_bitmap));
    }

    inline bool try_grow(u32 pages) {
        if (page_limit == MAX_PAGES_PER_REGION)
            return false;
        page_limit += pages;
        if (page_limit > MAX_PAGES_PER_REGION)
            page_limit = MAX_PAGES_PER_REGION;
        return true;
    }

    inline void mark_page_bitmap(u32 idx) {
        live_page_bitmap[idx / 64] |= 1ul << (idx % 64);
    }

    inline void clear_page_bitmap(u32 idx) {
        live_page_bitmap[idx / 64] &= ~(1ul << (idx % 64));
    }

    inline i32 next_free_page() {
        for (i64 i = 0; i < (page_limit + 63) / 64; i ++) if (live_page_bitmap[i] != 0xffffffffffffffffl) {
            return i * 64 + __builtin_ctzll(~live_page_bitmap[i]);
        }
        return -1;
    }

    inline void mark_pages(u32 idx, u32 n_pages) {
        for (i32 i = idx; i < idx + n_pages; i ++) 
            live_page_bitmap[i / 64] |= 1ul << (i % 64);
    }

    inline void clear_pages(u32 idx, u32 n_pages) {
        for (i32 i = idx; i < idx + n_pages; i ++) 
            live_page_bitmap[i / 64] &= ~(1ul << (i % 64));
    }

    inline i32 find_free_pages(u32 n_pages) {
        for (i64 i = 0; i < (page_limit + 63) / 64; i ++) if (live_page_bitmap[i] != 0xffffffffffffffffl) {
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

    inline bool should_grow_heap_heuristic() {
        return stats.freed_bytes < (uptr)page_limit * (uptr)BYTES_PER_PAGE / 8
            ; // || stats.since_gc_ns < 2 * stats.gc_time_ns; <-- Should we use a temporal heuristic too?
    }

    inline pair<gc_page*, gc_page_info> alloc_page_for(i32 size_class) {
        i32 result;
        if (top_page < page_limit) // If there are bump-allocatable pages in the arena.
            result = top_page ++;
        else if ((result = next_free_page()) >= 0) {
            // fall through to end
        }
        else {
            pushrframe(gc);
            return { nullptr, gc_page_info() };
        }

        info[result].size_class = size_class;
        mark_page_bitmap(result);
        return { pages + result, info[result] };
    }
    
    inline i32 max(i32 a, i32 b) {
        return a > b ? a : b;
    }
    
    inline void* alloc_huge(i32 size) {
        i32 n_pages = (size + BYTES_PER_PAGE - 1) / BYTES_PER_PAGE;
        if (top_page + n_pages <= page_limit) {
            void* result = pages + top_page;
            mark_pages(top_page, n_pages);
            info[top_page].huge = true;
            info[top_page].n_huge_pages = n_pages;
            top_page += n_pages;
            return result;
        }
        else {
            i32 next_page = find_free_pages(n_pages);
            if (next_page == -1) {
                pushrframe(gc);
                next_page = find_free_pages(n_pages);
                if (should_grow_heap_heuristic() || next_page == -1)
                    if (!try_grow(max(n_pages, DEFAULT_PAGES_PER_REGION)) && next_page == -1)
                        fatal("Out of memory!");
                return alloc_huge(size);
            }
            mark_pages(next_page, n_pages);
            info[next_page].huge = true;
            info[next_page].n_huge_pages = n_pages;
            return pages + next_page;
        }
    }

    inline void* alloc_in_new_page(i32 size_class) {
        auto page = alloc_page_for(size_class);
        if (!page.first) { // Did GC 
            auto list_entry = free_lists[size_class];
            if (stats.freed_bytes < (uptr)page_limit * (uptr)BYTES_PER_PAGE / 16 || list_entry.second < 0)
                if (!try_grow(DEFAULT_PAGES_PER_REGION) && list_entry.second < 0)
                    fatal("Out of memory!");
            return alloc(size_class);
        }
        u32 idx = page.first - pages;
        pair<u32, i32> list = { idx, -1 };
        for (i32 i = i32(WORDS_PER_PAGE) - SIZE_CLASSES[size_class] - i32(WORDS_PER_PAGE) % SIZE_CLASSES[size_class]; i >= 0; i -= SIZE_CLASSES[size_class]) {
            *page.first->words[i].as_ptr<pair<u32, i32>>() = list;
            list = { idx, i };
        }
        void* result = pages[list.first].words + list.second;
        free_lists[size_class] = *(pair<u32, i32>*)result;
        return result;
    }

    inline void* alloc(i32 size_class) {
        auto list_entry = free_lists[size_class];
        if (list_entry.second < 0) return alloc_in_new_page(size_class);
        void* result = pages[list_entry.first].words + list_entry.second;
        free_lists[size_class] = *(pair<u32, i32>*)result;
        return result;
    }

    inline void free_huge(i32 idx) {
        i32 n_pages = info[idx].n_huge_pages;
        clear_pages(idx, n_pages);
        info[idx].huge = false;
    }

    inline void free_huge(void* ptr) {
        i8* byte_ptr = (i8*)ptr;
        iptr offset = byte_ptr - (i8*)pages;
        i32 idx = offset / (BYTES_PER_WORD * WORDS_PER_PAGE);
        free_huge(idx);
    }

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

extern gc_heap THE_HEAP;

void gc_init();

template<typename T>
T* gc_alloc() {
    if (sizeof(T) > 16384) return (T*)THE_HEAP.alloc_huge(sizeof(T));
    return (T*)THE_HEAP.alloc(size_class_for_const(sizeof(T)));
}

inline void* gc_alloc_untyped(iptr size) {
    if (size > 16384) return THE_HEAP.alloc_huge(size);
    return THE_HEAP.alloc(size_class_for(size));
}

template<typename T>
inline void gc_free(T* ptr) {
    if (sizeof(T) > 16384) return (T*)THE_HEAP.free_huge(ptr);
    THE_HEAP.free_sized(ptr, size_class_for_const(sizeof(T)));
}

inline void gc_free_untyped(void* ptr) {
    THE_HEAP.free(ptr);
}

void* operator new(size_t, void* ptr);

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

void gc_deinit();

#endif