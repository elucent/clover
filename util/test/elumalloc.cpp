#include "util/test/harness.h"
#include "util/malloc.h"
#include "util/thread.h"
#include "util/elumalloc.h"

using namespace elumalloc;

struct Foo {
    u32 data[8];

    void* operator new(size_t);
    void operator delete(void*);
};

TEST(primitive_heap_construct) {
    PrimitiveHeap<Foo> heap;
}

TEST(primitive_heap_alloc_free_single) {
    PrimitiveHeap<Foo> heap;

    Foo* a = heap.malloc();
    heap.free(a);
    Foo* b = heap.malloc();
    ASSERT_EQUAL(a, b);
    heap.free(b);

    heap.destroy();
}

TEST(primitive_heap_alloc_free_10) {
    PrimitiveHeap<Foo> heap;

    Foo* ptrs[10];
    for (u32 i = 0; i < 10; i ++)
        ptrs[i] = heap.malloc();
    for (u32 i = 0; i < 10; i ++)
        heap.free(ptrs[i]);

    heap.destroy();
}

TEST(primitive_heap_alloc_free_10k) {
    PrimitiveHeap<Foo> heap;

    Foo* ptrs[10000];
    for (u32 i = 0; i < 10000; i ++) {
        ptrs[i] = heap.malloc();
        ptrs[i]->data[0] = i;
    }
    for (u32 i = 0; i < 10000; i ++) {
        ASSERT_EQUAL(ptrs[i]->data[0], i);
        heap.free(ptrs[i]);
    }

    heap.destroy();
}

struct ChaosLCG {
    constexpr static u64
        a = 16854749770870987091ull,
        c = 15284698627294678789ull;
    u64 x;

    inline u64 next() {
        return x = (a * x + c);
    }
};

TEST(elumalloc_primitive_heap_controlled_chaos) {
    ChaosLCG lcg;
    lcg.x = ChaosLCG::c + 127;

    PrimitiveHeap<Foo> heap;
    Foo* old[16384];
    Foo* young[65536];
    for (u32 i = 0; i < 16384; i ++)
        old[i] = nullptr;
    for (u32 i = 0; i < 65536; i ++)
        young[i] = nullptr;

    for (u32 i = 0; i < 256 * 1024 * 1024; i ++) {
        u64 rng = lcg.next();

        bool isOld = (rng & 0xffu) < 2;
        rng >>= 8;

        u32 idx = rng & 0xffffu;
        rng >>= 16;
        if (isOld)
            idx %= 16384;
        Foo*& slot = isOld ? old[idx] : young[idx];
        if (slot && rng & 1) {
            heap.free(slot);
            slot = nullptr;
        }
        if (rng & 2 && !slot)
            slot = heap.malloc();
    }

    heap.destroy();
}

TEST(size_classes) {
#if ELUMALLOC_USE_POWER_OF_TWO_SIZE_CLASSES
    ASSERT_EQUAL(Block::sizeClassFor(1), 0);
    ASSERT_EQUAL(Block::sizeClassFor(2), 0);
    ASSERT_EQUAL(Block::sizeClassFor(3), 0);
    ASSERT_EQUAL(Block::sizeClassFor(4), 0);

    ASSERT_EQUAL(Block::sizeClassFor(15), 0);
    ASSERT_EQUAL(Block::sizeClassFor(16), 0);
    ASSERT_EQUAL(Block::sizeClassFor(17), 1);

    ASSERT_EQUAL(Block::sizeClassFor(31), 1);
    ASSERT_EQUAL(Block::sizeClassFor(32), 1);
    ASSERT_EQUAL(Block::sizeClassFor(33), 2);

    ASSERT_EQUAL(Block::sizeClassFor(47), 2);
    ASSERT_EQUAL(Block::sizeClassFor(48), 2);
    ASSERT_EQUAL(Block::sizeClassFor(49), 2);

    ASSERT_EQUAL(Block::sizeClassFor(63), 2);
    ASSERT_EQUAL(Block::sizeClassFor(64), 2);
    ASSERT_EQUAL(Block::sizeClassFor(65), 3);

    ASSERT_EQUAL(Block::sizeClassFor(79), 3);
    ASSERT_EQUAL(Block::sizeClassFor(80), 3);
    ASSERT_EQUAL(Block::sizeClassFor(81), 3);

    ASSERT_EQUAL(Block::sizeClassFor(95), 3);
    ASSERT_EQUAL(Block::sizeClassFor(96), 3);
    ASSERT_EQUAL(Block::sizeClassFor(97), 3);

    ASSERT_EQUAL(Block::sizeClassFor(111), 3);
    ASSERT_EQUAL(Block::sizeClassFor(112), 3);
    ASSERT_EQUAL(Block::sizeClassFor(113), 3);

    ASSERT_EQUAL(Block::sizeClassFor(127), 3);
    ASSERT_EQUAL(Block::sizeClassFor(128), 3);
    ASSERT_EQUAL(Block::sizeClassFor(129), 4);

    ASSERT_EQUAL(Block::sizeClassFor(159), 4);
    ASSERT_EQUAL(Block::sizeClassFor(160), 4);
    ASSERT_EQUAL(Block::sizeClassFor(161), 4);

    ASSERT_EQUAL(Block::sizeClassFor(191), 4);
    ASSERT_EQUAL(Block::sizeClassFor(192), 4);
    ASSERT_EQUAL(Block::sizeClassFor(193), 4);

    ASSERT_EQUAL(Block::sizeClassFor(223), 4);
    ASSERT_EQUAL(Block::sizeClassFor(224), 4);
    ASSERT_EQUAL(Block::sizeClassFor(225), 4);

    ASSERT_EQUAL(Block::sizeClassFor(255), 4);
    ASSERT_EQUAL(Block::sizeClassFor(256), 4);
    ASSERT_EQUAL(Block::sizeClassFor(257), 5);
#else
    ASSERT_EQUAL(Block::sizeClassFor(1), 0);
    ASSERT_EQUAL(Block::sizeClassFor(2), 0);
    ASSERT_EQUAL(Block::sizeClassFor(3), 0);
    ASSERT_EQUAL(Block::sizeClassFor(4), 0);

    ASSERT_EQUAL(Block::sizeClassFor(15), 0);
    ASSERT_EQUAL(Block::sizeClassFor(16), 0);
    ASSERT_EQUAL(Block::sizeClassFor(17), 1);

    ASSERT_EQUAL(Block::sizeClassFor(31), 1);
    ASSERT_EQUAL(Block::sizeClassFor(32), 1);
    ASSERT_EQUAL(Block::sizeClassFor(33), 2);

    ASSERT_EQUAL(Block::sizeClassFor(47), 2);
    ASSERT_EQUAL(Block::sizeClassFor(48), 2);
    ASSERT_EQUAL(Block::sizeClassFor(49), 3);

    ASSERT_EQUAL(Block::sizeClassFor(63), 3);
    ASSERT_EQUAL(Block::sizeClassFor(64), 3);
    ASSERT_EQUAL(Block::sizeClassFor(65), 4);

    ASSERT_EQUAL(Block::sizeClassFor(79), 4);
    ASSERT_EQUAL(Block::sizeClassFor(80), 4);
    ASSERT_EQUAL(Block::sizeClassFor(81), 5);

    ASSERT_EQUAL(Block::sizeClassFor(95), 5);
    ASSERT_EQUAL(Block::sizeClassFor(96), 5);
    ASSERT_EQUAL(Block::sizeClassFor(97), 6);

    ASSERT_EQUAL(Block::sizeClassFor(111), 6);
    ASSERT_EQUAL(Block::sizeClassFor(112), 6);
    ASSERT_EQUAL(Block::sizeClassFor(113), 7);

    ASSERT_EQUAL(Block::sizeClassFor(127), 7);
    ASSERT_EQUAL(Block::sizeClassFor(128), 7);
    ASSERT_EQUAL(Block::sizeClassFor(129), 8);

    ASSERT_EQUAL(Block::sizeClassFor(159), 8);
    ASSERT_EQUAL(Block::sizeClassFor(160), 8);
    ASSERT_EQUAL(Block::sizeClassFor(161), 9);

    ASSERT_EQUAL(Block::sizeClassFor(191), 9);
    ASSERT_EQUAL(Block::sizeClassFor(192), 9);
    ASSERT_EQUAL(Block::sizeClassFor(193), 10);

    ASSERT_EQUAL(Block::sizeClassFor(223), 10);
    ASSERT_EQUAL(Block::sizeClassFor(224), 10);
    ASSERT_EQUAL(Block::sizeClassFor(225), 11);

    ASSERT_EQUAL(Block::sizeClassFor(255), 11);
    ASSERT_EQUAL(Block::sizeClassFor(256), 11);
    ASSERT_EQUAL(Block::sizeClassFor(257), 12);
#endif
}

struct SmallNode {
    ELUMALLOC_COMMON_HEAPED;

    u64 value;
    SmallNode* next;
};

struct LargeNode {
    ELUMALLOC_COMMON_HEAPED;

    u64 value;
    LargeNode* next;
    u64 stuff[126];
};

TEST(elumalloc_single_block_linked_list) {
    Heap heap;
    auto handle = heap.handle();

    SmallNode* list = nullptr;
    for (u32 i = 0; i < 1000; i ++) {
        list = new SmallNode {
            i,
            list
        };
    }
    u32 sum = 0;
    SmallNode* iter = list;
    while (iter) {
        sum += iter->value;
        iter = iter->next;
    }
    ASSERT_EQUAL(sum, (1000 * 999) / 2);

    iter = list;
    while (iter) {
        SmallNode* node = iter;
        iter = iter->next;
        delete node;
    }
}

TEST(elumalloc_multi_block_linked_list) {
    Heap heap;
    auto handle = heap.handle();

    SmallNode* list = nullptr;
    for (u32 i = 0; i < 10000; i ++) {
        list = new SmallNode {
            i,
            list
        };
    }
    u32 sum = 0;
    SmallNode* iter = list;
    while (iter) {
        sum += iter->value;
        iter = iter->next;
    }
    ASSERT_EQUAL(sum, (10000 * 9999) / 2);

    iter = list;
    while (iter) {
        SmallNode* node = iter;
        iter = iter->next;
        delete node;
    }
}

TEST(elumalloc_multi_block_linked_list_reuse) {
    Heap heap;
    auto handle = heap.handle();

    for (u32 k = 0; k < 10; k ++) {
        SmallNode* list = nullptr;
        for (u32 i = 0; i < 10000; i ++) {
            list = new SmallNode {
                i,
                list
            };
        }
        u32 sum = 0;
        SmallNode* iter = list;
        while (iter) {
            sum += iter->value;
            iter = iter->next;
        }
        ASSERT_EQUAL(sum, (10000 * 9999) / 2);

        iter = list;
        while (iter) {
            SmallNode* node = iter;
            iter = iter->next;
            delete node;
        }
    }
}

TEST(elumalloc_large_node_linked_list) {
    Heap heap;
    auto handle = heap.handle();

    LargeNode* list = nullptr;
    for (u32 i = 0; i < 1000; i ++) {
        list = new LargeNode {
            i,
            list,
            {}
        };
    }
    u32 sum = 0;
    LargeNode* iter = list;
    while (iter) {
        sum += iter->value;
        iter = iter->next;
    }
    ASSERT_EQUAL(sum, (1000 * 999) / 2);

    iter = list;
    while (iter) {
        LargeNode* node = iter;
        iter = iter->next;
        delete node;
    }
}

TEST(malloc_super_simple_bench) {
    void* ptrs[65536];
    for (u32 i = 0; i < 65536; i ++)
        ptrs[i] = nullptr;
    for (u32 i = 0; i < 256 * KB; i ++) {
        u32 j = i % 65536;
        if (ptrs[j])
            ::free(ptrs[j]);
        ptrs[j] = malloc(32);
    }
}

TEST(elumalloc_super_simple_bench) {
    void* ptrs[65536];
    for (u32 i = 0; i < 65536; i ++)
        ptrs[i] = nullptr;
    for (u32 i = 0; i < 256 * KB; i ++) {
        u32 j = i % 65536;
        if (ptrs[j])
            elumalloc::freeOutOfLine(ptrs[j]);
        ptrs[j] = elumalloc::allocateOutOfLine(32);
    }
}

TEST(elumalloc_nightmare_free_list) {
    u32* ptrs[65536];
    for (u32 i = 0; i < 65536; i ++)
        ptrs[i] = nullptr;
    for (u32 i = 0; i < 16; i ++) {
        for (u32 j = 0; j < 65536; j ++) {
            if (!ptrs[j])
                ptrs[j] = (u32*)elumalloc::allocateOutOfLine(32), *ptrs[j] = 42;
        }
        for (u32 j = i & 1; j < 65536; j += 2) {
            if (ptrs[j])
                elumalloc::freeOutOfLine(ptrs[j]), ptrs[j] = nullptr;
        }
    }
}

TEST(elumalloc_medium_allocations) {
    u32* ptrs[1024];
    for (u32 k = 0; k < 16; k ++) {
        for (u32 i = 0; i < 1024; i ++) {
            ptrs[i] = (u32*)elumalloc::allocateOutOfLine(16384);
            for (u32 j = 0; j < 4096; j ++)
                ptrs[i][j] = j;
        }
        for (u32 i = 0; i < 1024; i ++) {
            u32 sum = 0;
            for (u32 j = 0; j < 4096; j ++)
                sum += ptrs[i][j];
            ASSERT_EQUAL(sum, 8386560);
        }
        for (u32 i = 0; i < 1024; i ++)
            elumalloc::freeOutOfLine(ptrs[i]);

        for (u32 i = 0; i < 1024; i ++) {
            ptrs[i] = (u32*)elumalloc::allocateOutOfLine(65536);
            for (u32 j = 0; j < 16384; j ++)
                ptrs[i][j] = j;
        }
        for (u32 i = 0; i < 1024; i ++) {
            u32 sum = 0;
            for (u32 j = 0; j < 16384; j ++)
                sum += ptrs[i][j];
            ASSERT_EQUAL(sum, 134209536);
        }
        for (u32 i = 0; i < 1024; i ++)
            elumalloc::freeOutOfLine(ptrs[i]);

        for (u32 i = 0; i < 1024; i ++) {
            ptrs[i] = (u32*)elumalloc::allocateOutOfLine(262144);
            for (u32 j = 0; j < 65536; j ++)
                ptrs[i][j] = j;
        }
        for (u32 i = 0; i < 1024; i ++) {
            u32 sum = 0;
            for (u32 j = 0; j < 65536; j ++)
                sum += ptrs[i][j];
            ASSERT_EQUAL(sum, 2147450880);
        }
        for (u32 i = 0; i < 1024; i ++)
            elumalloc::freeOutOfLine(ptrs[i]);
    }
}

TEST(elumalloc_cover_all_size_classes) {
    void* ptrs[Block::NumSizeClasses * 64];
    void* large[64];
    for (u32 k = 0; k < 160; k ++) {
        for (u32 i = 0; i < 64; i ++) {
            for (u32 j = 0; j < Block::NumSizeClasses; j ++)
                ptrs[i * Block::NumSizeClasses + j] = elumalloc::allocate(Block::SizeClasses[j]);
            large[i] = elumalloc::allocate(Block::MediumBlockSize);
        }
        for (u32 i = 0; i < 64; i ++) {
            for (u32 j = 0; j < Block::NumSizeClasses; j ++)
                elumalloc::free(ptrs[i * Block::NumSizeClasses + j]);
            elumalloc::free(large[i]);
        }
    }
}

TEST(elumalloc_controlled_chaos_one_thread) {
    void* old[16384];
    void* young[65536];

    ChaosLCG lcg;
    lcg.x = lcg.c;

    for (u32 i = 0; i < 16384; i ++)
        old[i] = nullptr;
    for (u32 i = 0; i < 65536; i ++)
        young[i] = nullptr;

    for (u32 i = 0; i < 256 * 1024 * 1024; i ++) {
        u64 rng = lcg.next();

        bool isOld = (rng & 0xffu) < 2;
        rng >>= 8;

        u64 allocSize = rng & 0xffu;
        rng >>= 8;

        bool isLarge = (rng & 0xffu) < 1;
        bool isMedium = (rng & 0xffu) < 5;
        bool isntThatSmall = (rng & 0xffu) < 32;
        rng >>= 8;

        if (isLarge)
            allocSize *= 65536;
        else if (isMedium)
            allocSize *= 1024;
        else if (isntThatSmall)
            allocSize *= 32;
        if (allocSize == 0)
            allocSize = 1;

        u32 idx = rng & 0xffffu;
        rng >>= 16;
        if (isOld)
            idx %= 16384;
        void*& slot = isOld ? old[idx] : young[idx];
        if (slot && rng & 1)
            elumalloc::freeOutOfLine(slot), slot = nullptr;
        if (rng & 2 && !slot)
            slot = elumalloc::allocateOutOfLine(allocSize);
    }
}

void elumalloc_single_alloc_free_thread(u64) {
    for (u32 i = 0; i < 1024 * 1024; i ++) {
        void* obj = elumalloc::allocateOutOfLine(32);
        elumalloc::freeOutOfLine(obj);
    }
}

TEST(elumalloc_alloc_free_once_multi_thread) {
    SKIP_IF_USING_LIBC;
    thread<void(u64)>* threads[4];
    for (u32 i = 0; i < 4; i ++)
        threads[i] = new thread<void(u64)>(elumalloc_single_alloc_free_thread, 0);
    for (u32 i = 0; i < 4; i ++)
        delete threads[i];
}

void elumalloc_chaos_allocator_thread(u64 seed) {
    void* old[16384];
    void* young[65536];

    ChaosLCG lcg;
    lcg.x = seed;

    for (u32 i = 0; i < 16384; i ++)
        old[i] = nullptr;
    for (u32 i = 0; i < 65536; i ++)
        young[i] = nullptr;

    for (u32 i = 0; i < 256 * 1024 * 1024; i ++) {
        u64 rng = lcg.next();

        bool isOld = (rng & 0xffu) < 2;
        rng >>= 8;

        u64 allocSize = rng & 0xffu;
        rng >>= 8;

        bool isLarge = (rng & 0xffu) < 1;
        bool isMedium = (rng & 0xffu) < 5;
        bool isntThatSmall = (rng & 0xffu) < 32;
        rng >>= 8;

        if (isLarge)
            allocSize *= 65536;
        else if (isMedium)
            allocSize *= 1024;
        else if (isntThatSmall)
            allocSize *= 32;
        if (allocSize == 0)
            allocSize = 1;

        u32 idx = rng & 0xffffu;
        rng >>= 16;
        if (isOld)
            idx %= 16384;
        void*& slot = isOld ? old[idx] : young[idx];
        if (slot && rng & 1)
            elumalloc::freeOutOfLine(slot), slot = nullptr;
        if (rng & 2 && !slot)
            slot = elumalloc::allocateOutOfLine(allocSize);
    }
}

TEST(elumalloc_controlled_chaos_multi_thread) {
    SKIP_IF_USING_LIBC;
    thread<void(u64)>* threads[4];
    for (u32 i = 0; i < 4; i ++)
        threads[i] = new thread<void(u64)>(elumalloc_chaos_allocator_thread, (i + 1) * 4242424242ull);
    for (u32 i = 0; i < 4; i ++)
        delete threads[i];
}

TEST(out_of_line_usage) {
    volatile bool cond = false;

    if (cond) {
        ASSERT(sizeClassForOutOfLine(42));
        ASSERT(sizeForClassOutOfLine(16));
        ASSERT(elumalloc::allocateOutOfLine(1));
        elumalloc::freeOutOfLine(nullptr);
    }
}