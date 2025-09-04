#include "util/elumalloc.h"

i8 allocLogLock = 0;

slice<memory::page> mapAligned(u64 size, u64 align) {
    assert(!(align & align - 1));
    size = (size + align - 1) / align * align;
    u64 mapPages = (size + memory::PAGESIZE - 1) / memory::PAGESIZE + (align + memory::PAGESIZE - 1) * 2 / memory::PAGESIZE;
    auto pages = memory::map(mapPages);

    uptr base = uptr(pages.data()) + align - (uptr(pages.data()) % align);
    assert(base % align == 0);

    uptr end = uptr(pages.data() + pages.size());
    if (uptr(pages.data()) != base)
        memory::unmap({ pages.data(), iword(base - uptr(pages.data())) / memory::PAGESIZE });
    if (end != base + size)
        memory::unmap({ (memory::page*)(base + size), iword(end - (base + size)) / memory::PAGESIZE });

    ALLOC_LOG(1, "Mapped aligned pages (", size, " bytes, ", align, " aligned) from ", hex(base), " to ", hex(base + size));
    return { (memory::page*)base, iword(size / memory::PAGESIZE) };
}

static i8 smallToMediumMappingLock = 0;

PrimitiveHeap<BlockTree::Node> BlockTree::NodeHeap;
Heap commonHeap;

MultiplierAndShift Block::SizeClassDivisionMagic[Block::NumSizeClasses];

NOINLINE FreeList* Block::takeList() {
    assert(!header.isLarge());
    ALLOC_LOG(2, "Building list from free bits in block ", hex(uptr(this)), " with size ", header.allocationSize());

    FreeList* list = nullptr;
    FreeList* head = nullptr;
    u32 allocationSize = header.allocationSize();
    u32 incrementPerWord = allocationSize * 64;
    u64 wordIndex = 0;
    u64 numWords = (header.allocationsPerBlock + 63) / 64;
    u8* begin = bitcast<u8*>(startOfAllocatableRegion());

    while (wordIndex < numWords) {
        u64* word = header.bitmap + wordIndex;
        u64 bits = atomics::swap(word, 0);
        ALLOC_LOG(3, "Loaded new bit word ", binary(bits));

        while (bits) {
            u64 firstWordIndex = wordIndex;
            u64 first = ctz64(bits);
            u64 len = ctz64(~(bits >> first));
            if (first + len == 64) {
                u64 localLen;
                wordIndex ++;
                if (wordIndex < numWords) {
                    do {
                        word = header.bitmap + wordIndex;
                        bits = atomics::swap(word, 0);
                        ALLOC_LOG(3, "Loaded new overflow bit word ", binary(bits));
                        localLen = ctz64(~bits);
                        len += localLen;
                    } while (localLen == 64 && wordIndex < numWords);
                    bits &= 0xffffffffffffffffull << localLen;
                } else
                    bits &= 0xffffffffffffffffull << (first + len);
            } else
                bits &= 0xffffffffffffffffull << (first + len);

            u8* next = begin + firstWordIndex * incrementPerWord + first * allocationSize;
            if (list)
                list->offsetToNext = next - bitcast<u8*>(list);
            else
                head = bitcast<FreeList*>(next);

            #if ELUMALLOC_MARK_DEAD_ON_FREE
                process::lock(&markDeadLock);
                u64* iter = bitcast<u64*>(next);
                u64* end = iter + len * allocationSize / 8;
                while (iter != end) {
                    if (*iter ++ != 0xdeadbeefdeadbeef)
                        panic("Heap corruption! Tried to install free list in non-dead cell.");
                }
                process::unlock(&markDeadLock);
            #endif

            list = bitcast<FreeList*>(next);
            list->size = len * allocationSize;

            ALLOC_LOG(3, " - Created new freelist entry of size ", list->size, ", bit word is now ", binary(bits));
        }
        wordIndex ++;
    }

    if (list) {
        // Tie up the list with a sentinel in the last value. We do this by
        // storing the negative address of the list in the offset to the next
        // interval, since this means we'll actually compute a null pointer.
        list->offsetToNext = -bitcast<i64>(list);

        #if ELUMALLOC_VALIDATE_FREE_LIST
            uptr iter = bitcast<uptr>(head);
            uptr begin = bitcast<uptr>(this);
            uptr end = begin + (header.sizeClass >= FirstMediumSizeClass ? MediumBlockSize : SmallBlockSize);

            while (iter) {
                assert(iter >= begin + sizeof(Header) && iter < end);
                iter += bitcast<FreeList*>(iter)->offsetToNext;
            }
        #endif

        #if ELUMALLOC_VERBOSE
            {
                uptr iter = bitcast<uptr>(head);
                print("[ALLOC]\tSet up free interval list in block ", hex(uptr(this)), ": ");
                while (iter) {
                    print(hex(iter), "(size=", bitcast<FreeList*>(iter)->size, ", offset=", bitcast<FreeList*>(iter)->offsetToNext, ") -> ");
                    iter += bitcast<FreeList*>(iter)->offsetToNext;
                }
                println();
            }
        #endif
    }
    return head;
}

namespace elumalloc {
    i8 globalInitializationLock = 0;
    bool isGloballyInitialized = 0;
}

NOINLINE void* Allocator::allocateSlow(HeapHandle& handle, u64 sizeClass, u64 roundedSize) {
    if UNLIKELY(!elumalloc::isGloballyInitialized) {
        process::lock(&elumalloc::globalInitializationLock);
        if (!elumalloc::isGloballyInitialized) {
            elumalloc::isGloballyInitialized = true;
            elumalloc::initialize();
        }
        process::unlock(&elumalloc::globalInitializationLock);
    }
    if UNLIKELY(!handle.heap) {
        elumalloc::initializeThread();
        return allocate(handle, sizeClass, roundedSize);
    }
    ALLOC_LOG(1, "Hit allocation slow path for size class ", Block::SizeClasses[sizeClass]);
    Heap& heap = *handle.heap;
    SyncList<Block>& blocks = heap.blocksBySize[sizeClass];

    #if ELUMALLOC_VERIFY_BLOCK_LISTS
        for (u32 sz = 0; sz < Block::NumSizeClasses; sz ++) {
            SyncList<Block>& blocks = heap.blocksBySize[sz];
            auto iter = blocks.head;
            if (iter) do {
                auto item = iter->item;
                if (item->header.sizeClass != sz) {
                    println("Found size class mismatch between block ", hex(uptr(item)), " with size class ", Block::SizeClasses[item->header.sizeClass], " and containing list with size class ", Block::SizeClasses[sz]);
                    panic("Found size class mismatch!");
                }
                iter = iter->next;
            } while (iter != blocks.head);
        }
    #endif

    if (Block* block = blocks.pop()) {
        FreeList* list = block->takeList();
        blocks.push(block);
        ALLOC_LOG(3, "Appending block ", hex(uptr(block)), " to block list for size class ", Block::SizeClasses[sizeClass]);
        u32 numberToTry = 3;
        while (!list && numberToTry --) {
            block = blocks.pop();
            list = block->takeList();
            blocks.push(block);
        }
        if (list) {
            bottom = bitcast<u8*>(list);
            top = bottom + list->size;
            next = bottom + list->offsetToNext;
            ALLOC_LOG(1, "Found list ", hex(uptr(list)), " in block ", hex(uptr(block)));
            return allocate(handle, sizeClass, roundedSize);
        }
    }

    ALLOC_LOG(1, "Acquiring new block for size class ", Block::SizeClasses[sizeClass]);
    Block* block;
    if (sizeClass < Block::FirstMediumSizeClass && (block = heap.freeSmallBlocks.pop())) {
        block->initializeWithSize(sizeClass, roundedSize);
        blocks.push(block);
        ALLOC_LOG(1, "Used existing free small block ", hex(uptr(block)));
        ALLOC_LOG(3, "Appending block ", hex(uptr(block)), " to block list for size class ", Block::SizeClasses[sizeClass]);
    } else {
        block = elumalloc::getLargeBlock(handle, Block::MediumBlockSize);
        block->initializeWithSize(sizeClass, roundedSize);
        blocks.push(block);
        ALLOC_LOG(3, "Appending block ", hex(uptr(block)), " to block list for size class ", Block::SizeClasses[sizeClass]);
        ALLOC_LOG(1, "Mapped new block ", hex(uptr(block)));
        if (block->header.isSmall()) {
            uptr ptr = uptr(block) + Block::SmallBlockSize;
            while (ptr != uptr(block) + Block::MediumBlockSize) {
                ALLOC_LOG(1, "Recorded free small block ", hex(uptr(ptr)));
                heap.freeSmallBlocks.push((Block*)ptr);
                ptr += Block::SmallBlockSize;
            }
        }
    }
    bottom = bitcast<u8*>(block->startOfAllocatableRegion());
    top = bottom + block->header.allocationsPerBlock * block->header.allocationSize();
    next = nullptr;
    ALLOC_LOG(2, "Set up initial bumpable region of ", top - bottom, " bytes in block ", hex(uptr(block)), " with size class ", block->header.allocationSize(), " and ", block->header.allocationsPerBlock, " allocations per block");
    return allocate(handle, sizeClass, roundedSize);
}

namespace AllocStats {
    u64 bumpAllocs = 0, freeListAllocs = 0, slowPathAllocs = 0, largeAllocs = 0;
    u64 totalAllocatedBytes = 0;
}

#if ELUMALLOC_MARK_DEAD_ON_FREE
    i8 markDeadLock = 0;
#endif

namespace elumalloc {
    NOINLINE Block* getLargeBlock(HeapHandle& handle, u64 size) {
        Heap& heap = *handle.heap;
        if (!heap.largeBlocks.empty()) {
            if (Block* block = heap.largeBlocks.takeNextLarger(size))
                return block;
        }
        u64 roundedSize = ((size + Block::MediumBlockSize - 1) / Block::MediumBlockSize) * Block::MediumBlockSize;
        u64 mappedSize = max(32 * Block::MediumBlockSize, roundedSize);
        auto pages = mapAligned(mappedSize, Block::MediumBlockSize);
        Block* block = bitcast<Block*>(pages.data());
        block->initializeLarge(roundedSize);
        if (mappedSize > roundedSize) {
            Block* next = bitcast<Block*>(bitcast<u8*>(block) + roundedSize);
            next->initializeLarge(mappedSize - roundedSize);
            heap.largeBlocks.insert(next);
        }
        return block;
    }

    NOINLINE void freeLargeBlock(HeapHandle& handle, Block* block) {
        assert(block->header.isLarge());
        Heap& heap = *handle.heap;
        heap.largeBlocks.insert(block);

        ALLOC_LOG(1, "Freed large object of size ", block->header.largeSize(), " at ", hex(uptr(block->startOfAllocatableRegion())));

        #if ELUMALLOC_MARK_DEAD_ON_FREE
            process::lock(&markDeadLock);
            u64* iter = bitcast<u64*>(block->startOfAllocatableRegion());
            u64* end = bitcast<u64*>(block + block->header.size * Block::MediumToSmallRatio);
            while (iter != end)
                *iter ++ = 0xdeadbeefdeadbeef;
            process::unlock(&markDeadLock);
        #endif
    }

    NOINLINE void* allocateLarge(HeapHandle& handle, u64 bytes) {
        bytes += Block::largeHeaderSize();
        Block* block = getLargeBlock(handle, bytes);

        ALLOC_LOG(1, "Allocated large object of size ", block->header.largeSize(), " at ", hex(uptr(block->startOfAllocatableRegion())));

        #if ELUMALLOC_COLLECT_STATS
            AllocStats::largeAllocs ++;
            AllocStats::totalAllocatedBytes += block->header.largeSize();
        #endif

        return block->startOfAllocatableRegion();
    }

    void initializeThread() {
        new (&threadLocalHandle()) HeapHandle(commonHeap.handle());
    }

    void initialize() {
        // Initialize size class division magic.
        for (u32 i = 0; i < Block::NumSizeClasses; i ++)
            Block::SizeClassDivisionMagic[i] = multiplierAndShiftForDiv(Block::SizeClasses[i]);
    }

    void deinitializeThread() {
        //
    }

    void deinitialize() {
        // println("Total allocs: ", AllocStats::bumpAllocs + AllocStats::freeListAllocs + AllocStats::slowPathAllocs + AllocStats::largeAllocs);
        // println("Bump allocs: ", AllocStats::bumpAllocs);
        // println("Freelist allocs: ", AllocStats::freeListAllocs);
        // println("Slow path allocs: ", AllocStats::slowPathAllocs);
        // println("Large allocs: ", AllocStats::largeAllocs);
        // println("Total allocated bytes: ", AllocStats::totalAllocatedBytes);
    }

    extern "C" USED NOINLINE u64 sizeClassForOutOfLine(u32 size) {
        return Block::sizeClassFor(size);
    }

    extern "C" USED NOINLINE u32 sizeForClassOutOfLine(u32 size) {
        return Block::sizeForClass(size);
    }

    USED NOINLINE void* allocateOutOfLine(u64 size) {
        return allocate(size);
    }

    USED NOINLINE void freeOutOfLine(void* ptr) {
        free(ptr);
    }
}