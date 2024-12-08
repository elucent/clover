#ifndef UTILITY_ALLOC_H
#define UTILITY_ALLOC_H

#include "rt/def.h"
#include "rt/sys.h"
#include "util/io.h"
#include "util/math.h"

#define LogMalloc 0
#define VerifyBlockList 0

namespace allocator {
    /*
    * GC Types
    *
    * Memory in the GC is organized into the following units:
    *  - Bytes
    *  - Words: Granularity of allocation sizes in the heap. 16 bytes
    *  - Blocks: Contain many small allocations, or are part of a big allocation. 65KiB.
    *  - Regions: This is the granularity the entire heap is made out of, containing 256 blocks or 16MiB each.
    *  - Heaps: Made up of one or more regions, can grow or shrink as needed as the program runs.
    */

    struct word {
        u8 bytes[16];
    };

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

    constexpr i16 SizeClasses[] = {
        1, 2, 3, 4, 6, 8, 12, 16,
        24, 32, 48, 64, 96, 128, 192, 256,
        384, 512, 768, 1024 //, 1536, 2048
    };

    /*
    * These masks are used to efficiently scan the heap bitmap in units of each allocation size.
    */

    constexpr u64 SizeClassMasks[] = {
        0x3, 0xf, 0x3f, 0xff, 0x3ff, 0xfff, 0x3fff, 0xffff,
        0x3ffff, 0xfffff, 0x3fffff, 0xffffff, 0x3ffffff, 0xfffffff, 0x3fffffff, 0xffffffff,
        0x3ffffffff, 0xfffffffff, 0x3fffffffff, 0xffffffffff //, 0x3ffffffffff, 0xfffffffffff
    };

    constexpr u16 NumSizeClasses = 20;
    constexpr u64 MaxSizeClassBytes = SizeClasses[NumSizeClasses - 1] * sizeof(word);

    /*
    * If we know the size of an allocation at compile-time, for instance if we know the type to
    * be allocated, we can use this function to determine its size class ahead-of-time.
    */

    constexpr i16 sizeClassForConst(u32 bytes) {
        u32 words = divideRoundingUp<u32>(bytes, sizeof(word));
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
            // case 1025 ... 1536: return 20;
            // case 1537 ... 2048: return 21;
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

    inline i16 sizeClassFor(u32 bytes) {
        assert(bytes <= MaxSizeClassBytes);
        u32 words = divideRoundingUp<u32>(bytes, sizeof(word));
        i16 log2 = 32 - clz32(words - 1);
        if (words - 1 == 0) log2 = 1;
        u32 basewords = 1 << log2;
        u32 shift = basewords < 4 ? basewords >> 1 : basewords >> 2;
        i16 base = log2 * 2 - 1;
        if (words <= basewords - shift)
            base --;
        return base;
    }

    /*
    * Allocator State
    *
    * The state of the allocator consists of NumSizeClasses pointers to free lists. Each free list points
    * to a specific block, and so long as a block is referenced by an allocator, it is either:
    *  - A shared block, meaning it can be used by any thread, and is protected by locks.
    *  - A thread-local block, meaning it can only be used by a particular thread.
    * Whether a free list points to a shared or thread-local block is indicated by the bottom bit of the
    * pointer to the next element. If the free list is thread-local, the bottom (least-significant) bit is
    * zero; if it's shared, it must be one.
    */

    struct FreeList {
        uptr ptr;

        inline FreeList(): ptr(0) {}
        inline FreeList(void* p): ptr(uptr(p)) {}

        inline bool empty() const {
            return !(ptr & uptr(0xffff));
        }
    };

    struct Allocator {
        FreeList freeLists[NumSizeClasses];
        u8 failCounts[NumSizeClasses];
        u32 blocksPerList[NumSizeClasses];
        bool isShared;
    };

    constexpr u32 BytesPerBlock = 64 * memory::KiB;
    constexpr u32 WordsPerBlock = BytesPerBlock / sizeof(word);

    enum BlockState : u8 {
        EmptyBlock,
        InSharedAllocator,
        InThreadAllocator,
        ReservedShared,
        ReservedInThread,
        Retired,
        Huge
    };

    struct Block;

    struct BlockMeta {
        constexpr static u32 RetireThreshold = 2;

        FreeList freeList;
        u32 emptySlots;
        i16 sizeClass;
        BlockState state;
        i8 mutex;
        i32 owningThread;
        u32 hugeSize;
        Block* prevBlock;
        Block* nextBlock;
        u32 passUpCount; // Number of times this block has been skipped due to being too full.
        bool isLeaked;
        bool hadThreadLocalFree;
        uptr reserved[2];
        word words[0];
        i8 bytes[0];

        inline void lock() {
            process::lock(&mutex);
        }

        inline void unlock() {
            process::unlock(&mutex);
        }

        inline void makeEmpty() {
            sizeClass = 0;
            state = EmptyBlock;
            owningThread = -1;
            hugeSize = 0;
            mutex = 0;
            emptySlots = 0;
            prevBlock = nextBlock = nullptr;
            passUpCount = 0;
            isLeaked = false;
            freeList = FreeList();
        }

        inline void installFreeList(i16 size) {
            sizeClass = size;
            u16 wordsPerItem = SizeClasses[size];
            word* p = (word*)this + WordsPerBlock - wordsPerItem;
            FreeList list((Block*)this);
            while (p >= words) {
                *(FreeList*)p = list;
                list = FreeList(p);
                p -= wordsPerItem;
            }
            freeList = list;
        }

        inline void giveToThread(i32 thread) {
            owningThread = thread;
            state = InThreadAllocator;
        }

        inline void makeShared() {
            owningThread = -1;
            state = InSharedAllocator;
        }

        inline void makeHuge(u32 size) {
            owningThread = -1;
            sizeClass = -1;
            state = Huge;
            hugeSize = size;
        }
    };

    struct Block : public BlockMeta {
        word words[0];
        i8 bytes[BytesPerBlock - sizeof(BlockMeta)];
        static_assert(sizeof(bytes) % sizeof(word) == 0);
    };
    static_assert(sizeof(Block) % sizeof(memory::PAGESIZE) == 0);

    constexpr u32 MallocThreadLocalAllocatorOffset = 0;
    constexpr u32 WMallocThreadLocalAllocatorOffset = MallocThreadLocalAllocatorOffset + sizeof(Allocator);

    inline Allocator& threadLocalAllocator() {
        return *(Allocator*)process::tls();
    }

    struct Heap {
        constexpr static u64 BlocksPerRegion = 256; // Heaps are managed in chunks of 256 blocks.
        constexpr static u64 BytesPerRegion = BlocksPerRegion * sizeof(Block);
        constexpr static u64 MaxHeapSize = 64 * memory::GiB;
        constexpr static u64 ThreadLocalFailThreshold = 16;
        constexpr static u64 BlockListFailThreshold = 64;
        constexpr static u64 FirstPassEmptyThreshold = sizeof(Block) / 4;

        slice<u64> occupancy;
        slice<Block> blocks;
        Block* blockListHeads[NumSizeClasses];
        Block* blockListTails[NumSizeClasses];
        u32 toFreeCount;
        i8 storageMutex;
        i8 blockListMutex;
        u32 blockListFailCount;

        inline Heap() {}
        Heap(u32 regions);

        bool addRegions(u32 regions);
        void destroy();

        inline void lockStorage() {
            process::lock(&storageMutex);
        }

        inline void unlockStorage() {
            process::unlock(&storageMutex);
        }

        inline void lockBlockList() {
            process::lock(&blockListMutex);
        }

        inline void unlockBlockList() {
            process::unlock(&blockListMutex);
        }

        inline Block& blockFor(void* ptr) {
            uptr u = uptr(ptr);
            u = roundDownToNearest(u, sizeof(Block));
            return *(Block*)u;
        }

        inline Block* claimSingleFreeBlock() {
            lockStorage();
            for (u32 i = 0; i < occupancy.size(); i ++) {
                u64& word = occupancy[i];
                if (!word)
                    continue;
                u64 firstBit = ctz64(word);
                u64 blockIndex = i * 64 + firstBit;
                Block* block = blocks.data() + blockIndex;
                u64 mask = ~(u64(1) << firstBit);
                word &= mask;
                unlockStorage();
                return block;
            }
            return nullptr;
        }

        inline bool shouldReuseBlock(Block* block, u32 threshold) {
            return block->emptySlots * SizeClasses[block->sizeClass] * sizeof(word) >= threshold;
        }

        inline void printBlockList(Block* block) {
            if (!block)
                return;
            do {
                print(hex(block), "(", sizeof(Block) - block->emptySlots * SizeClasses[block->sizeClass] * sizeof(word), "/", sizeof(Block), ") -> ");
                block = block->nextBlock;
            } while(block);
        }

        inline void detachThreadLocal(Block*& threadLocalList, Block* block) {
            // Unstring the block from its neighbors.
            if (block->prevBlock)
                block->prevBlock->nextBlock = block->nextBlock;
            if (block->nextBlock)
                block->nextBlock->prevBlock = block->prevBlock;
            if (block == threadLocalList)
                threadLocalList = block->nextBlock;
            block->passUpCount = 0;
            threadLocalAllocator().blocksPerList[block->sizeClass] --;
        }

        inline void enqueueThreadLocal(Block*& threadLocalList, Block* block) {
            block->prevBlock = nullptr;
            block->nextBlock = threadLocalList;
            if (threadLocalList)
                threadLocalList->prevBlock = block;
            threadLocalList = block;
            threadLocalAllocator().blocksPerList[block->sizeClass] ++;
        }

        inline void detachGlobal(i16 sizeClass, Block* block) {
            // Unstring the block from its neighbors.
            if (block->prevBlock)
                block->prevBlock->nextBlock = block->nextBlock;
            if (block->nextBlock)
                block->nextBlock->prevBlock = block->prevBlock;
            Block*& head = blockListHeads[sizeClass];
            Block*& tail = blockListTails[sizeClass];
            if LIKELY(block == head) {
                head = block->nextBlock;
                if (!head) tail = nullptr;
            } else if (block == tail) {
                tail = block->prevBlock;
                if (!tail) head = nullptr;
            }
        }

        inline void enqueueGlobal(i16 sizeClass, Block* block) {
            Block*& head = blockListHeads[sizeClass];
            Block*& tail = blockListTails[sizeClass];
            if (shouldReuseBlock(block, FirstPassEmptyThreshold)) {
                block->nextBlock = head;
                block->prevBlock = nullptr;
                if (head)
                    head->prevBlock = block;
                else
                    tail = block;
                head = block;
            } else {
                block->prevBlock = tail;
                block->nextBlock = nullptr;
                if (tail)
                    tail->nextBlock = block;
                else
                    head = block;
                tail = block;
            }
        }

        inline void verifyBlockList(Block* block) {
            if (!block)
                return;
            Block* initialBlock = block;
            Block* prevBlock = nullptr;
            while (block) {
                if (block->prevBlock != prevBlock) {
                    println("Verification failed! Block ", hex(block), " does not point back to its predecessor ", hex(prevBlock), ", points to ", hex(block->prevBlock), " instead");
                    print("Erroring block list = ");
                    printBlockList(initialBlock);
                    println();
                    panic("");
                }
                if (prevBlock && prevBlock->nextBlock != block) {
                    println("Verification failed! Block ", hex(prevBlock), " does not point forward to its successor ", hex(block));
                    print("Erroring block list = ");
                    printBlockList(initialBlock);
                    println();
                    panic("");
                }
                prevBlock = block;
                block = block->nextBlock;
            }
        }

        inline void retire(Block*& threadLocalList, Block* block) {
            detachThreadLocal(threadLocalList, block);

            lockBlockList();
            enqueueGlobal(block->sizeClass, block);
            block->passUpCount = 0;
            block->owningThread = -1;
            block->state = Retired;
            if (LogMalloc) println("Thread ", process::current(), " retiring block ", hex(block));
            if (VerifyBlockList) verifyBlockList(blockListHeads[block->sizeClass]);
            unlockBlockList();
        }

        inline Block* tryReuseBlockThreadLocal(Block*& threadLocalList, u32 threshold) {
            while (threadLocalList && threadLocalList->prevBlock)
                threadLocalList = threadLocalList->prevBlock;
            // We don't need to claim any locks in this function because all the blocks involved are thread-local.
            Block* block = threadLocalList;
            while (block) {
                if (shouldReuseBlock(block, threshold))
                    break;
                Block* next = block->nextBlock;
                if UNLIKELY(++ block->passUpCount > BlockMeta::RetireThreshold)
                    retire(threadLocalList, block);
                block = next;
            }
            if (block)
                detachThreadLocal(threadLocalList, block);
            return block;
        }

        inline Block* tryReuseBlockRetired(i16 sizeClass, u32 threshold, u32 maxProbe) {
            // This function strongly assumes we already hold the block list lock.
            u32 i = 0;
            Block* block = blockListHeads[sizeClass];
            while (block) {
                if (i ++ >= maxProbe)
                    return nullptr;
                if (shouldReuseBlock(block, threshold))
                    break;
                block = block->nextBlock;
            }
            if (block)
                detachGlobal(sizeClass, block);
            return block;
        }

        inline NOINLINE void fixupBlockList(i16 sizeClass, u32 threshold) {
            Block*& head = blockListHeads[sizeClass];
            Block* block = head;
            while (block) {
                Block* next = block->nextBlock;
                if (shouldReuseBlock(block, threshold)) {
                    detachGlobal(sizeClass, block);
                    enqueueGlobal(sizeClass, block);
                }
                block = next;
            }
            blockListFailCount = 0;
        }

        inline NOINLINE FreeList processThreadLocalCache(Block* block) {
            Block* first = block;
            while (first && first->prevBlock)
                first = first->prevBlock;
            FreeList result = FreeList(first);
            block = first;
            while (block) {
                Block* next = block->nextBlock;
                if (block->emptySlots) {
                    block->lock();
                    FreeList blockList = block->freeList;
                    FreeList iter1 = blockList, iter2 = blockList;
                    while (!iter1.empty()) {
                        iter2 = iter1;
                        iter1 = *(FreeList*)iter1.ptr;
                    }
                    *(FreeList*)iter2.ptr = result; // Stitch freelists together.
                    block->freeList = FreeList(block);
                    block->emptySlots = 0;
                    block->unlock();
                } else if (!block->hadThreadLocalFree && block->passUpCount ++ > Block::RetireThreshold)
                    retire(first, block);
                block->hadThreadLocalFree = false;
                block = next;
            }
            return result;
        }

        inline NOINLINE FreeList claimFreeList(Allocator& allocator, i16 sizeClass) {
            Block* threadLocalList = (Block*)allocator.freeLists[sizeClass].ptr;
            FreeList list;
            if UNLIKELY(allocator.failCounts[sizeClass] ++ > ThreadLocalFailThreshold) {
                allocator.failCounts[sizeClass] = 0;
                list = processThreadLocalCache(threadLocalList);
                if (!list.empty())
                    return list;
            }
            Block* newBlock = tryReuseBlockThreadLocal(threadLocalList, FirstPassEmptyThreshold);
            if (newBlock) {
                enqueueThreadLocal(threadLocalList, newBlock);
                if (LogMalloc) println("Thread ", process::current(), " reusing block from thread-local cache ", hex(newBlock));
                if (VerifyBlockList) verifyBlockList(threadLocalList);
            } else if (blockListHeads[sizeClass]) {
                lockBlockList();
                if (LogMalloc) print("Thread ", process::current(), " trying to reuse block from block list ", sizeClass, " "), printBlockList(blockListHeads[sizeClass]), println();
                if (blockListFailCount ++ > BlockListFailThreshold)
                    fixupBlockList(sizeClass, FirstPassEmptyThreshold);
                newBlock = tryReuseBlockRetired(sizeClass, FirstPassEmptyThreshold, 8);
                if (!newBlock)
                    blockListFailCount ++;
                unlockBlockList();
                if (newBlock) {
                    if (LogMalloc) println("Thread ", process::current(), " reusing block from global block list ", sizeClass, ": ", hex(newBlock));
                    newBlock->giveToThread(process::current());
                    enqueueThreadLocal(threadLocalList, newBlock);
                    if (LogMalloc) print("Block list ", sizeClass, " after is "), printBlockList(blockListHeads[sizeClass]), println();
                    if (LogMalloc) print("Thread ", process::current(), " local list ", sizeClass, " after is "), printBlockList(newBlock), println();
                    if (VerifyBlockList) verifyBlockList(blockListHeads[sizeClass]);
                    if (VerifyBlockList) verifyBlockList(threadLocalList);
                }
            }
            if (!newBlock) {
                newBlock = claimSingleFreeBlock();
                if (newBlock) {
                    if (LogMalloc) println("Thread ", process::current(), " allocated new block ", hex(newBlock));
                    newBlock->giveToThread(process::current());
                    newBlock->installFreeList(sizeClass);
                    enqueueThreadLocal(threadLocalList, newBlock);
                    if (LogMalloc) print("Current thread-local list: "), printBlockList(newBlock), println();
                    if (VerifyBlockList) verifyBlockList(threadLocalList);
                }
            }
            if (!newBlock) {
                lockBlockList();
                newBlock = tryReuseBlockRetired(sizeClass, 1, 65536); // Desperation before we OOM.
                unlockBlockList();
                if (newBlock) {
                    if (LogMalloc) println("Thread ", process::current(), " desperately reusing block from global block list ", sizeClass, ": ", hex(newBlock));
                    newBlock->giveToThread(process::current());
                    enqueueThreadLocal(threadLocalList, newBlock);
                    if (LogMalloc) print("Block list ", sizeClass, " after is "), printBlockList(blockListHeads[sizeClass]), println();
                    if (LogMalloc) print("Thread ", process::current(), " local list ", sizeClass, " after is "), printBlockList(newBlock), println();
                    if (VerifyBlockList) verifyBlockList(blockListHeads[sizeClass]);
                    if (VerifyBlockList) verifyBlockList(threadLocalList);
                }
            }
            if (!newBlock) {
                println("Ran out of memory!");
                printHeapBreakdown();
                panic("");
            }

            assert(newBlock == threadLocalList);
            newBlock->lock();
            list = newBlock->freeList;
            newBlock->freeList = FreeList(newBlock);
            newBlock->emptySlots = 0;
            newBlock->unlock();
            return list;
        }

        inline void printHeapBreakdown() {
            u64 retiredCount = 0;
            u64 leakedRetiredCount = 0;
            u64 otherCount = 0;
            u64 emptyCount = 0;
            u64 hugeCount = 0;
            u64 threadCounts[64];
            memory::fill(threadCounts, 0, sizeof(threadCounts));
            for (u32 i = 0; i < blocks.size(); i ++) switch (blocks[i].state) {
                case InThreadAllocator:
                    if (LogMalloc) println("Found block ", hex(&blocks[i]), " belonging to thread ", blocks[i].owningThread);
                    threadCounts[blocks[i].owningThread] ++;
                    break;
                case Huge:
                    hugeCount += blocks[i].hugeSize;
                    i += blocks[i].hugeSize;
                    break;
                case EmptyBlock:
                    emptyCount ++;
                    break;
                case Retired:
                    leakedRetiredCount ++;
                    blocks[i].isLeaked = true;
                    break;
                default:
                    otherCount ++;
                    break;
            }

            lockBlockList();
            for (u32 i = 0; i < NumSizeClasses; i ++) {
                Block* block = blockListHeads[i];
                while (block) {
                    if (block->state != Retired)
                        println("Block list ", i, " contains block ", hex(block), " with non-retired state ", block->state);
                    retiredCount ++;
                    block->isLeaked = false;
                    block = block->nextBlock;
                }
            }
            unlockBlockList();
            println("Heap breakdown (", blocks.size(), " total blocks):");
            println(" - ", emptyCount, " empty blocks");
            println(" - ", retiredCount, " retired blocks");
            println(" - ", i64(leakedRetiredCount) - i64(retiredCount), " leaked retired blocks");
            println(" - ", hugeCount, " huge blocks");
            println(" - ", otherCount, " other");
            for (u32 i = 0; i < 64; i ++) if (threadCounts[i])
                println(" - ", threadCounts[i], " in thread ", i + 1);
            // println("Retired block lists:");
            // for (u32 i = 0; i < NumSizeClasses; i ++) {
            //     print(" - ");
            //     printBlockList(blockLists[i]);
            //     println();
            // }
            // print("Leaked blocks:");
            // for (const Block& block : blocks) if (block.meta.state == Retired && block.meta.isLeaked)
            //     print(" ", hex(&block));
            // println();
        }
    };
}

extern allocator::Heap TheHeap;

extern allocator::Allocator sharedAllocator;
extern i8 sharedMutexes[allocator::NumSizeClasses];

inline void gc_init() {
    new (&TheHeap) allocator::Heap(256); // Default to 4GB to start.
    new (&sharedAllocator) allocator::Allocator();
    sharedAllocator.isShared = true;
}

inline void gc_deinit() {
    TheHeap.destroy();
}

inline void gc_thread_init() {
    using namespace allocator;

    Allocator& allocator = threadLocalAllocator();
    for (u32 i = 0; i < NumSizeClasses; i ++) {
        allocator.freeLists[i] = FreeList();
        allocator.failCounts[i] = 0;
        allocator.blocksPerList[i] = 0;
    }
}

inline void gc_thread_deinit() {
    using namespace allocator;

    Allocator& allocator = threadLocalAllocator();
    if (LogMalloc) println("Deiniting thread ", process::current());
    Block* freeableBlocks[NumSizeClasses];
    memory::fill(freeableBlocks, 0, sizeof(freeableBlocks));
    for (u32 i = 0; i < NumSizeClasses; i ++) {
        FreeList list = allocator.freeLists[i];
        while (list.ptr) {
            Block* block = &TheHeap.blockFor((void*)list.ptr);
            if UNLIKELY(block->state == InThreadAllocator) {
                if (LogMalloc) println("Deiniting thread ", process::current(), " marks block ", hex(block), " as retired");
                block->state = Retired;
                freeableBlocks[i] = block;
            }
            if (!list.empty()) {
                FreeList nextList = *(FreeList*)list.ptr;
                block->lock();
                *(FreeList*)list.ptr = block->freeList;
                block->freeList = list;
                block->emptySlots ++;
                block->unlock();
                list = nextList;
            } else
                list = FreeList();
        }
    }
    for (u32 i = 0; i < NumSizeClasses; i ++) {
        if (!freeableBlocks[i])
            continue;
        Block* block = freeableBlocks[i];
        while (block->prevBlock) block = block->prevBlock;
        TheHeap.lockBlockList();
        while (block) {
            Block* nextBlock = block->nextBlock;
            TheHeap.enqueueGlobal(block->sizeClass, block);
            if (LogMalloc) println("Deiniting thread ", process::current(), " retires block ", hex(block));
            if (LogMalloc) print("Current block list ", i, ": "), TheHeap.printBlockList(TheHeap.blockListHeads[i]), println();
            if (VerifyBlockList) TheHeap.verifyBlockList(TheHeap.blockListHeads[i]);
            block = nextBlock;
        }
        TheHeap.unlockBlockList();
    }
}

inline NOINLINE void* mallocHuge(uword bytes) {
    using namespace allocator;

    TheHeap.lockStorage();
    uword sizeInBlocks = divideRoundingUp(bytes + sizeof(BlockMeta), sizeof(Block));
    Block* result = nullptr;
    Block* allBlocks = TheHeap.blocks.data();
    auto occupancy = TheHeap.occupancy;
    for (u32 i = 0; i < occupancy.size() && !result; i ++) {
        u64 word = occupancy[i];
        if (!word)
            continue;
        u64 bits = sizeInBlocks;
        u64 mask = bits >= 64 ? 0xffffffffffffffffull : (u64(1) << bits) - 1;
        while (word) {
            u64 index = ctz64(word);
            u64 shiftedMask = mask << index;
            u64 isect = word & shiftedMask;
            if (popcount64(isect) < popcount64(shiftedMask)) {
                // Mask didn't match, so let's try the same word again.
                word &= ~isect;
                continue;
            } else if (popcount64(shiftedMask) == sizeInBlocks) { // Whole mask fits - so allocation succeeds!
                result = allBlocks + i * 64 + index;
                break;
            } else { // We need to look into the next few blocks.
                bits -= popcount64(shiftedMask);
                u64 j = 1;
                while (bits) {
                    u64 nextWord = occupancy[i + j];
                    u64 mask = bits >= 64 ? 0xffffffffffffffffull : (u64(1) << bits) - 1;
                    if ((nextWord & mask) != mask)
                        goto mallocHuge_failMultiWord;
                    bits -= popcount64(mask);
                    j ++;
                }
                result = allBlocks + i * 64 + index;
                break;
            }
        }
mallocHuge_failMultiWord:;
    }
    result->makeHuge(sizeInBlocks);
    u64 index = result - TheHeap.blocks.data();
    u64 base = index / 64, offset = index % 64;
    while (sizeInBlocks) {
        u64& word = occupancy[base];
        u64 mask = sizeInBlocks >= 64 ? 0xffffffffffffffffull : (u64(1) << sizeInBlocks) - 1;
        u64 shiftedMask = mask << offset;
        word &= ~shiftedMask;
        sizeInBlocks -= popcount64(shiftedMask);
        offset = 0;
        base ++;
    }
    TheHeap.unlockStorage();
    if (LogMalloc) println("Malloced ", bytes, " bytes at ", hex(result));
    return (i8*)result + sizeof(BlockMeta);
}

#define SLOW_GUARD_MALLOC 0
#define USE_ELUMALLOC 1

#if SLOW_GUARD_MALLOC

// Optional malloc hardening mode: every allocation is independently mapped,
// with guard pages. Allocations are randomly placed against the right or left
// edges of the allocated page, so we can try and catch out-of-bounds reads or
// writes in either direction.

namespace MallocLCG {
    constexpr uword
        m = 0x100000000u,
        a = 16854749770870987091ull,
        c = 15284698627294678789ull;
    extern uword x;

    inline uword next() {
        return x = (a * x + c) % m;
    }
};

inline ALWAYSINLINE void* malloc(uword bytes) {
    uword pages = divideRoundingUp<uword>(bytes + 16, memory::PAGESIZE) + 2;
    auto alloc = memory::map(pages);
    if (!alloc.data())
        return nullptr;

    // Guard pages.
    memory::tag(alloc.take(1), 0);
    memory::tag({ alloc.data() + 1, (iword)pages }, memory::READ | memory::WRITE);
    memory::tag(alloc.drop(alloc.size() - 1), 0);

    u8* base = (u8*)&alloc[1];
    if (MallocLCG::next() % 2) {
        base = (u8*)(alloc.end() - 1) - (bytes + 16);
        base = (u8*)(uptr(base) & ~15ull); // Align to 16 bytes.
    }
    *(void**)base = alloc.data();
    *((iword*)base + 1) = alloc.size();
    return base + 16;
}

inline ALWAYSINLINE void free(void* ptr) {
    memory::page* alloc_base = *((memory::page**)ptr - 2);
    iword size = *((iword*)ptr - 1);
    slice<memory::page> alloc = { alloc_base, size };
    memory::unmap(alloc);
}
#elif USE_ELUMALLOC
#include "util/elumalloc.h"
inline ALWAYSINLINE void* malloc(uword bytes) {
    return elumalloc::allocate(bytes);
}

inline ALWAYSINLINE void free(void* ptr) {
    return elumalloc::free(ptr);
}
#else
inline ALWAYSINLINE void* malloc(uword bytes) {
    using namespace allocator;

    if UNLIKELY(!bytes)
        return nullptr;
    if UNLIKELY(bytes > MaxSizeClassBytes)
        return mallocHuge(bytes);
    i16 sizeClass = sizeClassFor(bytes);
    auto& allocator = threadLocalAllocator();
    FreeList& freeList = allocator.freeLists[sizeClass];
    FreeList* result;
    if UNLIKELY(freeList.empty())
        freeList = TheHeap.claimFreeList(allocator, sizeClass);
    result = (FreeList*)freeList.ptr;
    freeList = *result;
    if (LogMalloc) println("Malloced ", bytes, " bytes at ", hex(result));
    return result;
}

inline NOINLINE void freeHuge(allocator::Block& block) {
    using namespace allocator;

    TheHeap.lockStorage();
    u32 size = block.hugeSize;
    Block* blocks = &block;
    u32 index = &block - TheHeap.blocks.data();
    for (u32 i = 0; i < size; i ++) {
        u64& occupancyWord = TheHeap.occupancy[(i + index) / 64];
        occupancyWord |= u64(1) << (i + index) % 64;
        blocks[i].makeEmpty();
    }
    TheHeap.unlockStorage();
}

inline ALWAYSINLINE void free(void* ptr) {
    using namespace allocator;

    if UNLIKELY(!ptr)
        return;
    Block& block = TheHeap.blockFor(ptr);
    if (LogMalloc) println("Freeing ", hex(ptr), " in block ", hex(&block));
    if UNLIKELY(block.state == Huge)
        return freeHuge(block);
    if LIKELY(block.owningThread == process::current()) {
        FreeList& currentFreeList = threadLocalAllocator().freeLists[block.sizeClass];
        *(FreeList*)ptr = currentFreeList;
        currentFreeList = FreeList(ptr);
        block.hadThreadLocalFree = true;
    } else {
        block.lock();
        *(FreeList*)ptr = block.freeList;
        block.freeList = FreeList(ptr);
        block.emptySlots ++;
        block.unlock();
    }
}
#endif

extern "C" NOINLINE void* mallocOutOfLine(u64 bytes);
extern "C" NOINLINE void freeOutOfLine(void* ptr);

void* operator new(size_t bytes);
void operator delete(void* ptr) noexcept;
void operator delete(void* ptr, size_t) noexcept;
void* operator new[](size_t bytes);
void operator delete[](void* ptr) noexcept;
void operator delete[](void* ptr, size_t) noexcept;

#endif