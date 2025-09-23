#ifndef UTIL_ELUMALLOC_H
#define UTIL_ELUMALLOC_H

#include "rt/def.h"
#include "rt/sys.h"
#include "util/io.h"
#include "util/math.h"

// Enables verbose logging basically everywhere in the malloc.
#define ELUMALLOC_VERBOSE 0

// Collect allocation counts and allocated byte total and report it on exit.
#define ELUMALLOC_COLLECT_STATS 0

// Use power-of-two based size classes. These are a little faster to classify
// at the cost of having quite a bit more potential wasted memory.
#define ELUMALLOC_USE_POWER_OF_TWO_SIZE_CLASSES 0

// Use a lookup table to find the rounded number of bytes for a size class
// instead of computing it arithmetically.
#define ELUMALLOC_SIZE_FOR_CLASS_USES_MEMORY 1

// Write 0xdeadbeefdeadbeef over dead memory and assert that we only install
// free lists or allocate over memory that is actually dead.
#define ELUMALLOC_MARK_DEAD_ON_FREE 0

// Validate at allocation time that allocations have the same size class as
// their containing block and don't exceed its bounds.
#define ELUMALLOC_VALIDATE_ALLOCATION_SIZE 0

// Validate for each block list in the heap that the blocks' size classes are
// the size class of the list.
#define ELUMALLOC_VERIFY_BLOCK_LISTS 0

// Validate that the free list doesn't extend beyond the containing block.
#define ELUMALLOC_VALIDATE_FREE_LIST 0

extern i8 allocLogLock;

#define ALLOC_LOG(verbosity, ...) do { \
        if (ELUMALLOC_VERBOSE >= verbosity) { \
            process::lock(&allocLogLock); \
            println("[ALLOC]\t(#", 42, ")\t", __VA_ARGS__); \
            process::unlock(&allocLogLock); \
        } \
    } while (false)

/*
 * Primitives
 * ----------
 * Common types shared across all heap types and allocators.
 */

struct FreeList;

// Atoms are the smallest granule of allocations. They are ordinary data,
// 16 bytes in size, with no other special requirements other than they
// be big enough to encode a free list.

struct Atom {
    u8 bytes[16];

    inline FreeList& asFreelist();
};

// A FreeList is an intrusive list threaded through memory representing
// a queue of free intervals that can be allocated out of.

struct FreeList {
    i64 size;
    i64 offsetToNext;
};

inline FreeList& Atom::asFreelist() {
    return *bitcast<FreeList*>(this);
}

// Constants for different memory sizes.

constexpr u64 KB = 1024;
constexpr u64 MB = 1024 * KB;
constexpr u64 GB = 1024 * MB;

/*
 * Primitive Heap
 * --------------
 * Heap type backed by primitive virtual memory allocations that can store
 * elements of a single homogeneous size class. Used to allocate other
 * allocator data structures.
 *
 * Structurally, a Primitive Heap is a circular linked list of Blocks, each
 * of the same size. Each Block maintains a bitmap of free cells, and
 * allocation takes the form of finding the first free bit in the bitmap. We
 * store the currently scanned word + index in the heap itself to avoid needing
 * to fetch the word from the Block repeatedly. In an effort to keep this heap
 * relatively simple, we take a lock around every malloc and free.
 */

slice<i8> mapAligned(u64 size, u64 align);

template<typename T>
struct PrimitiveHeap {
    constexpr static u64 Size = sizeof(T);
    constexpr static u64 BlockSize = 64 * KB;
    constexpr static u64 AllocationSize = (Size + sizeof(Atom) - 1) / sizeof(Atom) * sizeof(Atom);

    constexpr static u64 BitmapBytes = 512; // One per atom in the block.
    constexpr static u64 BitmapWords = BitmapBytes / 8;
    constexpr static u64 BitmapWordsInUse = (BlockSize / AllocationSize) / 64;
    constexpr static u64 HeaderSize = sizeof(void*) * 2 + BitmapBytes;
    constexpr static u64 UsableSize = BlockSize - HeaderSize;
    static_assert(HeaderSize + AllocationSize <= BlockSize);

    struct alignas(sizeof(Atom)) Block {
        Block* next;
        void* pad;
        u64 bitmap[BitmapWords];

        inline void unmap() {
            memory::unmap({
                bitcast<i8*>(this),
                BlockSize
            });
        }
    };

    Block* head = nullptr;
    i8 lock = 0;
    u32 index = 0;
    u64 word = 0;

    inline void fillBitmap(u64* bitmap) {
        constexpr u64 AllocationsInHeader = (HeaderSize + AllocationSize - 1) / AllocationSize;
        constexpr u64 AllocationsInBlock = BlockSize / AllocationSize;
        constexpr u64 UsableAllocations = AllocationsInBlock - AllocationsInHeader;
        constexpr u64 AllOnes = 0xffffffffffffffffull;

        u64 i = 0;
        u64 remainingAllocations = UsableAllocations;

        if constexpr (AllocationsInBlock <= 64) {
            u64 word = AllOnes;

            // Zero off bottom bits for the bitmap.
            word <<= AllocationsInHeader;

            // Zero off any unused bits on top.
            static_assert(UsableAllocations <= 64);
            constexpr u64 UnusedTopBits = 64 - UsableAllocations;
            word <<= UnusedTopBits;
            word >>= UnusedTopBits;

            bitmap[i ++] = word;
        } else {
            // First word - zero off any unnecessary bits due to the header.
            u64 word = AllOnes;
            word <<= AllocationsInHeader;
            bitmap[i ++] = word;
            remainingAllocations -= (64 - AllocationsInHeader);

            // Loop until we would have one word remaining.
            for (; remainingAllocations > 64;) {
                bitmap[i ++] = AllOnes;
                remainingAllocations -= 64;
            }

            // Final word - zero off top bits that would overflow the block.
            u64 unusedTopBits = 64 - remainingAllocations;
            word = AllOnes;
            word <<= unusedTopBits;
            word >>= unusedTopBits;
            bitmap[i ++] = word;
        }

        for (; i < BitmapWords; i ++)
            bitmap[i] = 0;
    }

    inline void addBlock() {
        auto pages = mapAligned(BlockSize, BlockSize);
        if (!head) {
            head = bitcast<Block*>(pages.data());
            head->next = head;
        } else {
            auto prevHead = head;
            head = bitcast<Block*>(pages.data());
            head->next = prevHead;
        }
        fillBitmap(head->bitmap);
    }

    inline void refill() {
        if UNLIKELY(!head)
            addBlock();
        Block* ptr = head;
        while (true) {
            while (index < BitmapWordsInUse) {
                word = ptr->bitmap[index];
                if (word) {
                    head = ptr;
                    return;
                }
                index ++;
            }
            index = 0;
            Block* next = ptr->next;
            if (next == head)
                addBlock(), ptr->next = next = head;
            ptr = next;
        }
    }

    inline T* malloc() {
        process::lock(&lock);
        if (!word)
            refill();
        u64 i = ctz64(word);
        u64 indexInBlock = index * 64 + i;
        u64 mask = ~(1ull << i);
        word &= mask;
        head->bitmap[index] &= mask;
        assert(word == head->bitmap[index]);
        process::unlock(&lock);

        u64 offsetInBlock = indexInBlock * AllocationSize;
        ALLOC_LOG(3, "Allocated primitive value of ", sizeof(T), " bytes at ", hex(uptr(head) + offsetInBlock), " from heap ", hex(uptr(this)));
        ALLOC_LOG(3, "    Current alloc word: ", binary(word | (1ull << i), 64), " -> ", binary(word, 64));
        return bitcast<T*>(bitcast<u8*>(head) + offsetInBlock);
    }

    inline void free(void* t) {
        uptr address = bitcast<uptr>(t);
        u64 offsetInBlock = address & (BlockSize - 1ull);
        auto block = bitcast<Block*>(address - offsetInBlock);
        u64 indexInBlock = offsetInBlock / AllocationSize;
        u64 wordIndex = indexInBlock / 64;
        u64 mask = 1ull << (indexInBlock % 64);

        process::lock(&lock);
        block->bitmap[wordIndex] |= mask;
        if (wordIndex == index && block == head)
            word |= mask;
        assert(word == head->bitmap[index]);

        ALLOC_LOG(3, "Freed primitive value at ", hex(uptr(t)), " in heap ", hex(uptr(this)));
        ALLOC_LOG(3, "    Underlying bitmap word ", wordIndex, ": ", binary(block->bitmap[wordIndex] & ~mask, 64), " -> ", binary(block->bitmap[wordIndex], 64));
        ALLOC_LOG(3, "    Current alloc word: ", binary(word & ~mask, 64), " -> ", binary(word, 64));
        process::unlock(&lock);
    }

    inline void destroy() {
        Block* ptr = head;
        do {
            Block* cur = ptr;
            ptr = ptr->next;
            cur->unmap();
        } while (ptr != head);
    }
};

/*
 * Block Management
 * ----------------
 * In general, a heap is made up of Blocks. These blocks are one of three
 * kinds:
 *
 *  - Small Blocks are 64 KB each, and store small allocations of a specific
 *    size class.
 *
 *  - Medium Blocks are 1 MB each, and store medium allocations of one or
 *    more cells of a certain size class.
 *
 *  - Large Blocks can be any size over the minimum large allocation threshold.
 *    They store allocations that are too large for the other Block kinds.
 *
 * For small and medium Blocks, we store the available Blocks per size class in
 * a max-heap datastructure, ordered by the free slots in the Block. This
 * allows us to prioritize Blocks with a large number of open slots, and
 * heuristically claim Blocks if no other Block has a certain free percentage.
 *
 * For large Blocks, since the whole Block is either a single allocation or
 * free, we only store freed Blocks. We use a balanced binary tree for this
 * purpose, ordered by overall Block size.
 */

// A Block is a large block of memory prefixed by a Header, used to store:
//  - Some number of small or medium-size allocations.
//  - A single large allocation.
// Small blocks all have the same size, as do medium blocks. Large allocations
// can be any size larger than the large allocation threshold.

struct Block {
    constexpr static u64 BitmapBytes = 512;
    constexpr static u64 BitmapWords = BitmapBytes / 8;
    constexpr static u64 SmallBlockSize = 64 * KB;
    constexpr static u64 MediumBlockSize = 1 * MB;
    constexpr static u64 MediumToSmallRatio = MediumBlockSize / SmallBlockSize;

    using SizeClass = u8;

    #if ELUMALLOC_USE_POWER_OF_TWO_SIZE_CLASSES
        // If power-of-two size classes are enabled, medium and small
        // allocations are rounded up to the nearest power of two. This
        // dramatically increases possible memory waste, since an allocation
        // can potentially need nearly 100% padding overhead (less one byte) to
        // hit the next size class. But it does make computing the size class,
        // and the rounded size of an allocation, much faster.

        constexpr static u64 NumSizeClasses = 16;
        constexpr static u32 SizeClasses[NumSizeClasses] = {
            16, 32, 64, 128, 256, 512, 1024, 2048,
            4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288
        };
        constexpr static u64 FirstMediumSizeClass = 10;
        constexpr static u64 MaxSizeForMedium = SizeClasses[NumSizeClasses - 1];
    #else
        // If power-of-two size classes are disabled, medium and small
        // allocations obey a more complex classification system. The first
        // eight size classes are just one to eight atoms in size. Beyond that,
        // for every four size classes, we go up by one power of two, up to a
        // maximum size of 524288 bytes. Between each of these powers are
        // "quartiles" - we advance by quarters of the prior power of two. So
        // between the 128-byte size class, and the 256-byte size class, we
        // define size classes for every 128 / 4 = 32 bytes. So 160, 192, and
        // 224 bytes. In this way, we keep up some exponential scaling, but
        // upper bound the maximum wasted memory by about 25%.

        constexpr static u64 NumSizeClasses = 56;
        constexpr static u32 SizeClasses[NumSizeClasses] = {
            16, 32, 48, 64, 80, 96, 112, 128,
            160, 192, 224, 256, 320, 384, 448, 512,
            640, 768, 896, 1024, 1280, 1536, 1792, 2048,
            2560, 3072, 3584, 4096, 5120, 6144, 7168, 8192,
            10240, 12288, 14336, 16384, 20480, 24576, 28672, 32768,
            40960, 49152, 57344, 65536, 81920, 98304, 114688, 131072,
            163840, 196608, 229376, 262144, 327680, 393216, 458752, 524288
        };
        constexpr static u64 FirstMediumSizeClass = 32;
        constexpr static u64 MaxSizeForMedium = SizeClasses[NumSizeClasses - 1];
    #endif

    constexpr static SizeClass LargeSizeClass = 255;

    static MultiplierAndShift SizeClassDivisionMagic[NumSizeClasses];

    inline static u32 sizeForClass(u32 sizeClass) {
        #if ELUMALLOC_USE_POWER_OF_TWO_SIZE_CLASSES
            return 1u << (sizeClass + 4u);
        #elif ELUMALLOC_SIZE_FOR_CLASS_USES_MEMORY
            return SizeClasses[sizeClass];
        #else
            assert(sizeClass < NumSizeClasses);

            // Bias to make the math work a bit better.
            sizeClass ++;
            if (sizeClass <= 8)
                return sizeClass * 16;

            // Extract the power, increase it by 3, then compute two to that power.
            // This gives the size of the quartile within a set of 4 classes.
            u32 quartile = (1u << (sizeClass >> 2)) + 3;

            // Get the bottom 3 bits to see which quartile of the power of two we
            // are targeting.
            u32 subShift = sizeClass & 3 + 4;

            // Compute the power of two we're going to use for the quartiles.
            return subShift * quartile;
        #endif
    }

    inline static u64 sizeClassFor(u32 size) {
        #if ELUMALLOC_USE_POWER_OF_TWO_SIZE_CLASSES
            return 32u - clz32((size - 1) >> 4);
        #else
            // First, we compute the size in atoms, with a quirk: every power of
            // two size is actually rounded down. This reflects the fact that
            // size classes are top-inclusive - the size class for 64 bytes can
            // represent any object up to *and including* 64 bytes.
            size = (size - 1) >> 4;

            // If our size is 8 atoms or less, we can return it outright - the
            // first 8 classes are each separated by an atom.
            if (size < 8)
                return size;

            // Compute the power factor. Starting from 128, we have four size
            // classes per power of two. First we extract the power, as a base
            // factor, then we extract the next two most significant bits after
            // the top to select within those four size classes. So:

            // 1. First, we get the log2 of the size minus 3.
            u64 power = 29 - clz32(size);

            // 2. Next, we shift down by that amount, and extract those two bits.
            u64 nextBits = (size >> power) & 3;

            // 3. Finally, we combine the next bits plus the power * 4, plus a
            //    bias.
            u64 result = nextBits + power * 4 + 4;
            return result;
        #endif
    }

    struct Header {
        SizeClass sizeClass;
        i8 shiftForDiv;
        u16 freeSlots;
        union {
            struct { u16 allocationsPerBlock; u16 sizeClassSize; };
            u32 size;
        };
        i32 multiplierForDiv;
        u32 pad;
        u64 bitmap[BitmapWords];

        inline bool isSmall() const {
            return sizeClass < FirstMediumSizeClass;
        }

        inline bool isMedium() const {
            return sizeClass >= FirstMediumSizeClass && sizeClass < LargeSizeClass;
        }

        inline bool isLarge() const {
            return sizeClass == LargeSizeClass;
        }

        inline u64 largeSize() const {
            return size * MediumBlockSize;
        }

        inline u64 allocationSize() const {
            return u64(sizeClassSize) * 8 + 8;
        }
    };

    Header header;
    u8 bytes[0];

    constexpr static u64 UsableSmallBlockSize = SmallBlockSize - sizeof(Header);
    constexpr static u64 UsableMediumBlockSize = MediumBlockSize - sizeof(Header);

    inline void initializeWithSize(SizeClass sizeClass, u64 roundedSize) {
        header.sizeClass = sizeClass;
        header.freeSlots = 0;
        u64 allocationSize = roundedSize;
        header.sizeClassSize = allocationSize / 8 - 1;

        MultiplierAndShift magic = SizeClassDivisionMagic[sizeClass];
        header.multiplierForDiv = magic.multiplier;
        header.shiftForDiv = magic.shift;

        header.allocationsPerBlock = (sizeClass < FirstMediumSizeClass ? UsableSmallBlockSize : UsableMediumBlockSize) / allocationSize;

        u32 numWords = (header.allocationsPerBlock + 63) / 64 + 1;
        if (numWords > BitmapWords)
            numWords = BitmapWords;
        ALLOC_LOG(3, "Clearing ", numWords, " words of the bitmap of block ", hex(uptr(this)), " with size ", roundedSize, " and ", header.allocationsPerBlock, " allocations per block");
        for (u32 i = 0; i < numWords; i ++)
            header.bitmap[i] = 0;
    }

    inline void initializeLarge(u64 size) {
        u64 roundedSize = (size + MediumBlockSize - 1) / MediumBlockSize;
        assert(roundedSize < 1ull << 32);
        header.sizeClass = LargeSizeClass;
        header.size = roundedSize;
    }

    inline static u64 largeHeaderSize() {
        return offsetof(Header, bitmap);
    }

    inline void* startOfAllocatableRegion() {
        if UNLIKELY(header.isLarge())
            return &header.bitmap[0];
        return &bytes;
    }

    NOINLINE FreeList* takeList();
};

using SizeClass = Block::SizeClass;

extern "C" USED NOINLINE u64 sizeClassForOutOfLine(u32 size);
extern "C" USED NOINLINE u32 sizeForClassOutOfLine(u32 sizeClass);

// A List is a singly-linked list of nodes.

template<typename Item>
struct List {
    struct Node {
        Item* item;
        Node* next;

        inline void* operator new(size_t n) {
            return NodeHeap.malloc();
        }

        inline void operator delete(void* p) {
            NodeHeap.free(p);
        }
    };
    static PrimitiveHeap<Node> NodeHeap;

    Node* head;

    inline List():
        head(nullptr) {}

    inline bool empty() const {
        return !head;
    }

    inline Item* pop() {
        if (!head)
            return nullptr;
        auto next = head->next;
        if (next == head) {
            auto item = head->item;
            delete head;
            head = nullptr;
            return item;
        }
        head->next = next->next;
        auto item = next->item;
        delete next;
        return item;
    }

    inline void push(Item* item) {
        assert(item);
        if (!head) {
            head = new Node {
                item,
                nullptr
            };
            head->next = head;
        } else {
            head->next = new Node {
                item,
                head->next
            };
            head = head->next;
        }
    }

    inline void printContents() {
        auto iter = head;
        if (!iter) {
            println("{}");
            return;
        }
        do {
            print(hex(uptr(iter->item)), " -> ");
            iter = iter->next;
        } while (iter != head);
        println();
    }
};

template<typename Item>
PrimitiveHeap<typename List<Item>::Node> List<Item>::NodeHeap;

template<typename Item>
struct SyncList : public List<Item> {
    i8 lock;

    inline SyncList():
        List<Item>(), lock(0) {}

    inline Item* pop() {
        process::lock(&lock);
        auto item = List<Item>::pop();
        process::unlock(&lock);
        return item;
    }

    inline void push(Item* item) {
        process::lock(&lock);
        List<Item>::push(item);
        process::unlock(&lock);
    }
};

// A BlockTree is a balanced binary search tree of nodes that permits
// duplicates.

struct BlockTree {
    struct Node {
        List<Block> blocks;
        Node* left;
        Node* right;

        inline u64 key() {
            assert(!blocks.empty());
            return blocks.head->item->header.size;
        }

        inline void* operator new(size_t n) {
            return NodeHeap.malloc();
        }

        inline void operator delete(void* p) {
            NodeHeap.free(p);
        }
    };
    static PrimitiveHeap<Node> NodeHeap;

    Node* root;
    i8 lock;

    inline BlockTree():
        root(nullptr), lock(0) {}

    inline bool empty() const {
        return !root;
    }

    inline void printContents(Node* root) {
        if (!root) {
            print("nil");
            return;
        }
        print("([");
        auto head = root->blocks.head;
        auto iter = head;
        bool first = true;
        do {
            if (!first)
                print(' ');
            first = false;
            print(iter->item->header.size);
            iter = iter->next;
        } while (iter != head);
        print("] ");
        printContents(root->left);
        print(' ');
        printContents(root->right);
        print(")");
    }

    inline void insert(Node*& root, Block* block) {
        assert(lock);
        if (!root) {
            root = new Node {
                List<Block>(),
                nullptr,
                nullptr
            };
            root->blocks.push(block);
            return;
        }

        if (block->header.size < root->key())
            insert(root->left, block);
        else if (block->header.size > root->key())
            insert(root->right, block);
        else
            root->blocks.push(block);
    }

    inline void insert(Block* item) {
        process::lock(&lock);
        insert(root, item);
        process::unlock(&lock);
    }

    inline Block* takeNextLarger(Node*& root, u64 size, u64 bestSoFar) {
        assert(lock);
        if (!root)
            return nullptr;
        if (size > root->key())
            return takeNextLarger(root->right, size, bestSoFar);
        if (size <= root->key()) {
            Block* possibleBetter = takeNextLarger(root->left, size, root->key());
            if (possibleBetter)
                return possibleBetter;
        }

        // This is our best candidate, less or equal to our desired size.
        Block* block = root->blocks.pop();
        if (root->blocks.empty()) {
            Node* left = root->left;
            Node* right = root->right;
            delete root;
            if (!left && !right)
                root = nullptr;
            else if (left) {
                root = left;
                Node* cursor = root;
                while (cursor->right)
                    cursor = cursor->right;
                cursor->right = right;
            } else if (right) {
                root = right;
            }
        }
        return block;
    }

    inline Block* takeNextLarger(u64 size) {
        size = (size + Block::MediumBlockSize - 1) / Block::MediumBlockSize;
        process::lock(&lock);
        auto block = takeNextLarger(root, size, 0xffffffffffffffffull);
        if (!block) {
            process::unlock(&lock);
            return block;
        }
        if (block->header.size >= size * 2) {
            uptr head = bitcast<uptr>(block) + size * Block::MediumBlockSize;
            Block* nextBlock = bitcast<Block*>(head);
            nextBlock->initializeLarge(block->header.largeSize() - size * Block::MediumBlockSize);
            block->header.size = size;
            insert(root, nextBlock);
        }
        process::unlock(&lock);
        return block;
    }
};

/*
 * Allocators
 * ----------
 * Allocators are thread-local and multi-modal. Each allocator is associated
 * with a block of a specific size class, either small or medium. Within that
 * block, it can be in either bump mode, which allows bump allocations after a
 * block is initially mapped; or free-list mode, which allows popping items off
 * a thread-local free-list. Allocators run into a slow path when either the
 * bumpable region or current free-list are exhausted, triggering a
 * reorganization of the heap. We update the block max-heaps based on the
 * current free slots of each block. Then, we pick the one with the most free
 * slots and steal its free list. Alternatively, if no block has a lot of free
 * slots, we may also heuristically decide to simply map a new block.
 */

struct Heap;
struct HeapHandle;

namespace AllocStats {
    extern u64 bumpAllocs, freeListAllocs, slowPathAllocs, largeAllocs;
    extern u64 totalAllocatedBytes;
}

#if ELUMALLOC_MARK_DEAD_ON_FREE
    extern i8 markDeadLock;
#endif

struct Allocator {
    u8* bottom;
    u8* top;
    u8* next;

    inline Allocator() {
        bottom = top = next = nullptr;
    }

    NOINLINE void* allocateSlow(HeapHandle& handle, u64 sizeClass, u64 roundedSize);

    inline void* allocate(HeapHandle& handle, u64 sizeClass, u64 roundedSize) {
        assert(sizeClass < Block::LargeSizeClass);

        void* result;

        if LIKELY(bottom < top) {
            #if ELUMALLOC_COLLECT_STATS
                AllocStats::bumpAllocs ++;
            #endif
        fastPath:
            result = bottom;
            bottom += roundedSize;
            #if ELUMALLOC_COLLECT_STATS
                AllocStats::totalAllocatedBytes += roundedSize;
            #endif
            ALLOC_LOG(2, "Bump allocated object of size class ", Block::SizeClasses[sizeClass], " at ", hex(uptr(result)));

            #if ELUMALLOC_VALIDATE_ALLOCATION_SIZE
                assert(!(uptr(bottom) & 0xffff000000000000ull));
                assert(!(uptr(top) & 0xffff000000000000ull));
                uptr addr = bitcast<uptr>(result);
                uptr mediumBase = addr & ~(Block::MediumBlockSize - 1ull);
                Block* block = bitcast<Block*>(mediumBase);

                if LIKELY(block->header.isSmall())
                    block = bitcast<Block*>(addr & ~(Block::SmallBlockSize - 1ull));
                assert(block->header.sizeClass == sizeClass);
            #endif

            return result;
        }

        if UNLIKELY(!next) {
            #if ELUMALLOC_COLLECT_STATS
                AllocStats::slowPathAllocs ++;
            #endif
            return allocateSlow(handle, sizeClass, roundedSize);
        }

        #if ELUMALLOC_COLLECT_STATS
            AllocStats::freeListAllocs ++;
        #endif

        bottom = next;
        FreeList freeList = load<FreeList>(bottom);
        ALLOC_LOG(2, "Loaded free list at ", hex(uptr(bottom)), " with size = ", freeList.size, " and offset = ", freeList.offsetToNext);
        top = bottom + freeList.size;
        next = bottom + freeList.offsetToNext;
        ALLOC_LOG(2, "Set up bump interval in local allocator from ", hex(uptr(bottom)), " to ", hex(uptr(top)), " with next = ", hex(uptr(next)));

        #if ELUMALLOC_MARK_DEAD_ON_FREE
            process::lock(&markDeadLock);
            u64* iter = bitcast<u64*>(bottom) + 2;
            u64* end = bitcast<u64*>(top);
            while (iter != end) {
                if (*iter ++ != 0xdeadbeefdeadbeef)
                    panic("Heap corruption! Tried to allocate out of non-dead cell.");
            }
            process::unlock(&markDeadLock);
        #endif

        goto fastPath;
    }
};

/*
 * Heaps
 * -----
 * Heaps are distinct memory spaces that hold and manage page allocations. All
 * allocations happen within a specific heap, and all allocators are bound to a
 * specific heap. By default, allocations target the "common heap", a global,
 * statically-initialized heap that is assumed to
 */

struct HeapHandle {
    Heap* heap;
    Allocator allocators[Block::NumSizeClasses];
};

struct Heap {
    SyncList<Block> blocksBySize[Block::NumSizeClasses];
    SyncList<Block> freeSmallBlocks;
    BlockTree largeBlocks;

    inline HeapHandle handle() {
        return { this, {} };
    }
};

extern Heap commonHeap;

inline HeapHandle& threadLocalHandle() {
    return *bitcast<HeapHandle*>(bitcast<u8*>(process::tls()) + 264);
}

/*
 * Allocator API
 * -------------
 * The following functions constitute the top-level interface for allocating
 * objects in a heap. We guard them behind a namespace to prevent pollution
 * of the global namespace. We also define some macros here to opt a type
 * into elumalloc allocation.
 */

namespace elumalloc {
    extern i8 globalInitializationLock;
    extern bool isGloballyInitialized;

    NOINLINE void* allocateLarge(HeapHandle& handle, u64 size);
    NOINLINE Block* getBlockForSize(HeapHandle& handle, SizeClass sizeClass);
    NOINLINE Block* getLargeBlock(HeapHandle& handle, u64 size);
    NOINLINE void freeLargeBlock(HeapHandle& handle, Block* block);
    void initializeThread();
    void initialize();
    void deinitializeThread();
    void deinitialize();

    inline void* allocateInHeap(HeapHandle& handle, u64 bytes) {
        if (!bytes)
            return nullptr;
        if UNLIKELY(bytes > Block::MaxSizeForMedium)
            return allocateLarge(handle, bytes);
        u64 sizeClass = Block::sizeClassFor(bytes);
        u64 roundedSize = Block::sizeForClass(sizeClass);
        Allocator& allocator = handle.allocators[sizeClass];
        return allocator.allocate(handle, sizeClass, roundedSize);
    }

    inline void freeInHeap(HeapHandle& handle, void* ptr) {
        if (!ptr)
            return;
        uptr addr = bitcast<uptr>(ptr);
        uptr mediumBase = addr & ~(Block::MediumBlockSize - 1ull);
        Block* block = bitcast<Block*>(mediumBase);

        if UNLIKELY(block->header.isLarge()) {
            ALLOC_LOG(2, "Freeing large object ", hex(uptr(ptr)));
            freeLargeBlock(handle, bitcast<Block*>(addr - 16));
            return;
        }

        if LIKELY(block->header.isSmall())
            block = bitcast<Block*>(addr & ~(Block::SmallBlockSize - 1ull));
        ALLOC_LOG(2, "Freeing ", block->header.isSmall() ? "small" : "medium", " object at ", hex(uptr(ptr)), " with block base ", hex(uptr(block)), ", size class ", block->header.allocationSize());

        #if ELUMALLOC_USE_POWER_OF_TWO_SIZE_CLASSES
            // Because we know size classes are powers of two, we can just do a shift.
            i32 offsetInBlock = (addr - bitcast<uptr>(block->bytes)) >> i32(block->header.shiftForDiv);
        #else
            // We use integer reciprocal method to quickly divide the byte offset
            // by the allocation size.
            i32 offsetInBlock = addr - bitcast<uptr>(block->bytes);
            i32 quotient = u64(i64(offsetInBlock) * i64(block->header.multiplierForDiv)) >> 32u;
            quotient += offsetInBlock;
            offsetInBlock = quotient >> i32(block->header.shiftForDiv);
        #endif

        i32 bitmapWord = offsetInBlock >> 6;
        i32 bitIndex = offsetInBlock & 0x3f;
        u64 prevWord = block->header.bitmap[bitmapWord];
        atomics::set_bit(block->header.bitmap + bitmapWord, bitIndex);
        ALLOC_LOG(3, "    ", binary(prevWord, 64), " -> ", binary(block->header.bitmap[bitmapWord], 64));

        #if ELUMALLOC_MARK_DEAD_ON_FREE
            process::lock(&markDeadLock);
            u64* iter = bitcast<u64*>(ptr);
            u64* end = iter + block->header.sizeClassSize + 1;
            while (iter != end)
                *iter ++ = 0xdeadbeefdeadbeef;
            process::unlock(&markDeadLock);
        #endif
    }

    inline void* allocate(u64 bytes) {
        return allocateInHeap(threadLocalHandle(), bytes);
    }

    inline void free(void* ptr) {
        freeInHeap(threadLocalHandle(), ptr);
    }

    extern USED NOINLINE void* allocateOutOfLine(u64 size);
    extern USED NOINLINE void freeOutOfLine(void* ptr);
}

#define ELUMALLOC_COMMON_HEAPED \
    inline void* operator new(size_t size) { return elumalloc::allocate(size); } \
    inline void* operator new[](size_t size) { return elumalloc::allocate(size); } \
    inline void operator delete(void* ptr) { return elumalloc::free(ptr); } \
    inline void operator delete[](void* ptr) { return elumalloc::free(ptr); } \
    inline void operator delete[](void* ptr, size_t) { return elumalloc::free(ptr); } \
    constexpr static u32 x ## __LINE__ = 42

#endif