#include "util/malloc.h"
#include "util/io.h"

namespace allocator {
    /*
    * Uses the memory.map and unmap functions to map a virtual memory region aligned
    * to a certain alignment.
    */

    slice<memory::page> mapAligned(uptr bytes, uptr align) {
        iptr padded = bytes + align + memory::PAGESIZE - 1;
        iptr rounded = bytes + memory::PAGESIZE - 1;
        auto mapped = memory::map(padded / memory::PAGESIZE);
        if (!mapped.data())
            return mapped;
        uptr addr = (uptr)mapped.data(), aligned_addr = ((addr + align - 1ul) & ~(align - 1ul));
        if (addr != aligned_addr) {
            iptr diff = padded / memory::PAGESIZE - rounded / memory::PAGESIZE;
            iptr prepages = (aligned_addr - addr) / memory::PAGESIZE;

            memory::unmap({ mapped.data(), prepages });
            memory::unmap({ (memory::page*)aligned_addr + rounded / memory::PAGESIZE, diff - prepages });
            mapped = { (memory::page*)aligned_addr, rounded / memory::PAGESIZE };
        }
        return mapped;
    }

    Heap::Heap(u32 regions):
        blocks({ nullptr, 0 }), toFreeCount(0), storageMutex(0), blockListMutex(0) {
        memory::fill(blockListHeads, 0, sizeof(blockListHeads));
        memory::fill(blockListTails, 0, sizeof(blockListTails));
        if (!addRegions(regions))
            panic("Failed to allocate initial heap space!");
    }

    bool Heap::addRegions(u32 regions) {
        u64 bytes = regions * BytesPerRegion;
        auto newBlockSpace = mapAligned(bytes, BytesPerRegion);
        if (!newBlockSpace.data())
            return false;
        blocks = newBlockSpace.as_slice<Block>();

        u32 occupancyPages = divideRoundingUp<u32>(regions * BlocksPerRegion / 8, memory::PAGESIZE); // Pages needed to store 1 bit per block.
        occupancyPages = max<u32>(occupancyPages, 16);
        if (occupancyPages != divideRoundingUp<u32>(occupancy.size() * sizeof(u64), memory::PAGESIZE)) {
            auto newOccupancy = memory::map(occupancyPages);
            memory::fill(newOccupancy.data(), 0xff, memory::PAGESIZE * newOccupancy.size());
            memory::copy(newOccupancy.data(), occupancy.data(), occupancy.size() * sizeof(u64));
            if (occupancy.size())
                memory::unmap({ (memory::page*)occupancy.data(), occupancy.size() * (iword)sizeof(u64) / memory::PAGESIZE });
            occupancy = { (u64*)newOccupancy.data(), iword(regions * BlocksPerRegion / 64) };
        }

        return true;
    }

    void Heap::destroy() {
        memory::unmap(blocks.as_slice<memory::page>());
        memory::unmap(occupancy.as_slice<memory::page>());
    }
}

void* operator new(size_t bytes) {
    return malloc(bytes);
}

void operator delete(void* ptr) noexcept {
    free(ptr);
}

void operator delete(void* ptr, size_t) noexcept {
    free(ptr);
}

void* operator new[](size_t bytes) {
    return malloc(bytes);
}

void operator delete[](void* ptr) noexcept {
    free(ptr);
}

void operator delete[](void* ptr, size_t) noexcept {
    free(ptr);
}

extern "C" NOINLINE void* mallocOutOfLine(u64 bytes) {
    return malloc(bytes);
}

extern "C" NOINLINE void freeOutOfLine(void* ptr) {
    return free(ptr);
}

allocator::Heap TheHeap;
allocator::Allocator sharedAllocator;
i8 sharedMutexes[allocator::NumSizeClasses];

#if SLOW_GUARD_MALLOC
namespace MallocLCG {
    uword x = 3353408407ull;
}
#endif