#include "rt/sys.h"

#ifdef RT_LINUX
    #include "sys/mman.h"
    #include "unistd.h"
#endif

namespace memory {
    static u64 page_size = 0;

    u64 pagesize() ASMLABEL("memory.pagesize");
    u64 pagesize() {
        if UNLIKELY(!page_size) {
            #ifdef RT_LINUX
                return page_size = getpagesize();
            #endif
        }
        return page_size;
    }

    slice<i8> map(u64 bytes) ASMLABEL("memory.map");
    slice<i8> map(u64 bytes) {
        assert(bytes % pagesize() == 0);
        i8* result;
        #ifdef RT_LINUX
            result = (i8*)mmap(0, bytes, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
            madvise(result, bytes, MADV_WILLNEED);
        #endif
        return { result, (iword)bytes };
    }

    void tag(slice<i8> pages, u64 flags) ASMLABEL("memory.tag");
    void tag(slice<i8> pages, u64 flags) {
        assert(pages.size() % pagesize() == 0);
        u64 nativeFlags = 0;
        #ifdef RT_LINUX
            if (flags & Tag::READ) nativeFlags |= PROT_READ;
            if (flags & Tag::WRITE) nativeFlags |= PROT_WRITE;
            if (flags & Tag::EXEC) nativeFlags |= PROT_EXEC;
            mprotect(pages.data(), pages.size(), nativeFlags);
        #endif
    }

    void unmap(slice<i8> pages) ASMLABEL("memory.unmap");
    void unmap(slice<i8> pages) {
        assert(pages.size() % pagesize() == 0);
        #ifdef RT_LINUX
            munmap(pages.data(), pages.size());
        #endif
    }

    void decommit(slice<i8> pages) ASMLABEL("memory.decommit");
    void decommit(slice<i8> pages) {
        assert(pages.size() % pagesize() == 0);
        #ifdef RT_LINUX
            madvise(pages.data(), pages.size(), MADV_DONTNEED);
        #endif
    }
}