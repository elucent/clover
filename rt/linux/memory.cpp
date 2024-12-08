#include "rt/linux/common.h"
#include "sys/mman.h"

namespace memory {
    enum Syscall {
        Mmap = 0x09,
        Mprotect = 0x0a,
        Munmap = 0x0b,
        Madvise = 0x1c
    };

    slice<page> map(iword pages) ASMLABEL("memory.map");
    slice<page> map(iword pages) {
        page* result = (page*)os_syscall(Mmap, 0, pages * PAGESIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        os_syscall(Madvise, (iword)result, pages * PAGESIZE, MADV_WILLNEED);
        return { result, pages };
    }

    void tag(slice<page> pages, iword flags) ASMLABEL("memory.tag");
    void tag(slice<page> pages, iword flags) {
        iword nativeFlags = 0;
        if (flags & Tag::READ) nativeFlags |= PROT_READ;
        if (flags & Tag::WRITE) nativeFlags |= PROT_WRITE;
        if (flags & Tag::EXEC) nativeFlags |= PROT_EXEC;
        os_syscall(Mprotect, (iword)pages.data(), pages.size() * PAGESIZE, nativeFlags);
    }

    void unmap(slice<page> pages) ASMLABEL("memory.unmap");
    void unmap(slice<page> pages) {
        os_syscall(Munmap, (iword)pages.data(), pages.size() * PAGESIZE);
    }

    void decommit(slice<page> pages) ASMLABEL("memory.decommit");
    void decommit(slice<page> pages) {
        os_syscall(Madvise, (iword)pages.begin(), pages.size() * PAGESIZE, MADV_DONTNEED);
    }
}
