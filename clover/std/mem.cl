# Represents a page of memory.
type page: i8[4096]

# Requests a contiguous block of general-purpose pages from the operating system.
page[] mmap(int n)

# Frees a contiguous block of pages from the operating system.
void munmap(page[] pages)

# Protects a contiguous block of pages.
int MP_NONE: 0, MP_READ: 1, MP_WRITE: 2, MP_EXEC: 4
void mprot(page[] pages, int flags)
