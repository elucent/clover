module memory:
    # Represents a page of memory.
    type page: i8[4096]

    # Requests a contiguous block of general-purpose pages from the operating system.
    page[] map(i64 n)

    # Frees a contiguous block of pages from the operating system.
    void unmap(page[] pages)

    # Protects a contiguous block of pages.
    i8 NONE: 0, READ: 1, WRITE: 2, EXEC: 4
    void tag(page[] pages, int flags)
