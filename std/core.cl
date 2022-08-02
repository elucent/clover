module core:
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

    module file:
        # System-agnostic representation for a file handle, managed by
        # the clover runtime.
        alias fd: i64

        # File access flags.
        i8 NONE: 0, READ: 1, WRITE: 2, APPEND: 4

        # Open and close files.
        fd open(string path, int flags)
        void close(fd file)

        # Standard filehandles for input, output, and errors.
        fd stdin: 0, stdout: 1, stderr: 2

        # Read and write from files.
        i64 read(fd file, i8[] buf)
        i64 write(fd file, i8[] buf)
        