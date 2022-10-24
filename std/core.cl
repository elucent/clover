module core:
    module memory:
        # Represents a page of memory.
        type page: i8[4096]
    
        # Requests a contiguous block of general-purpose pages from the operating system.
        page[] map(int n)
    
        # Frees a contiguous block of pages from the operating system.
        void unmap(page[] pages)
    
        # Protects a contiguous block of pages.
        i8 NONE: 0, READ: 1, WRITE: 2, EXEC: 4
        void tag(page[] pages, i8 flags)

    module fs:
        # Kind ids for different filesystem entities.
        type kind:
            case none
            case file
            case dir
            case socket
        
        # Permission flags attached to file descriptors.
        i8 NONE: 0, READ: 1, WRITE: 2, EXEC: 4, APPEND: 8

        # System-agnostic representation for a file handle, managed by
        # the clover runtime.
        alias fd: int

        # System-agnostic representation for a path, managed by the
        # clover runtime.
        alias path: i8[]

        # Generic information describing a directory or file.
        type info:
            case file:
                int length  # Length in bytes of file.
            case dir:
                int length  # Number of entries in directory.

        # Returns the kind of a file descriptor.
        kind fs.kindof(fd file)

    alias fd: fs.fd
    alias path: fs.path

    module file:
        # File access flags.

        # Open and close files.
        fd open(fd dir, path p, i8 flags)
        void close(fd file)

        # Standard filehandles for input, output, and errors.
        fd stdin: 0, stdout: 1, stderr: 2

        # Read and write from files.
        int read(fd file, i8[] buf)
        int write(fd file, i8[] buf)

    module dir:
        # Open and close directories.
        fd open(fd dir, path p)
        void close(fd dir)

        # Filehandles for the root directory and current working directory.
        fd root, wd

        # System-agnostic representation of a directory entry.
        type entry:
            path p
            fs.kind k

        # Read first entries of directory, starting from index 'start'.
        # Returns number of entries read.
        int read(fd dir, int start, entry[] entries)
