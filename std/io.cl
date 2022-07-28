module io:
    # System-agnostic representation for a file handle, managed by
    # the clover runtime.
    alias fd: i64

    module file:
        # File access flags.
        i8 NONE: 0, READ: 1, WRITE: 2, APPEND: 4

        # Open and close files.
        fd open(string path, int flags)
        void close(fd file)

        # Standard filehandles for input, output, and errors.
        fd stdin: 0, stdout: 1, stderr: 2

    # Represents an opened file stream.
    type Stream:
        i8[] buf
        i32 start, end
        fd desc
    
    var EMPTY_STREAM: Stream([], -1, -1, -1)
    Stream*[] streams: new[65536] &EMPTY_STREAM
    defer del streams