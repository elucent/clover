module io:
    use "std/core"

    # Represents an IO stream for reading and writing values.
    type Stream:
        case Nil:
        case Buffer:
            i8[] buf
            i32 start, end
        case File:
            i8[] buf
            i32 start, end
            core.file.fd desc
    
    void Stream.free():
        if this is Buffer b:
            del b.buf
        else if this is File f:
            del f.buf
    
    Stream[] streams: new[65536] Stream.Nil()
    defer:
        for stream in streams: 
            stream.free()
        del streams
