
############
#  Memory  #
############

in memory:
    alias Flags: u32

    const Read: 1, Write: 2, Exec: 4

    u64 pagesize()
    i8[] map(u64 bytes)
    void unmap(i8[] pages)
    void tag(i8[] pages, Flags)
    void decommit(i8[] pages)

###########
#  Files  #
###########

in file:
    alias Flags: u32
    alias Kind: u8

    const Read: 1, Write: 2, Append: 4

    type fd:
        i32 id

    fd stdin, stdout

    fd open(i8[] path, Flags)
    u32 read(fd, uninit i8[] output)
    u32 write(fd, i8[] message)
    void close(fd)

    type Info:
        u32 size
        Kind kind

    Info info(fd)
    Info pathinfo(i8[] path)

    void remove(i8[] path)
    i8[] cwd(i8[] output)

#################
#  Directories  #
#################

in dir:
    use file.fd, file.Flags, file.Kind

    fd open(i8[] path, Flags)
    void close(fd)
    void remove(i8[] path)

    type Entry:
        i8[] path
        u32 pathlen
        Kind kind

    u32 read(fd, Entry[] output)

###############
#  Processes  #
###############

in process:
    void trap()
    void exit(i32 exitCode)
    i32 exec(i8[] path, i8[][] argv)
    u32 argc()
    i8[] arg(u32 i)

##########
#  Time  #
##########

in time:
    i64 seconds()
    i64 millis()
    i64 nanos()
    i64 ticks()

#############
#  Atomics  #
#############

in atomic:
    void setBit(u64* word, u64 bit)
    void clearBit(u64* word, u64 bit)
    void testSetBit(u64* word, u64 bit)
    void testClearBit(u64* word, u64 bit)

