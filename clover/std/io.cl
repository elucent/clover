use std/rt

# Declare file buffer storage

type File:
    u32 id
    const Read: 1, Write: 2, Append: 4

type FileBuffer:
    CLRTFd desc
    u16 start, end
    i32 link
    bool eof
    i8[65520] buf

const IOFileBufferSize: 65520   # TODO: Replace this with sizeof() math.

own FileBuffer*[65536] IOTable: uninit
i32 IOTableFreeList: -1
u32 IOTableLimit: 2

# Initialize stdin

IOTable[0] = own FileBuffer*(new FileBuffer)
IOTable[0].desc = CLRTFileStdin
IOTable[0].start = 0
IOTable[0].end = 0
IOTable[0].link = -1
IOTable[0].eof = false
File stdin: File(0)

# Initialize stdout

IOTable[1] = own FileBuffer*(new FileBuffer)
IOTable[1].desc = CLRTFileStdout
IOTable[1].start = 0
IOTable[1].end = 0
IOTable[1].link = -1
IOTable[1].eof = false
File stdout: File(1)

# File lifetime - allocating file descriptors, and opening and closing files.

u32 IONextFreeFd():
    u32 result: 0
    if IOTableFreeList != -1:
        result = IOTableFreeList as u32
        IOTableFreeList = IOTable[result].link
    else:
        result = IOTableLimit ++
        IOTable[result] = own FileBuffer*(new FileBuffer)
    return result

File open(i8[] path, u32 flags):
    var desc: CLRTFileOpen(path, flags)
    var id: IONextFreeFd() as i32
    IOTable[id].desc = desc
    IOTable[id].start = 0
    IOTable[id].end = 0
    IOTable[id].eof = false
    return File(id as u32)

void close(File file):
    CLRTFileClose(IOTable[file.id].desc)
    IOTable[file.id].link = IOTableFreeList
    IOTableFreeList = file.id

# Flushing for both input and output.

void flushOutput(File file):
    var entry: IOTable[file.id]
    CLRTFileWrite(entry.desc, entry.buf[entry.start:entry.end])
    entry.start = 0
    entry.end = 0

void flushInput(File file):
    var entry: IOTable[file.id]
    for entry.start <= i < entry.end:
        entry.buf[i - entry.start] = entry.buf[i]
    entry.end -= entry.start
    entry.start = 0
    var amount: CLRTFileRead(entry.desc, entry.buf[entry.end:IOFileBufferSize])
    if amount < IOFileBufferSize - entry.end:
        entry.eof = true
    entry.end += amount

# Standard IO interface for input and output.

i8 get(File file):
    var entry: IOTable[file.id]
    if entry.start == entry.end:
        return 0 if entry.eof
        file.flushInput()
    return entry.buf[entry.start ++]

void put(File file, i8 byte):
    var entry: IOTable[file.id]
    file.flushOutput() if entry.end == IOFileBufferSize
    entry.buf[entry.end ++] = byte
    file.flushOutput() if file.id == stdout.id and byte == '\n' as i8

i8[] reserveOutput(File file, u32 amount):
    var entry: IOTable[file.id]
    if entry.end + amount > IOFileBufferSize:
        file.flushOutput()
    return entry.buf[entry.end:IOFileBufferSize]

void advance(File file, u32 amount):
    IOTable[file.id].end += amount

void put(File file, i8[] string):
    var entry: IOTable[file.id]
    if file.id == stdout.id:
        var start: 0
        for i < |string|:
            if string[i] == '\n' as i8:
                var segment: i + 1 - start
                i8[] slice: file.reserveOutput(segment)
                slice[j] = string[start + j] for j < segment
                entry.end += segment
                file.flushOutput()
                start = i + 1
            else if i - start >= IOFileBufferSize:
                var segment: i - start
                i8[] slice: file.reserveOutput(segment)
                slice[j] = string[start + j] for j < segment
                entry.end += segment
                start = i
        if start < |string|:
            var remaining: |string| - start
            i8[] slice: file.reserveOutput(remaining)
            slice[i] = string[start + i] for i < remaining
            file.flushOutput()
            entry.end += remaining
        return
    var remaining: |string|
    var ptr: 0
    while remaining > 0:
        i8[] slice: file.reserveOutput(remaining)
        var amount: remaining if remaining < |slice| else |slice|
        slice[i] = string[ptr + i] for i < amount
        entry.end += amount
        ptr += amount
        remaining -= amount

# Other File methods.

bool eof?(File file):
    var entry: IOTable[file.id]
    entry.eof and entry.start == entry.end

# Generic formatting functions.

fun parse(io, i8* byte):
    *byte = io.get()
    return io

fun write(io, i8 byte):
    io.put(byte)
    return io

fun write(io, i8[] string):
    io.put(string)
    return io

fun write(io, u64 number):
    i8[] buffer: io.reserveOutput(20)
    if number == 0:
        buffer[0] = '0' as i8
        io.advance(1)
        return io
    var p: 1, digits: 0
    while p < number:
        p *= 10
        digits ++
    p /= 10
    for i < digits:
        var digit: number / p
        buffer[i] = '0' as i8 + i8(digit)
        number -= digit * p
        p /= 10
    io.advance(digits)
    return io

fun write(io, bool b):
    write(io, "true") if b else write(io, "false")

void print(x):
    write(stdout, x)

void println(x):
    print(x)
    print('\n' as i8)

# Iteration by line or word.

type LineIterator:
    File file

LineIterator lines(File file):
    LineIterator(file)

LineIterator iter(LineIterator iter):
    iter

LineIterator next(LineIterator iter):
    iter

own i8[] read(LineIterator iter):
    var entry: IOTable[iter.file.id]
    for entry.start <= i < entry.end:
        if entry.buf[i] == '\n' as i8:
            var result: new i8[i + 1 - entry.start]
            result[:] = entry.buf[entry.start:i + 1]
            entry.start = i + 1
            return own i8[](result)
    if entry.eof:
        var result: new i8[entry.end - entry.start]
        result[:] = entry.buf[entry.start:entry.end]
        entry.start = entry.end
        return own i8[](result)
    iter.file.flushInput()
    return iter.read()

bool done(LineIterator iter):
    return iter.file.eof?()

