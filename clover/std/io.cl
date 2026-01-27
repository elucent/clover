use std/rt

# Declare file buffer storage

type File:
    u32 id
    const Read: 1, Write: 2, Append: 4

type FileBuffer:
    file.fd desc
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
IOTable[0].desc = file.stdin
IOTable[0].start = 0
IOTable[0].end = 0
IOTable[0].link = -1
IOTable[0].eof = false
File stdin: File(0)

# Initialize stdout

IOTable[1] = own FileBuffer*(new FileBuffer)
IOTable[1].desc = file.stdout
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
    var desc: file.open(path, flags)
    var id: IONextFreeFd() as i32
    IOTable[id].desc = desc
    IOTable[id].start = 0
    IOTable[id].end = 0
    IOTable[id].eof = false
    return File(id as u32)

void close(File f):
    file.close(IOTable[f.id].desc)
    IOTable[f.id].link = IOTableFreeList
    IOTableFreeList = f.id as i32

# Flushing for both input and output.

void flushOutput(File f):
    var entry: IOTable[f.id]
    file.write(entry.desc, entry.buf[entry.start:entry.end])
    entry.start = 0
    entry.end = 0

void flushInput(File f):
    var entry: IOTable[f.id]
    for entry.start <= i < entry.end:
        entry.buf[i - entry.start] = entry.buf[i]
    entry.end -= entry.start
    entry.start = 0
    var amount: file.read(entry.desc, entry.buf[entry.end:IOFileBufferSize])
    if amount < IOFileBufferSize - entry.end:
        entry.eof = true
    entry.end += amount as u16

# Standard IO interface for input and output, for Files.

i8 peek(File file):
    var entry: IOTable[file.id]
    if entry.start == entry.end:
        return 0 if entry.eof
        file.flushInput()
    return entry.buf[entry.start]

File get(File file, i8* byte):
    var entry: IOTable[file.id]
    if entry.start == entry.end:
        if entry.eof:
            *byte = 0
            return file
        file.flushInput()
    *byte = entry.buf[entry.start ++]
    return file

File put(File file, i8 byte):
    var entry: IOTable[file.id]
    file.flushOutput() if entry.end == IOFileBufferSize
    entry.buf[entry.end ++] = byte
    file.flushOutput() if file.id == stdout.id and byte == '\n' as i8
    return file

i8[] reserveOutput(File file, u32 amount):
    var entry: IOTable[file.id]
    if entry.end + amount > IOFileBufferSize:
        file.flushOutput()
    return entry.buf[entry.end:IOFileBufferSize]

File advance(File file, u32 amount):
    IOTable[file.id].end += amount
    return file

File put(File file, i8[] string):
    return file if |string| == 0
    var entry: IOTable[file.id]
    i8[] buffer: entry.buf[entry.end:IOFileBufferSize]
    if |string| <= |buffer|:
        bool shouldFlush: false
        if file.id == stdout.id for i < |string| if string[i] == '\n' as i8:
            shouldFlush = true
        buffer[i] = string[i] for i < |string|
        entry.end += |string|
        file.flushOutput() if shouldFlush
        return file
    file.flushOutput()
    var sizeInBuffers: |string| / IOFileBufferSize
    for i < sizeInBuffers:
        file.put(string[i * IOFileBufferSize:i * IOFileBufferSize + IOFileBufferSize])
    file.put(string[sizeInBuffers * IOFileBufferSize:|string|])
    return file

# Standard IO interface implemented for byte slices.

i8 peek(i8[] string):
    return string[0]

i8[] get(i8[] string, i8* byte):
    *byte = string[0]
    return string[1:]

i8[] put(i8[] string, i8 byte):
    string[0] = byte
    return string[1:]

i8[] reserveOutput(i8[] string, u32 amount):
    return string if |string| < amount else string[:amount]

i8[] advance(i8[] string, u32 amount):
    string[|string|:] if |string| < amount else string[amount:]

i8[] put(i8[] string, i8[] input):
    string[:|input|] = input
    return string[|input|:]

# Other File methods.

bool eof?(File file):
    var entry: IOTable[file.id]
    entry.eof and entry.start == entry.end

own i8[] read(File f):
    var entry: IOTable[f.id]
    var info: file.info(entry.desc)
    var output: new i8[info.size]
    file.read(entry.desc, output)
    return output as own i8[]

# Generic formatting functions.

fun parse(io, u64* number):
    var acc: 0, mul: 1
    while io.peek() >= i8('0') and io.peek() <= i8('9'):
        acc *= 10
        var digit: (io.peek() - i8('0')) as u64
        acc += digit
        io = io.advance(1)
    *number = acc
    return io

fun parse(io, i64* number):
    i64 mul: 1
    if io.peek() == '-' as i8:
        mul = -1
        io = io.advance(1)
    u64 num
    io = parse(io, &num)
    *number = num as i64 * mul
    return io

fun write(io, i8[] string):
    io.put(string)
    return io

fun write(io, u64 number):
    if number == 0:
        return io.put('0' as i8)
    i8[] buffer: io.reserveOutput(20)
    var p: 1, q: p, digits: 0
    while p <= number and p >= q:
        q = p
        p *= 10
        digits ++
    for i < digits:
        buffer[digits - i - 1] = '0' as i8 + i8(number % 10)
        number /= 10
    io = io.advance(digits)
    return io

fun write(io, i64 number):
    if number < 0:
        io = io.put('-' as i8)
        number = -number
    io.write(number as u64)

fun write(io, bool b):
    write(io, "true") if b else write(io, "false")

void print(x):
    write(stdout, x)

void println(x):
    print(x)
    stdout.put('\n' as i8)

i8[] sprint(i8[] buffer, x):
    var result: buffer.write(x)
    return buffer[:|buffer|-|result|]

# Iteration by line or word.

type FileLineIterator:
    File file

FileLineIterator lines(File file):
    FileLineIterator(file)

FileLineIterator iter(FileLineIterator iter):
    iter

FileLineIterator next(FileLineIterator iter):
    iter

own i8[] read(FileLineIterator iter):
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

bool done(FileLineIterator iter):
    return iter.file.eof?()

