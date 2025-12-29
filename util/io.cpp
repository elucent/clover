#include "util/io.h"
#include "rt/sys.h"

using namespace memory;
using namespace file;

namespace file {
    extern fd map_new_fd(file::Kind, i32, const_slice<i8>);
    extern fd max_file_yet;
}

extern "C" void io_init() {
    io_stdin = map_new_fd(file::FILE, file::stdin, {"stdin", 6});
    io_stdout = map_new_fd(file::FILE, file::stdout, {"stdout", 7});
    io_stderr = map_new_fd(file::FILE, file::stderr, {"stderr", 7});
}

extern "C" void io_deinit() {
    for (u32 i = 0; i <= max_file_yet; i ++) {
        if (fd_table[i] && fd_table[i]->meta.kind == file::FILE)
            flush(i);
        if (fd_table[i])
            memory::unmap({ (i8*)fd_table[i], FDBUF_SIZE });
    }
}

namespace utilities {
    extern void printString(const_slice<i8> msg) {
        print(msg);
    }

    extern void printUint(iword msg) {
        print(msg);
    }
}

bool isdigit(char c) {
    return c >= '0' && c <= '9';
}

u64 read_digits(fd io) {
    u64 acc = 0;
    u8 i = 0;
    FileStorage& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    while (isdigit(buf[info.meta.start]) && i < 18) {
        acc *= 10;
        acc += buf[info.meta.start] - '0';
        i ++;
        info.meta.start ++;
    }
    return acc;
}

u64 read_uint(fd io) {
    io_await_input(io, 1);
    return read_digits(io);
}

i64 read_int(fd io) {
    io_await_input(io, 1);
    i64 i = 1;
    FileStorage& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    if (buf[info.meta.start] == '-') i = -1, info.meta.start ++;
    io_await_input(io, 1);
    return i * read_digits(io);
}

void read_string(fd io, i8* str, uptr n) {
    FileStorage& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    while (n >= FDBUF_SIZE) {
        flush_input(io);
        memory::copy(str, buf + info.meta.start, info.meta.end - info.meta.start);
        n -= FDBUF_SIZE;
    }
    io_await_input(io, n);
    memory::copy(str, buf + info.meta.start, n);
}

i8 read_byte(fd io) {
    io_await_input(io, 1);
    FileStorage& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    return buf[info.meta.start ++];
}

void flush(fd io) {
    flush_output(io);
}

fd io_stdin, io_stdout, io_stderr;