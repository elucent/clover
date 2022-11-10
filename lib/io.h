#ifndef BASIL_LIB_IO_H
#define BASIL_LIB_IO_H

#include "core/sys.h"

#define STREAM_SIZE 16384
constexpr const u32 STREAMBUF_SIZE = STREAM_SIZE - sizeof(u32) * 3 - sizeof(i32);
#define N_STREAMS 65536

enum StreamType {
    ST_STD = 0, ST_FILE = 1, ST_BUF = 2
};

struct stream {
    i32 fd;
    u32 start, end;
    StreamType type;
    i8 buf[STREAMBUF_SIZE];
};

#define BASIL_STDIN_FD 0
#define BASIL_STDOUT_FD 1
#define BASIL_STDERR_FD 2

extern "C" void io_init();
extern "C" void io_deinit();
struct rune;

void write_string(stream& io, const i8* str, uptr n);
void write_char(stream& io, i32 r);
void write_rune(stream& io, const rune& r);
void write_byte(stream& io, i8 c);
void write_uint(stream& io, u64 u);
void write_int(stream& io, i64 i);
void write_float(stream& io, double f);
void write_hex(stream& io, u64 u, i64 min = 1);
void read_string(stream& io, i8* str, uptr n);
rune read_char(stream& io);
i8 read_byte(stream& io);
u64 read_uint(stream& io);
i64 read_int(stream& io);

void flush(stream& io);

stream* open(const i8* path, FDFLAGS flags);
void close(stream* file);

extern stream stdin, stdout, stderr;

inline void write_impl(stream& io, const i8* const& str) {
    write_string(io, str, cidx(str, '\0'));
}

inline void write_impl(stream& io, i8* && str) {
    write_string(io, str, cidx(str, '\0'));
}

inline void write_impl(stream& io, const_slice<i8> str) {
    write_string(io, str.ptr, str.n);
}

inline void write_impl(stream& io, const rune& r) {
    write_rune(io, r);
}

inline void write_impl(stream& io, i8 i) {
    write_byte(io, i);
}

inline void write_impl(stream& io, i16 i) {
    write_int(io, i);
}

inline void write_impl(stream& io, i32 i) {
    write_int(io, i);
}

inline void write_impl(stream& io, i64 i) {
    write_int(io, i);
}

inline void write_impl(stream& io, u8 i) {
    write_byte(io, i);
}

inline void write_impl(stream& io, u16 i) {
    write_uint(io, i);
}

inline void write_impl(stream& io, u32 i) {
    write_uint(io, i);
}

inline void write_impl(stream& io, u64 i) {
    write_uint(io, i);
}

inline void write_impl(stream& io, float f) {
    write_float(io, f);
}

inline void write_impl(stream& io, double f) {
    write_float(io, f);
}

inline void write(stream& io) {}

template<typename T, typename... Args>
void write(stream& io, const T& t, const Args&... args) {
    write_impl(io, t);
    write(io, args...);
}

template<typename... Args>
void writeln(stream& io, const Args&... args) {
    write(io, args...);
    write_impl(io, '\n');
}

template<typename... Args>
void print(const Args&... args) {
    write(stdout, args...);
}

template<typename... Args>
void println(const Args&... args) {
    write(stdout, args...);
    write_impl(stdout, '\n');
}

#endif