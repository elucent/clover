#ifndef BASIL_LIB_IO_H
#define BASIL_LIB_IO_H

#include "core/sys.h"

enum StreamType {
    ST_STD = 0, ST_FILE = 1, ST_BUF = 2
};

extern "C" void io_init();
extern "C" void io_deinit();
struct rune;

void write_string(fd io, const i8* str, uptr n);
void write_char(fd io, i32 r);
void write_rune(fd io, const rune& r);
void write_byte(fd io, i8 c);
void write_uint(fd io, u64 u);
void write_int(fd io, i64 i);
void write_float(fd io, double f);
void write_hex(fd io, u64 u, i64 min = 1);
void read_string(fd io, i8* str, uptr n);
rune read_char(fd io);
i8 read_byte(fd io);
u64 read_uint(fd io);
i64 read_int(fd io);

void flush(fd io);

extern fd io_stdin, io_stdout, io_stderr;

inline void write_impl(fd io, const i8* const& str) {
    write_string(io, str, cidx(str, '\0'));
}

inline void write_impl(fd io, i8* && str) {
    write_string(io, str, cidx(str, '\0'));
}

inline void write_impl(fd io, const_slice<i8> str) {
    write_string(io, str.ptr, str.n);
}

inline void write_impl(fd io, const rune& r) {
    write_rune(io, r);
}

inline void write_impl(fd io, i8 i) {
    write_byte(io, i);
}

inline void write_impl(fd io, i16 i) {
    write_int(io, i);
}

inline void write_impl(fd io, i32 i) {
    write_int(io, i);
}

inline void write_impl(fd io, i64 i) {
    write_int(io, i);
}

inline void write_impl(fd io, u8 i) {
    write_byte(io, i);
}

inline void write_impl(fd io, u16 i) {
    write_uint(io, i);
}

inline void write_impl(fd io, u32 i) {
    write_uint(io, i);
}

inline void write_impl(fd io, u64 i) {
    write_uint(io, i);
}

inline void write_impl(fd io, float f) {
    write_float(io, f);
}

inline void write_impl(fd io, double f) {
    write_float(io, f);
}

inline void write(fd io) {}

template<typename T, typename... Args>
void write(fd io, const T& t, const Args&... args) {
    write_impl(io, t);
    write(io, args...);
}

template<typename... Args>
void writeln(fd io, const Args&... args) {
    write(io, args...);
    write_impl(io, '\n');
}

template<typename... Args>
void print(const Args&... args) {
    write(io_stdout, args...);
}

template<typename... Args>
void println(const Args&... args) {
    write(io_stdout, args...);
    write_impl(io_stdout, '\n');
}

#endif