#include "lib/io.h"
#include "core/sys.h"
#include "lib/utf.h"

static void flush_input(fd io) {
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    mcpy(buf, buf + info.meta.start, info.meta.end - info.meta.start);
    info.meta.end -= info.meta.start, info.meta.start = 0;
    i64 amt = file_read(io, { buf + info.meta.end, FDBUF_SIZE - (info.meta.end - info.meta.start) });
    info.meta.end += amt;
}

static void flush_output(fd io) {
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    file_write(io, { buf + info.meta.start, info.meta.end - info.meta.start });
    info.meta.end = info.meta.start = info.pathlen + sizeof(FileInfo);
}

extern "C" fd libcore_map_new_fd(FileMeta::Kind, i32, const_slice<i8>);

extern "C" void io_init() {
    io_stdin = libcore_map_new_fd(FileMeta::FILE, file_stdin, {"stdin", 6});
    io_stdout = libcore_map_new_fd(FileMeta::FILE, file_stdout, {"stdout", 7});
    io_stderr = libcore_map_new_fd(FileMeta::FILE, file_stderr, {"stderr", 7});
}

extern "C" void io_deinit() {
    for (u32 i = 0; i < MAX_FDS; i ++) {
        if (fd_table[i] && fd_table[i]->meta.kind == FileMeta::FILE) 
            flush(i);
        if (fd_table[i])
            memory_free({ (page*)fd_table[i], FDBUF_SIZE / PAGESIZE });
    }
}

static inline void push_if_necessary(fd io, u32 n = 64) {
    FileInfo& info = *fd_table[io];
    if (FDBUF_SIZE - info.meta.end < n) flush_output(io);
}

static inline void pull_if_necessary(fd io, u32 n = 64) {
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    if (info.meta.end - info.meta.start < n) flush_input(io);
}

static inline void put(fd io, u8 c) {
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    buf[info.meta.end ++] = c;
    if (io == io_stdout && c == '\n') flush_output(io);
}

const i8* digits[] = {
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
    "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
    "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
    "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
    "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
    "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
    "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
    "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
    "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
    "90", "91", "92", "93", "94", "95", "96", "97", "98", "99"
};

void write_uint(fd io, u64 u) {
    push_if_necessary(io, 24);
    if (!u) return put(io, '0');
    u32 c = 0, d = 0;
    u64 p = 1;
    while (p <= u) p *= 10, ++ c;
    d = c;
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    while (c >= 2) {
        *(u16*)(buf + info.meta.end + c - 2) = *(u16*)(digits[u % 100]); 
        u /= 100;
        c -= 2;
    }
    if (c) buf[info.meta.end + c - 1] = "0123456789"[u % 10];
    info.meta.end += d;
}

void write_int(fd io, i64 i) {
    push_if_necessary(io, 24);
    if (i < 0) put(io, '-'), i = -i;
    write_uint(io, i); 
}

inline double abs(double f) {
    return f < 0 ? -f : f;
}

#define FP_PRECISION 7

void write_float(fd io, double f) {
    push_if_necessary(io, 1);
    if (f < 0) f = -f, put(io, '-');
    i64 ipart = i64(f + 0.00000001);
    write_uint(io, ipart);
    write_byte(io, '.');
    double frac = f - ipart;
    if (frac < 0) frac = -frac;
    i64 ndigits = 0, nzeroes = -1;
    while (frac - i64(frac) >= 0.0000001 && frac - i64(frac) <= 0.9999999 && ndigits < FP_PRECISION) {
        if (i64(frac) == 0) nzeroes ++;
        ndigits ++, frac *= 10;
    }
    if (frac - i64(frac) > 0.9999999) frac ++;
    for (i64 i = 0; i < nzeroes; i ++) write_byte(io, '0');
    write_uint(io, i64(frac));
}

void write_hex(fd io, u64 u, i64 min) {
    push_if_necessary(io, 16);
    if (!u) write_byte(io, '0'), write_byte(io, '0');
    else {
        u64 copy = u, digits = 0;
        while (copy) copy >>= 4, digits ++;
        if (digits < 2) digits = 2;
        if (digits % 2 == 0) digits --;
        for (i64 i = digits * 4; i >= 0; i -= 4) {
            u8 digit = u >> i & 15;
            put(io, "0123456789abcdef"[digit]);
        }
    }
}

void write_string(fd io, const i8* str, uptr n) {
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    u32 i = 0;
    if (io == io_stdout) for (i = 0; i < n; i ++) {
        push_if_necessary(io, 1);
        buf[info.meta.end ++] = str[i];
        if (str[i] == '\n') flush_output(io);
    }
    else while (n) {
        u32 chunk = n > FDBUF_SIZE ? FDBUF_SIZE : n;
        push_if_necessary(io, chunk);
        mcpy(buf + info.meta.end, str + i, chunk);
        info.meta.end += chunk;
        i += chunk;
        n -= chunk;
    }
}

void write_char(fd io, i32 c) {
    push_if_necessary(io, 4);
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    rune r = c;
    info.meta.end += utf8_encode(&r, 1, buf + info.meta.end, 4);
    if (io == io_stdout && c == '\n') flush_output(io);
}

void write_rune(fd io, const rune& r) {
    write_char(io, r.u);
}

void write_byte(fd io, i8 c) {
    push_if_necessary(io, 1);
    put(io, c);
    if (io == io_stdout && c == '\n') flush_output(io);
}

bool isdigit(char c) {
    return c >= '0' && c <= '9';
}

u64 read_digits(fd io) {
    u64 acc = 0;
    u8 i = 0;
    FileInfo& info = *fd_table[io];
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
    pull_if_necessary(io);
    return read_digits(io);
}

i64 read_int(fd io) {
    pull_if_necessary(io);
    i64 i = 1;
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    if (buf[info.meta.start] == '-') i = -1, info.meta.start ++;
    pull_if_necessary(io);
    return i * read_digits(io);
}

void read_string(fd io, i8* str, uptr n) {
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    while (n >= FDBUF_SIZE) {
        flush_input(io);
        mcpy(str, buf + info.meta.start, info.meta.end - info.meta.start);
        n -= FDBUF_SIZE;
    }
    pull_if_necessary(io, n);
    mcpy(str, buf + info.meta.start, n);
}

i8 read_byte(fd io) {
    pull_if_necessary(io, 1);
    FileInfo& info = *fd_table[io];
    i8* buf = info.path + info.pathlen;
    return buf[info.meta.start ++];
}

void flush(fd io) {
    flush_output(io);
}

fd io_stdin, io_stdout, io_stderr;