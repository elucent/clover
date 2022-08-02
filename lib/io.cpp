#include "lib/io.h"
#include "lib/utf.h"

stream* new_stream(i32 fd) {
    stream* s = (stream*)mreq(STREAM_SIZE / PAGESIZE).ptr;
    s->fd = fd;
    s->start = 0;
    s->end = 0;
    return s;
}

static stream* streams[N_STREAMS];

static_assert(STREAM_SIZE % PAGESIZE == 0, "Stream is not a multiple of the page size.");
static_assert(sizeof(stream) == STREAM_SIZE, "Stream is different from constant stream size.");

stream& io_for_fd(i64 i) {
    return *streams[i];
}

static void flush_input(stream& io) {
    mcpy(io.buf, io.buf + io.start, io.end - io.start);
    io.end -= io.start, io.start = 0;
    i64 amt = fdread(io.fd, { io.buf + io.end, STREAMBUF_SIZE - (io.end - io.start) });
    io.end += amt;
}

static void flush_output(stream& io) {
    fdwrite(io.fd, { io.buf + io.start, io.end - io.start });
    io.end = io.start;
}

stream* open(const i8* path, FDFLAGS flags) {
    i64 fd = fdopen(path, flags);
    if (fd < 0) return nullptr;
    else {
        stream* s = nullptr;
        for (u32 i = 3; i < N_STREAMS; i ++) { // skip fds 0-2 since they're standard
            if (!streams[i]) {
                s = streams[i] = new_stream(fd);
                break;
            }
        }
        return s; // returns -1 if we couldn't find an open stream
    }
}

void close(stream* file) {
    if (!file) return;
    i32 i = file->fd;
    if (i < 0 || i >= N_STREAMS || !streams[i]) return;
    if (streams[i]->end != streams[i]->start) flush_output(*streams[i]);
    fdclose(streams[i]->fd);
    mfree({ (page*)streams[i], sizeof(stream) / PAGESIZE });
    streams[i] = nullptr;
}

extern "C" void io_init() {
    stdin.fd = 0;
    stdout.fd = 1;
    stderr.fd = 2;
    streams[BASIL_STDIN_FD] = &stdin;
    streams[BASIL_STDOUT_FD] = &stdout;
    streams[BASIL_STDERR_FD] = &stderr;
}

extern "C" void io_deinit() {
    for (u32 i = 0; i < N_STREAMS; i ++)
        if (streams[i]) flush(*streams[i]);

    for (u32 i = 3; i < N_STREAMS; i ++)
        if (streams[i]) mfree({ (page*)streams[i], sizeof(stream) / PAGESIZE });
        else break;
}

static inline void push_if_necessary(stream& io, u32 n = 64) {
    if (STREAMBUF_SIZE - io.end < n) flush_output(io);
}

static inline void pull_if_necessary(stream& io, u32 n = 64) {
    if (io.end - io.start < n) flush_input(io);
}

static inline void put(stream& io, u8 c) {
    io.buf[io.end ++] = c;
    if (&io == &io_for_fd(BASIL_STDOUT_FD) && c == '\n') flush_output(io);
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

void write_uint(stream& io, u64 u) {
    push_if_necessary(io, 24);
    if (!u) return put(io, '0');
    u32 c = 0, d = 0;
    u64 p = 1;
    while (p <= u) p *= 10, ++ c;
    d = c;
    while (c >= 2) {
        *(u16*)(io.buf + io.end + c - 2) = *(u16*)(digits[u % 100]); 
        u /= 100;
        c -= 2;
    }
    if (c) io.buf[io.end + c - 1] = "0123456789"[u % 10];
    io.end += d;
}

void write_int(stream& io, i64 i) {
    push_if_necessary(io, 24);
    if (i < 0) put(io, '-'), i = -i;
    write_uint(io, i); 
}

inline double abs(double f) {
    return f < 0 ? -f : f;
}

#define FP_PRECISION 7

void write_float(stream& io, double f) {
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

void write_hex(stream& io, u64 u) {
    push_if_necessary(io, 16);
    if (!u) write_byte(io, '0'), write_byte(io, '0');
    else {
        u64 digits = 0, mask = 15;
        while (mask << digits < u) mask <<= 4, digits ++;
        if (digits % 2 == 0) digits ++;
        for (i64 i = digits * 4; i >= 0; i -= 4) {
            u8 digit = u >> i & 15;
            put(io, "0123456789abcdef"[digit]);
        }
    }
}

void write_string(stream& io, const i8* str, uptr n) {
    u32 i = 0;
    if (&io == &io_for_fd(BASIL_STDOUT_FD)) for (i = 0; i < n; i ++) {
        push_if_necessary(io, 1);
        io.buf[io.end ++] = str[i];
        if (str[i] == '\n') flush_output(io);
    }
    else while (n) {
        u32 chunk = n > STREAMBUF_SIZE ? STREAMBUF_SIZE : n;
        push_if_necessary(io, chunk);
        mcpy(io.buf + io.end, str + i, chunk);
        io.end += chunk;
        i += chunk;
        n -= chunk;
    }
}

void write_char(stream& io, i32 c) {
    push_if_necessary(io, 4);
    rune r = c;
    io.end += utf8_encode(&r, 1, io.buf + io.end, 4);
    if (&io == &io_for_fd(BASIL_STDOUT_FD) && c == '\n') flush_output(io);
}

void write_rune(stream& io, const rune& r) {
    write_char(io, r.u);
}

void write_byte(stream& io, i8 c) {
    push_if_necessary(io, 1);
    put(io, c);
    if (&io == &io_for_fd(BASIL_STDOUT_FD) && c == '\n') flush_output(io);
}

bool isdigit(char c) {
    return c >= '0' && c <= '9';
}

u64 read_digits(stream& io) {
    u64 acc = 0;
    u8 i = 0;
    while (isdigit(io.buf[io.start]) && i < 18) {
        acc *= 10;
        acc += io.buf[io.start] - '0';
        i ++;
        io.start ++;
    }
    return acc;
}

u64 read_uint(stream& io) {
    pull_if_necessary(io);
    return read_digits(io);
}

i64 read_int(stream& io) {
    pull_if_necessary(io);
    i64 i = 1;
    if (io.buf[io.start] == '-') i = -1, io.start ++;
    pull_if_necessary(io);
    return i * read_digits(io);
}

void read_string(stream& io, i8* str, uptr n) {
    while (n >= STREAMBUF_SIZE) {
        flush_input(io);
        mcpy(str, io.buf + io.start, io.end - io.start);
        n -= STREAMBUF_SIZE;
    }
    pull_if_necessary(io, n);
    mcpy(str, io.buf + io.start, n);
}

i8 read_byte(stream& io) {
    pull_if_necessary(io);
    return io.buf[io.start ++];
}

void flush(stream& io) {
    flush_output(io);
}

stream stdin, stdout, stderr;