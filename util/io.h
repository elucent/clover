#ifndef UTILITY_IO_H
#define UTILITY_IO_H

#include "rt/sys.h"
#include "util/math.h"

enum StreamType {
    ST_STD = 0, ST_FILE = 1, ST_BUF = 2
};

extern "C" void io_init();
extern "C" void io_deinit();

using file::fd;

void flush(fd io);

extern fd io_stdin, io_stdout, io_stderr;

inline void flush_input(fd io) {
    file::FileStorage& info = *file::fd_table[io];
    i8* buf = info.path + info.pathlen;
    memory::copy(buf, buf + info.meta.start, info.meta.end - info.meta.start);
    info.meta.end -= info.meta.start, info.meta.start = 0;
    i64 amt = file::read(io, { buf + info.meta.end, file::FDBUF_SIZE - (info.meta.end - info.meta.start) });
    info.meta.end += amt;
}

inline void flush_output(fd io) {
    file::FileStorage& info = *file::fd_table[io];
    i8* buf = info.path + info.pathlen;
    file::write(io, { buf + info.meta.start, info.meta.end - info.meta.start });
    info.meta.end = info.meta.start = 0;
}

inline const_slice<i8> io_await_input(fd io, i64 n) {
    file::FileStorage& info = *file::fd_table[io];
    i8* buf = info.path + info.pathlen;
    if (info.meta.end - info.meta.start < n) flush_input(io);
    return {buf + info.meta.start, n};
}

inline slice<i8> io_reserve_output(fd io, i64 n) {
    file::FileStorage& info = *file::fd_table[io];
    if (file::FDBUF_SIZE - info.meta.end < n) flush_output(io);
    return {info.path + info.pathlen + info.meta.end, n};
}

template<typename IO, typename T, typename... Args>
IO parse(IO io, T& t, Args&... args);
template<typename IO, typename T, typename... Args>
IO format(IO io, const T& t, const Args&... args);

template<typename T>
struct Formatter;

template<>
struct Formatter<fd> {
    inline static fd get(fd io, i8& ch) {
        file::FileStorage& info = *file::fd_table[io];
        const_slice<i8> buf = io_await_input(io, 1);
        ch = buf[0];
        info.meta.start ++;
        return io;
    }

    inline static fd get(fd io, slice<i8> str) {
        iptr i = 0;
        file::FileStorage& info = *file::fd_table[io];
        u32 buflen = file::FDBUF_SIZE - (offsetof(file::FileStorage, path) + info.pathlen);
        while (i < str.size()) {
            u32 n = str.size() - i;
            u32 chunk = n > buflen ? buflen : n;
            const_slice<i8> buf = io_await_input(io, n);
            memory::copy(str.data() + i, buf.data(), buf.size());
            info.meta.start += chunk;
            i += chunk;
        }
        return io;
    }

    inline static fd put(fd io, i8 c) {
        file::FileStorage& info = *file::fd_table[io];
        slice<i8> buf = io_reserve_output(io, 1);
        buf[0] = c;
        info.meta.end ++;
        if ((io == io_stdout || io == io_stderr) && c == '\n') flush(io);
        return io;
    }

    inline static fd put(fd io, const_slice<i8> str) {
        iptr i = 0;
        if ((io == io_stdout || io == io_stderr)) {
            if (str.size() >= file::FDBUF_SIZE) {
                flush_output(io);
                file::write(io, str);
                return io;
            }
            for (i8 c : str) put(io, c);
        } else {
            file::FileStorage& info = *file::fd_table[io];
            u32 buflen = file::FDBUF_SIZE - (offsetof(file::FileStorage, path) + info.pathlen);
            if (str.size() >= buflen) {
                flush_output(io);
                file::write(io, str);
                return io;
            }
            while (i < str.size()) {
                u32 n = str.size() - i;
                u32 chunk = n > buflen ? buflen : n;
                slice<i8> buf = io_reserve_output(io, chunk);
                memory::copy(buf.data(), str.data(), chunk);
                info.meta.end += chunk;
                i += chunk;
            }
        }
        return io;
    }

    inline static slice<i8> reserve(fd io, i32 n) {
        return io_reserve_output(io, n);
    }

    inline static fd advance(fd io, i32 n) {
        file::FileStorage& info = *file::fd_table[io];
        info.meta.end += n;
        return io;
    }

    inline static void begin(fd io) {
    }

    inline static void end(fd io) {
    }
};

template<>
struct Formatter<slice<i8>> {
    inline static slice<i8> get(slice<i8> io, i8& ch) {
        assert(io.size());
        ch = io[0];
        return io.drop(1);
    }

    inline static slice<i8> get(slice<i8> io, slice<i8> str) {
        assert(io.size() > str.size());
        memory::copy(str.data(), io.data(), str.size());
        return io.drop(str.size());
    }

    inline static slice<i8> put(slice<i8> io, i8 c) {
        if (!io.size())
            return io;
        io[0] = c;
        return io.drop(1);
    }

    inline static slice<i8> put(slice<i8> io, const_slice<i8> str) {
        if (io.size() < str.size())
            str = str.take(io.size());
        memory::copy(io.data(), str.data(), str.size());
        return io.drop(str.size());
    }

    inline static slice<i8> reserve(slice<i8> io, i32 n) {
        assert(io.size() >= n);
        return io;
    }

    inline static slice<i8> advance(slice<i8> io, i32 n) {
        return io.drop(n);
    }

    inline static void begin(slice<i8>) {}
    inline static void end(slice<i8>) {}
};

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, u8 i) {
    return Format::put(io, i);
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, i8 i) {
    return Format::put(io, i);
}

template<typename IO, typename Format = Formatter<IO>>
inline IO parse_impl(IO io, u8& i) {
    i8 t;
    io = Format::get(io, t);
    i = u8(t);
    return io;
}

template<typename IO, typename Format = Formatter<IO>>
inline IO parse_impl(IO io, i8& i) {
    return Format::get(io, i);
}

constexpr const i8* digits[] = {
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

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, u64 u) {
    if (!u)
        return Format::put(io, '0');

    slice<i8> buf = Format::reserve(io, 24);

    u32 c = 0, d = 0;
    u64 p = 1, q = p;
    while (p <= u && p >= q)
        q = p, p *= 10, ++ c;
    d = c;
    while (c >= 2) {
        *(u16*)(buf.data() + c - 2) = *(u16*)(digits[u % 100]);
        u /= 100;
        c -= 2;
    }
    if (c) buf[c - 1] = "0123456789"[u % 10];
    return Format::advance(io, d);
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, i64 i) {
    if (i < 0) io = Format::put(io, '-'), i = -i;
    return format_impl(io, u64(i));
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, u16 i) {
    return format_impl(io, u64(i));
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, u32 i) {
    return format_impl(io, u64(i));
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, i16 i) {
    return format_impl(io, i64(i));
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, i32 i) {
    return format_impl(io, i64(i));
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const i8* str) {
    return Format::put(io, const_slice<i8>{ str, findc(str, '\0') });
}

template<typename IO, typename Format = Formatter<IO>, uword N>
inline IO format_impl(IO io, const i8 str[N]) {
    return Format::put(io, const_slice<i8>{ str, findc(str, '\0') });
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const_slice<i8> str) {
    return Format::put(io, str);
}

#define FP_PRECISION 7

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, double f) {
    if UNLIKELY(is_nan(f)) {
        if (bitcast<u64>(f) & 0x8000000000000000ull)
            return Format::put(io, const_slice<i8>{ "-NaN", 4 });
        return Format::put(io, const_slice<i8>{ "NaN", 3 });
    } else if UNLIKELY(is_inf(f)) {
        if (bitcast<u64>(f) & 0x8000000000000000ull)
            return Format::put(io, const_slice<i8>{ "-Infinity", 9 });
        return Format::put(io, const_slice<i8>{ "Infinity", 8 });
    }

    if (f < 0) io = Format::put(io, '-'), f = -f;
    i64 ipart = i64(f + 0.00000001);
    io = format_impl<IO>(io, ipart);
    io = Format::put(io, '.');

    double frac = f - ipart;
    if (frac < 0) frac = -frac;
    u64 ndigits = 0, leadingZeroes = 0;
    bool isLeading = true;
    double roundUpThreshold = 1;
    while (ndigits < FP_PRECISION) {
        frac *= 10;
        i64 ipart = i64(frac);
        if (!ipart && isLeading)
            leadingZeroes ++;
        else
            isLeading = false;
        if (!isLeading)
            roundUpThreshold *= 10;
        ndigits ++;
    }
    u64 digits = (u64)round(frac);
    if (digits >= roundUpThreshold && leadingZeroes)
        leadingZeroes --;
    if (!digits) leadingZeroes = 0;
    while (digits && digits % 10 == 0)
        digits /= 10;
    for (i64 i = 0; i < leadingZeroes; i ++) io = Format::put(io, '0');
    return format_impl<IO>(io, digits);
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, float f) {
    return format_impl(io, double(f));
}

template<typename T>
struct hex {
    using WordType = choice<i64, u64, isSigned<T>>;
    T value;
    u64 mindigits;

    inline hex(T u):
        value(u), mindigits(0) {}

    inline hex(T u, u64 digits_in):
        value(u), mindigits(digits_in) {}
};

template<typename IO, typename T, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const hex<T>& h) {
    slice<i8> buf = Format::reserve(io, 16);
    u64 digits = 0, n = 0;
    typename hex<T>::WordType copy = (typename hex<T>::WordType)(h.value);
    if (isSigned<T>)
        while (copy && copy != -1) copy >>= 4, digits ++;
    else
        while (copy) copy >>= 4, digits ++;
    if (digits < 2) digits = 2;
    if (digits < h.mindigits) digits = h.mindigits;

    copy = (typename hex<T>::WordType)(h.value);
    for (i64 i = digits * 4 - 4; i >= 0; i -= 4) {
        u8 digit = copy >> i & 15;
        buf[n ++] = "0123456789abcdef"[digit];
    }
    return Format::advance(io, n);
}

template<typename T>
struct binary {
    using WordType = choice<i64, u64, isSigned<T>>;
    T value;
    u64 mindigits;

    inline binary(T u):
        value(u), mindigits(0) {}

    inline binary(T u, u64 digits_in):
        value(u), mindigits(digits_in) {}
};

template<typename IO, typename T, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const binary<T>& b) {
    slice<i8> buf = Format::reserve(io, 64);
    u64 n = 0;
    if (!b.value) {
        u64 digits = b.mindigits;
        if (digits < 2) digits = 2;
        while (digits --)
            buf[n ++] = '0';
        return Format::advance(io, n);
    }
    u64 numRealDigits = 64 - clz64(b.value);
    i32 padding = i32(b.mindigits) - i32(numRealDigits);
    for (i32 i = 0; i < padding; i ++)
        buf[n ++] = '0';
    u64 probe = 1ull << (numRealDigits - 1);
    while (probe) {
        buf[n ++] = (b.value & probe) ? '1' : '0';
        probe >>= 1;
    }
    return Format::advance(io, n);
}

struct leb {
    i64 value;

    inline leb() {}

    inline leb(i64 value_in):
        value(value_in) {}
};

struct uleb {
    u64 value;

    inline uleb() {}

    inline uleb(u64 value_in):
        value(value_in) {}
};

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const leb& l) {
    i64 n = l.value;
    bool more = true;

    while (more) {
        u8 val = n & 127;
        n >>= 7;

        if ((n == 0 && !(val & 64)) || (n == -1 && (val & 64)))
            more = 0;
        else val |= 128;
        io = format(io, val);
    }
    return io;
}

template<typename IO, typename Format = Formatter<IO>>
inline IO parse_impl(IO io, leb& l) {
    u64 acc = 0, shift = 0;
    u8 val;
    io = parse(io, val);
    while (val & 128) {
        acc |= u64(val & 127) << shift;
        shift += 7;
        io = parse(io, val);
    }
    acc |= u64(val & 127) << shift;
    shift += 7;
    if (shift < 64)
        acc |= u64(val & 64 ? ~0ull : 0) << shift;
    l.value = acc;
    return io;
}

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const uleb& u) {
    u64 n = u.value;
    do {
        u8 val = n & 127;
        n >>= 7;
        if (n) val |= 128;
        io = format(io, (i8)val);
    } while (n);
    return io;
}

template<typename IO, typename U, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const U* ptr) {
    return format_impl(io, hex(uptr(ptr), sizeof(uptr) * 2));
}

template<typename IO, typename Format = Formatter<IO>>
inline IO parse_impl(IO io, uleb& u) {
    u64 acc = 0, shift = 0;
    u8 val;
    io = parse(io, val);
    while (val & 128) {
        acc |= (val & 127) << shift;
        shift += 7;
        io = parse(io, val);
    }
    u.value = acc |= val << shift;
    return io;
}

template<typename IO>
inline IO parse(IO io) {
    return io;
}

template<typename IO, typename T, typename... Args>
IO parse(IO io, T& t, Args&... args) {
    io = parse_impl<IO>(io, t);
    return parse(io, args...);
}

template<typename IO>
inline IO format(IO io) {
    return io;
}

template<typename IO, typename T, typename... Args>
IO format(IO io, const T& t, const Args&... args) {
    io = format_impl<IO>(io, t);
    return format(io, args...);
}

inline void write(fd io) {}

template<typename T, typename... Args>
void write(fd io, const T& t, const Args&... args) {
    format(io, t, args...);
}

template<typename... Args>
void writeln(fd io, const Args&... args) {
    format(format(io, args...), '\n');
}

template<typename... Args>
slice<i8> prints(slice<i8> s, const Args&... args) {
    iword n = s.size();
    i8* data = s.data();
    s = format(s, args...);
    return { data, n - s.size() };
}

template<typename... Args>
slice<i8> printsln(slice<i8> s, const Args&... args) {
    iword n = s.size();
    i8* data = s.data();
    s = format(s, args...);
    format(s, '\n');
    return { data, n - s.size() };
}

template<typename... Args>
void print(const Args&... args) {
    write(io_stdout, args...);
}

template<typename... Args>
void println(const Args&... args) {
    write(io_stdout, args...);
    format(io_stdout, '\n');
}

template<typename T, typename... Args>
void read(fd io, T& t, Args&... args) {
    parse(io, t, args...);
}

template<typename... Args>
void input(Args&... args) {
    read(io_stdin, args...);
}

template<typename... Args>
iword inputs(slice<i8> s, Args&... args) {
    iword n = s.size();
    s = parse(s, args...);
    return s.size() - n;
}

template<typename T, typename IO, typename Format = Formatter<IO>>
T get(IO& io) {
    T t;
    io = parse(io, t);
    return t;
}

template<typename T>
T gets(slice<i8> s) {
    gets<slice<i8>, T>(s);
}

template<typename T>
struct DOT {
    const T& value;

    inline DOT(const T& value_in): value(value_in) {}
};

template<typename T>
struct SeqFormat {
    const char* s;
    const T& t;
    inline SeqFormat(const char* s_in, const T& t_in): s(s_in), t(t_in) {}
};

template<typename T>
struct ListFormat {
    const char *l, *s, *r;
    const T& t;
    inline ListFormat(const char* l_in, const char* s_in, const char* r_in, const T& t_in): l(l_in), s(s_in), r(r_in), t(t_in) {}
};

template<typename IO, typename T, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const SeqFormat<T>& seq) {
    bool first = true;
    for (auto i : seq.t) {
        if (!first) io = format(io, seq.s);
        first = false;
        io = format(io, i);
    }
    return io;
}

template<typename IO, typename T, typename Format = Formatter<IO>>
inline IO format_impl(IO io, const ListFormat<T>& seq) {
    bool first = true;
    io = format(io, seq.l);
    for (auto i : seq.t) {
        if (!first) io = format(io, seq.s);
        first = false;
        io = format(io, i);
    }
    return io = format(io, seq.r);
}

namespace utilities {
    template<typename... Args>
    inline void panicPrint(const Args&... args) {
        print(args...);
    }
}

#endif