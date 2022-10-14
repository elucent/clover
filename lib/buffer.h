#ifndef BASIL_LIB_BUFFER_H
#define BASIL_LIB_BUFFER_H

#include "core/def.h"
#include "lib/malloc.h"

template<typename T, uptr N>
struct buffer {
    T elts[N];
    iptr start = 0, end = 0;

    T& operator[](iptr i) {
        return elts[start + i & N - 1];
    }

    const T& operator[](iptr i) const {
        return elts[start + i & N - 1];
    }

    iptr size() const {
        return end - start & N - 1;
    }

    operator bool() const {
        return size();
    }

    T& peek() {
        return elts[start];
    }

    const T& peek() const {
        return elts[start];
    }

    T read() {
        T elt = peek();
        start = start + 1 & N - 1;
        return elt;
    }

    void push(const T& t) {
        elts[end] = t;
        end = end + 1 & N - 1;
    }

    bool full() {
        return size() == N - 1;
    }
};

enum class EndianOrder : u32 {
    UTIL_LITTLE_ENDIAN = 0x03020100ul,
    UTIL_BIG_ENDIAN = 0x00010203ul
};

static const union {
    u8 bytes[4];
    u32 value;
} host_order = { { 0, 1, 2, 3 } };

template<typename T>
T flip_endian(T value) {
    T result = 0;
    constexpr const i64 bits = sizeof(T) * 8;
    for (i64 i = bits - 8; i >= 0; i -= 8) {
        result |= ((value >> i) & 0xff) << ((bits - 8) - i);
    }
    return result;
}

template<typename T>
T little_endian(T value) {
    if ((EndianOrder)host_order.value == EndianOrder::UTIL_LITTLE_ENDIAN)
        return value;
    return flip_endian(value);
}

template<typename T>
T big_endian(T value) {
    if ((EndianOrder)host_order.value == EndianOrder::UTIL_BIG_ENDIAN)
        return value;
    return flip_endian(value);
}

template<typename T>
T from_big_endian(T value) {
    if ((EndianOrder)host_order.value == EndianOrder::UTIL_BIG_ENDIAN)
        return value;
    return flip_endian(value);
}

template<typename T>
T from_little_endian(T value) {
    if ((EndianOrder)host_order.value == EndianOrder::UTIL_LITTLE_ENDIAN)
        return value;
    return flip_endian(value);
}

template<>
inline float big_endian(float f) {
    u32 u = big_endian<u32>(*(u32*)&f);
    return *(float*)&u;
}

template<>
inline double big_endian(double f) {
    u64 u = big_endian<u64>(*(u64*)&f);
    return *(double*)&u;
}

template<>
inline float little_endian(float f) {
    u32 u = little_endian<u32>(*(u32*)&f);
    return *(float*)&u;
}

template<>
inline double little_endian(double f) {
    u64 u = little_endian<u64>(*(u64*)&f);
    return *(double*)&u;
}

template<>
inline float from_big_endian(float f) {
    u32 u = from_big_endian<u32>(*(u32*)&f);
    return *(float*)&u;
}

template<>
inline double from_big_endian(double f) {
    u64 u = from_big_endian<u64>(*(u64*)&f);
    return *(double*)&u;
}

template<>
inline float from_little_endian(float f) {
    u32 u = from_little_endian<u32>(*(u32*)&f);
    return *(float*)&u;
}

template<>
inline double from_little_endian(double f) {
    u64 u = from_little_endian<u64>(*(u64*)&f);
    return *(double*)&u;
}

template<typename Alloc = allocator>
struct bytebuf {
    iptr _start;
    iptr _end;
    u64 _capacity;
    Alloc* _alloc;
    i8* _data;
    bytebuf(Alloc* alloc = Alloc::instance, u64 capacity = 32):
        _start(0), _end(0), _capacity(capacity), _alloc(alloc), _data(new(*_alloc) i8[_capacity]) {
        // starts with empty (uninitialized) buffer of 32 bytes
    }

    ~bytebuf() {
        _alloc->free(_data);
    }

    bytebuf(const bytebuf& other):
        _start(other._start), _end(other._end),
        _capacity(other._capacity), _alloc(other._alloc), _data(new(*_alloc) i8[_capacity]) {

        // copies memory from start to end
        for (i64 i = _start; i != _end; i = (i + 1) & (_capacity - 1))
            _data[i] = other._data[i];
    }

    bytebuf& operator=(const bytebuf& other) {
        if (this != &other) {
            // free existing allocation
            _alloc->free(_data);

            // copy from other
            _start = other._start;
            _end = other._end;
            _capacity = other._capacity;
            _alloc = other._alloc;
            _data = new(*_alloc) i8[_capacity];
            for (i64 i = _start; i != _end; i = (i + 1) & (_capacity - 1))
                _data[i] = other._data[i];
        }
        return *this;
    }

    i8 peek() const {
        // empty buffer returns null char
        if (_start == _end) return '\0';

        // return next byte
        return _data[_start];
    }

    i8 read() {
        // empty buffer returns null char
        if (_start == _end) return '\0';

        // read next byte
        i8 byte = _data[_start];
        _start = (_start + 1) & (_capacity - 1);
        return byte;
    }

    void read(char* buffer, u64 length) {
        for (i64 i = 0; i < length; i ++)
            buffer[i] = read();
    }

    void write(i8 byte) {
        iptr new_end = (_end + 1) & (_capacity - 1);
        if (new_end == _start) {
            // hold onto old buffer
            iptr old_start = _start, old_end = _end, old_capacity = _capacity;
            i8* old_data = _data;

            // grow and clear buffer
            _start = 0;
            _end = 0;
            _capacity *= 2;
            _data = new(*_alloc) i8[_capacity];

            // copy from old buffer
            while (old_start != old_end) {
                write(old_data[old_start]);
                old_start = (old_start + 1) & (old_capacity - 1);
            }

            // free old buffer
            _alloc->free(old_data);
        }

        _data[_end] = byte;
        _end = (_end + 1) & (_capacity - 1);
    }

    void write(u8 byte) {
        write(i8(byte));
    }

    void write(const i8* string, u64 length) {
        for (i64 i = 0; i < length; i ++)
            write((i8)string[i]);
    }

    iptr size() const {
        iptr end = _end;
        if (end < _start) end += _capacity; // account for wraparound
        return end - _start;
    }

    void clear() {
        _start = _end;
    }

    template<typename T>
    T readLE() {
        return from_little_endian<T>(read<T>());
    }

    template<typename T>
    void writeLE(const T& value) {
        write<T>(little_endian<T>(value));
    }

    template<typename T>
    T readBE() {
        return from_big_endian<T>(read<T>());
    }

    template<typename T>
    void writeBE(const T& value) {
        write<T>(big_endian<T>(value));
    }

    template<typename T>
    T read() {
        // serializes object from lowest to highest address
        static i8 buffer[sizeof(T)];
        for (u32 i = 0; i < sizeof(T); i ++)
          buffer[i] = bytebuf::read();
        return *(T*)buffer;
    }

    template<typename T>
    void write(const T& value) {
        // deserializes object, assuming first byte is lowest address
        const i8* data = (const i8*)&value;
        for (u32 i = 0; i < sizeof(T); i ++) write(data[i]);
    }

    void writeLEB(i64 n) {
        bool more = true;
        bool negative = n < 0;

        while (more) {
            u8 val = n & 127;
            n >>= 7;

            if ((n == 0 && !(val & 64)) || (n == -1 && (val & 64)))
                more = 0;
            else val |= 128;
            write<u8>(val);
        }
    }

    i64 readLEB() {
        u64 acc = 0, shift = 0;
        u8 val = (u8)read();
        while (val & 128) {
            acc |= (val & 127) << shift;
            shift += 7;
            val = (u8)read();
        }
        acc |= val << shift;
        shift += 7;
        if ((val & 64) && shift < 64) acc |= (-1 << shift);
        return acc;
    }

    void writeULEB(u64 n) {
        do {
            u8 val = n & 127;
            n >>= 7;
            if (n) val |= 128;
            write((i8)val);
        } while (n);
    }

    u64 readULEB() {
        u64 acc = 0, shift = 0;
        u8 val = (u8)read();
        while (val & 128) {
            acc |= (val & 127) << shift;
            shift += 7;
            val = (u8)read();
        }
        return acc |= val << shift;
    }
};

#endif