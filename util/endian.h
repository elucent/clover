#ifndef UTILITY_ENDIAN_H
#define UTILITY_ENDIAN_H

#include "rt/def.h"

enum class EndianOrder : u32 {
    LITTLE = 0x03020100ul,
    BIG = 0x00010203ul
};

static const union {
    u8 bytes[4];
    u32 value;
} host_order = { { 0, 1, 2, 3 } };

template<typename T>
inline T bswap(T t);

template<>
inline u8 bswap(u8 x) { return bswap8(x); }
template<>
inline u16 bswap(u16 x) { return bswap16(x); }
template<>
inline u32 bswap(u32 x) { return bswap32(x); }
template<>
inline u64 bswap(u64 x) { return bswap64(x); }

template<typename T>
T flip_endian(T value) {
    return bswap<bit_uint<T>>(value);
}

inline bool is_big_endian() {
    return (EndianOrder)host_order.value == EndianOrder::LITTLE;
}

inline bool is_little_endian() {
    return (EndianOrder)host_order.value == EndianOrder::LITTLE;
}

template<typename T>
T little_endian(T value) {
    if ((EndianOrder)host_order.value == EndianOrder::LITTLE)
        return value;
    return flip_endian(value);
}

template<typename T>
T big_endian(T value) {
    if ((EndianOrder)host_order.value == EndianOrder::BIG)
        return value;
    return flip_endian(value);
}

template<EndianOrder Order, typename T>
T with_endian(T value) {
    if (Order == EndianOrder::BIG)
        return big_endian<T>(value);
    else
        return little_endian<T>(value);
}

template<typename T>
T from_big_endian(T value) {
    if ((EndianOrder)host_order.value == EndianOrder::BIG)
        return value;
    return flip_endian(value);
}

template<typename T>
T from_little_endian(T value) {
    if ((EndianOrder)host_order.value == EndianOrder::LITTLE)
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

#endif