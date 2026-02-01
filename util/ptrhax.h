#ifndef UTILITY_POINTER_HACKS_H
#define UTILITY_POINTER_HACKS_H

#include "rt/def.h"
#include "util/endian.h"

template<typename Ptr, typename Extra>
struct ptr_tuple {
#ifdef RT_48BIT_ADDRESS
    struct Padded {
        Extra extra;
        i8 pad[2 - sizeof(Extra)];
    };

    u64 bits;

    inline void setPtr(Ptr ptr) {
        bits &= 0xffff000000000000ull;
        bits |= bitcast<u64>(ptr);
    }

    inline Ptr getPtr() const {
        return bitcast<Ptr>(bits & 0x0000ffffffffffffull);
    }

    inline void setExtra(Extra extra) {
        bits &= 0x0000ffffffffffffull;
        bits |= (u64)bitcast<u16>(Padded { extra, {} }) << 48;
    }

    inline Extra& refExtra() {
#ifdef RT_LITTLE_ENDIAN
        return *bitcast<Extra*>(bitcast<i8*>(&bits));
#else
        return *bitcast<Extra*>(bitcast<i8*>(&bits) + 6);
#endif
    }

    inline Extra getExtra() const {
        Padded padded = bitcast<Padded>(u16((bits & 0xffff000000000000ull) >> 48));
        return padded.extra;
    }

    inline void setBoth(Ptr ptr, Extra extra) {
        bits = (bitcast<u64>(ptr) & 0x0000ffffffffffffull) | (u64)bitcast<u16>(Padded { extra, {} }) << 48;
    }
#else
    Ptr ptr;
    Extra extra;

    inline void setPtr(Ptr ptr_in) {
        ptr = ptr_in;
    }

    inline Ptr getPtr() const {
        return ptr;
    }

    inline void setExtra(Extra extra_in) {
        extra = extra_in;
    }

    inline Extra& refExtra() {
        return extra;
    }

    inline Extra getExtra() const {
        return extra;
    }

    inline void setBoth(Ptr ptr, Extra extra) {
        setPtr(ptr);
        setExtra(extra);
    }
#endif

    inline ptr_tuple(Ptr ptr_in, Extra extra_in) {
        setBoth(ptr_in, extra_in);
    }
};

template<typename T>
struct packed_ptr {
#ifdef RT_48BIT_ADDRESS
    u8 bytes[6];

    inline packed_ptr(void* ptr) {
        pack(ptr);
    }

    inline void pack(void* ptr) {
        uptr u = uptr(ptr);
        store<u32>(u & 0xffffffffu, bytes);
        store<u16>(u >> 32, bytes + 4);
    }

    inline T* unpack() {
        uptr u =  (uptr)load<u32>(bytes) | (uptr)load<u16>(bytes + 4) << 32;
        return (T*)u;
    }

    inline const T* unpack() const {
        uptr u =  (uptr)load<u32>(bytes) | (uptr)load<u16>(bytes + 4) << 32;
        return (const T*)u;
    }
#else
    u8 bytes[sizeof(void*)];

    inline packed_ptr(void* ptr) {
        store<void*>(ptr, bytes);
    }

    inline void pack(void* ptr) {
        store<void*>(ptr, bytes);
    }

    inline T* unpack() {
        return load<T*>(bytes);
    }

    inline const T* unpack() const {
        return load<const T*>(bytes);
    }
#endif
    inline const T* operator->() const {
        return unpack();
    }

    inline T* operator->() {
        return unpack();
    }

    inline const T& operator*() const {
        return *unpack();
    }

    inline T& operator*() {
        return *unpack();
    }

    inline packed_ptr& operator+=(uptr u) {
        pack(unpack() + u);
        return *this;
    }

    inline packed_ptr& operator-=(uptr u) {
        pack(unpack() + u);
        return *this;
    }

    inline packed_ptr& operator++() {
        return operator+=(1);
    }

    inline packed_ptr& operator--() {
        return operator-=(1);
    }

    inline packed_ptr& operator++(int) {
        auto old = *this;
        operator+=(1);
        return old;
    }

    inline packed_ptr& operator--(int) {
        auto old = *this;
        operator-=(1);
        return old;
    }
};

#ifdef RT_48BIT_ADDRESS
static_assert(sizeof(packed_ptr<i32>) == 6);
#endif

template<typename T>
inline packed_ptr<T> operator+(packed_ptr<T> p, iptr diff) {
    return p.unpack() + diff;
}

template<typename T>
inline packed_ptr<T> operator-(packed_ptr<T> p, iptr diff) {
    return p.unpack() - diff;
}

template<typename T>
inline i32 operator<=>(packed_ptr<T> p, packed_ptr<T> o) {
    return p.unpack() <=> o.unpack();
}

template<typename T>
inline i32 operator<=>(packed_ptr<T> p, T* o) {
    return p.unpack() <=> o;
}

#endif