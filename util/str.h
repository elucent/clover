#ifndef UTILITY_STR_H
#define UTILITY_STR_H

#include "rt/def.h"
#include "util/vec.h"
#include "util/io.h"

u64 touint(const_slice<i8> str);
u64 hextouint(const_slice<i8> str);
u64 binarytouint(const_slice<i8> str);
i64 toint(const_slice<i8> str);
i64 hextoint(const_slice<i8> str);
i64 binarytoint(const_slice<i8> str);

double tofloat(const_slice<i8> str);

template<u32 N>
struct vec_ref {
    vec<i8, N>* v;

    inline explicit vec_ref(vec<i8, N>& v_in): v(&v_in) {}
};

template<u32 N>
struct Formatter<vec_ref<N>> {
    inline static vec_ref<N> put(vec_ref<N> io, i8 c) {
        io.v->push(c);
        return io;
    }

    inline static vec_ref<N> put(vec_ref<N> io, const_slice<i8> str) {
        for (i8 c : str)
            io.v->push(c);
        return io;
    }

    inline static slice<i8> reserve(vec_ref<N> io, i32 n) {
        while (io.v->size() + n >= io.v->capacity())
            io.v->grow();
        return { (i8*)io.v->data() + io.v->size(), n };
    }

    inline static vec_ref<N> advance(vec_ref<N> io, i32 n) {
        io.v->_size += n;
        return io;
    }

    inline static void begin(vec_ref<N>) {}
    inline static void end(vec_ref<N>) {}
};

template<u32 N = 8, typename... Args>
const_slice<i8> tostring(const Args&... args) {
    vec<i8, N> v;
    auto r = vec_ref<N>(v);
    format(r, args...);
    return v.take_slice();
}

#endif