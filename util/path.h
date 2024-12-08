#ifndef UTILITY_PATH_H
#define UTILITY_PATH_H

#include "rt/def.h"
#include "util/utf.h"
#include "util/vec.h"

#if defined(RT_WINDOWS)
constexpr rune OSPathSeparators[2] = { '\\', '/' };
#else
constexpr rune OSPathSeparators[1] = { '/' }; 
#endif

constexpr bool isOSPathSeparator(rune r) {
    for (u32 i = 0; i < sizeof(OSPathSeparators) / sizeof(rune); i ++)
        if (r == OSPathSeparators[i])
            return true;
    return false;
}

struct Path {
    vec<const_slice<i8>, 4> segments;

    inline Path(const_slice<i8> base) {
        const i8* data = base.data();
        const i8* prev = data;
        const i8* iter = data;
        rune current;
        while (iter != base.end()) {
            iter = utf8_decode_forward(iter, &current);
            if (isOSPathSeparator(current)) {
                const i8* end = iter - current.bytes();
                segments.push(const_slice<i8>(prev, end - prev).dup());
                prev = iter;
            }
        }
        if (iter != prev)
            segments.push(const_slice<i8>(prev, iter - prev).dup());
    }

    inline ~Path() {
        for (auto s : segments)
            delete[] s.data();
    }

    inline Path(const Path& other) {
        for (auto s : other.segments)
            segments.push(s.dup());
    }

    inline Path& operator=(const Path& other) {
        if (this != &other) {
            for (auto s : segments)
                delete[] s.data();
            segments.clear();
            for (auto s : other.segments)
                segments.push(s.dup());
        }
        return *this;
    }

    inline slice<i8> to_bytes() const {
        static_assert(sizeof(OSPathSeparators));
        rune separator = OSPathSeparators[0];
        u32 computedLength = (segments.size() - 1) * separator.bytes();
        for (auto s : segments) computedLength += s.size();
        slice<i8> result = { new i8[computedLength], computedLength };
        i8* writer = result.data();
        for (u32 i = 0; i < segments.size(); i ++) {
            if (i)
                writer += utf8_encode(&separator, 1, writer, separator.bytes());
            memory::copy(writer, segments[i].data(), segments[i].size());
            writer += segments[i].size();
        }
        assert(writer == result.end());
        return result;
    }

    inline Path subpath(const_slice<i8> name) const {
        Path result = *this;
        Path parsedChild(name);

        for (auto s : parsedChild.segments)
            result.segments.push(s);
        parsedChild.segments.clear();
        return result;
    }

    inline Path parent() const {
        Path result = *this;
        delete[] result.segments.pop().data();
        return result;
    }
};

inline const_slice<i8> basename(const_slice<i8> name) {
    for (i64 i = i64(name.size()) - 1; i >= 0; i --)
        if (name[i] == '.')
            return { name.data(), i };
    return name;
}

#endif