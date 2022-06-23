#ifndef BASIL_JASMINE_TABLE_H
#define BASIL_JASMINE_TABLE_H

#include "lib/vec.h"
#include "lib/hash.h"
#include "lib/slice.h"
#include "lib/io.h"
#include "jasmine/type.h"

using dataidx = u64;
using statidx = u64;
using funcidx = u64;
using stridx = u64;

constexpr const dataidx DATA_EXT = 0xffff;
constexpr const statidx STAT_EXT = 0xffff;
constexpr const funcidx FUNC_EXT = 0xffff;
constexpr const stridx STR_EXT = 0xffff;

struct Version {
    u8 major, minor, patch;

    inline Version(u8 major_in, u8 minor_in, u8 patch_in):
        major(major_in), minor(minor_in), patch(patch_in) {}

    inline bool operator<(const Version& v) {
        if (major != v.major) return major < v.major;
        else if (minor != v.minor) return minor < v.minor;
        else return patch < v.patch;
    }

    inline bool operator==(const Version& v) {
        return major == v.major && minor == v.minor && patch == v.patch;
    }

    inline bool operator!=(const Version& v) { return !(*this == v); }
    inline bool operator<=(const Version& v) { return *this == v || *this < v; }
    inline bool operator>(const Version& v) { return *this != v && !(*this < v); }
    inline bool operator>=(const Version& v) { return !(*this < v); }
};

inline void write(stream& io, const Version& v) {
    write(io, (u16)v.major, '.', (u16)v.minor, '.', (u16)v.patch);
}

struct MetaTable {
    JasmineModule* obj;
    Version ver;
    u8 encoding; 
    stridx modname;
    funcidx entry;
    map<const_slice<i8>, const_slice<i8>, 8, arena> entries;

    MetaTable(JasmineModule* obj_in, Version ver_in, u16 encoding_in, stridx modname_in);

    inline void add(const_slice<i8> key, const_slice<i8> value) {
        entries.put(key, value);
    }

    u32 size() const;
    void write(bytebuf<arena>& buf) const;
    void read(bytebuf<arena>& buf);
    void format(stream& io) const;
};

struct StringTable {
    JasmineModule* obj;
    vec<const_slice<i8>, 16, arena> strings;
    map<const_slice<i8>, stridx, 16, arena> strtab;

    StringTable(JasmineModule* obj_in);

    stridx intern(const_slice<i8> str);
    inline stridx intern(u32 len, const i8* str) {
        return intern({ str, len });
    }

    u32 size() const;
    void write(bytebuf<arena>& buf) const;
    void read(bytebuf<arena>& buf);
    void format(stream& io) const;
};

#endif