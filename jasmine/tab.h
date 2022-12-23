#ifndef BASIL_JASMINE_TABLE_H
#define BASIL_JASMINE_TABLE_H

#include "lib/vec.h"
#include "lib/hash.h"
#include "lib/slice.h"
#include "lib/io.h"
#include "lib/buffer.h"

MODULE(jasmine)

using dataidx = i32;
using statidx = i32;
using funcidx = i32;
using stridx = i32;

using typeidx = i32;

using localidx = i32;
using labelidx = i32;

struct JasmineModule;

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

inline void write_impl(fd io, const Version& v) {
    write(io, (u16)v.major, '.', (u16)v.minor, '.', (u16)v.patch);
}

struct MetaTable {
    JasmineModule* obj;
    Version ver;
    u8 encoding; 
    stridx modname;
    funcidx entry;
    map<const_slice<i8>, const_slice<i8>, 8> entries;

    MetaTable(JasmineModule* obj_in, Version ver_in, u16 encoding_in, stridx modname_in);

    inline void add(const_slice<i8> key, const_slice<i8> value) {
        entries.put(key, value);
    }

    u32 size() const;
    void write(bytebuf<>& buf) const;
    void read(bytebuf<>& buf);
    void format(fd io) const;
};

struct SymbolTable {
    vec<const_slice<i8>, 16> strings;
    map<const_slice<i8>, stridx, 16> strtab;

    inline stridx intern(const_slice<i8> str) {
        auto it = strtab.find(str);
        if (it == strtab.end()) {
            slice<i8> objstr = { new i8[str.n], str.n };
            mcpy(objstr.ptr, str.ptr, str.n);
            strtab.put(objstr, strings.size());
            strings.push(objstr);
            return strings.size() - 1;
        }
        else return it->value;
    }

    inline stridx anon() {
        stridx i = strings.size();
        strings.push(const_slice<i8>{"", (iptr)0});
        return i;
    }

    inline stridx intern(u32 len, const i8* str) {
        return intern({ str, len });
    }

    inline const_slice<i8> str(stridx sym) const {
        return strings[sym];
    }
};

struct StringTable : public SymbolTable {
    JasmineModule* obj;

    StringTable(JasmineModule* obj_in);

    u32 size() const;
    void write(bytebuf<>& buf) const;
    void read(bytebuf<>& buf);
    void format(fd io) const;
};

ENDMODULE()

#endif