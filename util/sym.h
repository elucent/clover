#ifndef UTILITY_SYM_H
#define UTILITY_SYM_H

#include "util/hash.h"
#include "util/vec.h"
#include "util/io.h"

using Symbol = i32;

struct string_key {
    const i8* ptr;
    u32 length;
    u32 hash;

    inline string_key(slice<i8> str):
        string_key(str, ::hash(const_slice<i8>{ str.data(), str.size() })) {}

    inline string_key(const_slice<i8> str):
        string_key(str, ::hash(str)) {}

    inline string_key(slice<i8> str, u32 hash_in) {
        ptr = str.data();
        assert(str.size() < 0xffffffffu);
        length = str.size();
        hash = hash_in;
    }

    inline string_key(const_slice<i8> str, u32 hash_in) {
        ptr = str.data();
        assert(str.size() < 0xffffffffu);
        length = str.size();
        hash = hash_in;
    }

    inline bool operator==(const string_key& other) const {
        if (ptr == other.ptr)
            return true;
        if (length != other.length)
            return false;
        if (hash != other.hash)
            return false;
        if (!length)
            return true;
        if (*ptr != *other.ptr)
            return false;
        return memory::compare(ptr, other.ptr, length) == 0;
    }
};

inline u64 hash(const string_key& s) {
    return s.hash;
}

struct SymbolTable {
    vec<const_slice<i8>, 16> strings;
    map<string_key, Symbol, 16> strtab;

    inline Symbol get(const i8* str) {
        return get({ str, findc(str, '\0') });
    }

    inline Symbol get(const_slice<i8> str) {
        string_key key(str);
        auto it = strtab.find(key);
        if (it == strtab.end()) {
            slice<i8> objstr = { nullptr, 0 };
            if (str.size())
                objstr = { new i8[str.size()], str.size() };
            memory::copy(objstr.data(), str.data(), str.size());
            strtab.put(string_key(objstr, key.hash), strings.size());
            strings.push(objstr);
            return strings.size() - 1;
        }
        else return it->value;
    }

    inline Symbol operator[](const i8* str) {
        return get({ str, findc(str, '\0') });
    }

    inline Symbol operator[](const_slice<i8> str) {
        return get(str);
    }

    inline Symbol anon() {
        slice<i8> buf = { new i8[32], 32 };
        prints(buf, ".L", strings.size());
        strings.push(buf);
        return strings.size() - 1;
    }

    inline const_slice<i8> operator[](Symbol sym) const {
        return strings[sym];
    }

    inline const_slice<i8> get(Symbol sym) const {
        return strings[sym];
    }
};

#endif