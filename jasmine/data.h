#ifndef BASIL_JASMINE_DATA_H
#define BASIL_JASMINE_DATA_H

#include "jasmine/tab.h"
#include "jasmine/type.h"

MODULE(jasmine)

union DVal {
    i8 int8;
    i16 int16;
    i32 int32;
    i64 int64;
    iword iw;
    iptr ptr;
    float f32;
    double f64;
    u8 uint8;
    u16 uint16;
    u32 uint32;
    u64 uint64;
    uword uw;
    iptr ref;
    slice<i8> arr_i8;
    slice<i16> arr_i16;
    slice<i32> arr_i32;
    slice<i64> arr_i64;
    slice<iword> arr_iword;
    slice<iptr> arr_ptr;
    slice<float> arr_f32;
    slice<double> arr_f64;
    slice<u8> arr_u8;
    slice<u16> arr_u16;
    slice<u32> arr_u32;
    slice<u64> arr_u64;
    slice<uword> arr_uword;
    slice<iptr> arr_ref;
    slice<DVal> fields;

    inline DVal() {}

    u32 size(TypeTable* tab, typeidx t) const;
    void write(TypeTable* tab, typeidx t, bytebuf<>& buf) const;
    void read(TypeTable* tab, typeidx t, bytebuf<>& buf);
    void format(fd io, TypeTable* tab, typeidx t) const;
};

template<typename T>
DVal data(TypeTable* tab, typeidx t, const T& v);

template<>
inline DVal data<i8>(TypeTable* tab, typeidx t, const i8& v) {
    if (t != T_I8) fatal("Unexpected i8.");
    DVal d;
    d.int8 = v;
    return d;
}

template<>
inline DVal data<i16>(TypeTable* tab, typeidx t, const i16& v) {
    if (t != T_I16) fatal("Unexpected i16.");
    DVal d;
    d.int16 = v;
    return d;
}

template<>
inline DVal data<i32>(TypeTable* tab, typeidx t, const i32& v) {
    if (t != T_I32) fatal("Unexpected i32.");
    DVal d;
    d.int32 = v;
    return d;
}

template<>
inline DVal data<i64>(TypeTable* tab, typeidx t, const i64& v) {
    if (t != T_I64 && t != T_IWORD && t != T_PTR && t != T_REF) fatal("Unexpected i64.");
    DVal d;
    d.int64 = v;
    return d;
}

template<>
inline DVal data<float>(TypeTable* tab, typeidx t, const float& v) {
    if (t != T_F32) fatal("Unexpected f32.");
    DVal d;
    d.f32 = v;
    return d;
}

template<>
inline DVal data<double>(TypeTable* tab, typeidx t, const double& v) {
    if (t != T_F64) fatal("Unexpected f64.");
    DVal d;
    d.f64 = v;
    return d;
}

template<>
inline DVal data<u8>(TypeTable* tab, typeidx t, const u8& v) {
    if (t != T_U8) fatal("Unexpected i8.");
    DVal d;
    d.uint8 = v;
    return d;
}

template<>
inline DVal data<u16>(TypeTable* tab, typeidx t, const u16& v) {
    if (t != T_U16) fatal("Unexpected i16.");
    DVal d;
    d.uint16 = v;
    return d;
}

template<>
inline DVal data<u32>(TypeTable* tab, typeidx t, const u32& v) {
    if (t != T_U32) fatal("Unexpected i32.");
    DVal d;
    d.uint32 = v;
    return d;
}

template<>
inline DVal data<u64>(TypeTable* tab, typeidx t, const u64& v) {
    if (t != T_U64 && t != T_UWORD) fatal("Unexpected i64.");
    DVal d;
    d.uint64 = v;
    return d;
}

template<>
inline DVal data<const_slice<i8>>(TypeTable* tab, typeidx t, const const_slice<i8>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_I8) fatal("Unexpected i8.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_i8 = {new i8[arr.n], arr.n};
    for (i64 i = 0; i < arr.n; i ++) d.arr_i8[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<i16>>(TypeTable* tab, typeidx t, const const_slice<i16>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_I16) fatal("Unexpected i16.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_i16 = {new i16[arr.n], arr.n};
    for (i64 i = 0; i < arr.n; i ++) d.arr_i16[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<i32>>(TypeTable* tab, typeidx t, const const_slice<i32>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_I32) fatal("Unexpected i32.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_i32 = {new i32[arr.n], arr.n};
    for (i64 i = 0; i < arr.n; i ++) d.arr_i32[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<i64>>(TypeTable* tab, typeidx t, const const_slice<i64>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_I64 && type.elt != T_IWORD && type.elt != T_PTR && type.elt != T_REF) fatal("Unexpected i64.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_i64 = {new i64[arr.n], arr.n};
    for (i64 i = 0; i < arr.n; i ++) d.arr_i64[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<float>>(TypeTable* tab, typeidx t, const const_slice<float>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_F32) fatal("Unexpected f32.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_f32 = {new float[arr.n], arr.n};
    for (i64 i = 0; i < arr.n; i ++) d.arr_f32[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<double>>(TypeTable* tab, typeidx t, const const_slice<double>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_F64) fatal("Unexpected f64.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_f64 = {new double[arr.n], arr.n};
    for (i64 i = 0; i < arr.n; i ++) d.arr_f64[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<u8>>(TypeTable* tab, typeidx t, const const_slice<u8>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_U8) fatal("Unexpected u8.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_u8 = {new u8[arr.n], arr.n};
    for (u64 i = 0; i < arr.n; i ++) d.arr_u8[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<u16>>(TypeTable* tab, typeidx t, const const_slice<u16>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_U16) fatal("Unexpected u16.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_u16 = {new u16[arr.n], arr.n};
    for (u64 i = 0; i < arr.n; i ++) d.arr_u16[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<u32>>(TypeTable* tab, typeidx t, const const_slice<u32>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_U32) fatal("Unexpected u32.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_u32 = {new u32[arr.n], arr.n};
    for (u64 i = 0; i < arr.n; i ++) d.arr_u32[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<u64>>(TypeTable* tab, typeidx t, const const_slice<u64>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.elt != T_U64 && type.elt != T_UWORD) fatal("Unexpected u64.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.arr_u64 = {new u64[arr.n], arr.n};
    for (u64 i = 0; i < arr.n; i ++) d.arr_u64[i] = arr[i];
    return d;
}

template<>
inline DVal data<const_slice<DVal>>(TypeTable* tab, typeidx t, const const_slice<DVal>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_ARR) fatal("Expected array type.");
    else if (type.nelts != arr.n) fatal("Array data size mismatch.");

    DVal d;
    d.fields = {new DVal[arr.n], arr.n};
    for (u64 i = 0; i < arr.n; i ++) d.fields[i] = arr[i];
    return d;
}

inline DVal datatup(TypeTable* tab, typeidx t, const const_slice<DVal>& arr) {
    const Type& type = tab->types[t];
    if (type.kind != TK_TUP) fatal("Expected tuple type.");

    DVal d;
    d.fields = {new DVal[arr.n], arr.n};
    for (u64 i = 0; i < arr.n; i ++) d.fields[i] = arr[i];
    return d;
}

inline void populate_data(TypeTable* tab, typeidx t, slice<DVal>& fields, u64 idx) {
    if (idx != fields.n) fatal("Array/tuple data size mismatch.");
}

template<typename T, typename... Args>
inline void populate_data(TypeTable* tab, typeidx t, slice<DVal>& fields, u64 idx, const T& v, const Args&... args) {
    fields[idx] = data(tab, tab->types[t].members[idx], v);
    idx ++;
    populate_data(tab, t, fields, idx, args...);
}

template<typename T>
inline void populate_slice(slice<T>& fields, u64 idx) {
    if (idx != fields.n) fatal("Array/tuple data size mismatch.");
}

template<typename T, typename... Args>
inline void populate_slice(slice<T>& fields, u64 idx, const T& v, const Args&... args) {
    fields[idx ++] = v;
    populate_slice(fields, idx, args...);
}

template<typename... Args>
inline DVal datatup(TypeTable* tab, typeidx t, const Args&... args) {
    constexpr u64 len = sizeof...(Args);
    DVal d;
    d.fields = slice<DVal>{new DVal[len], (iptr)len};
    populate_data(tab, t, d.fields, 0, args...);
    return d;
}

template<typename T, typename... Args>
inline DVal dataarr(TypeTable* tab, typeidx t, const T& v, const Args&... args) {
    constexpr u64 len = sizeof...(Args) + 1;
    slice<T> s = slice<T>{new T[len], (iptr)len};
    populate_slice(s, 0, v, args...);
    DVal d = data<const_slice<T>>(tab, t, s);
    delete[] s.ptr;
    return d;
}

struct GlobalEntry {
    typeidx type;
    stridx name;
    DVal dat;
};

struct GlobalTable {
    JasmineModule* obj;
    vec<GlobalEntry, 16> entries;
    map<const_slice<i8>, GlobalEntry, 16> entrymap;

    GlobalTable(JasmineModule* obj_in);

    void write(bytebuf<>& buf) const;
    void read(bytebuf<>& buf);
    TypeTable* ttable();
    void def(const GlobalEntry& e);
};

struct DataTable : public GlobalTable {
    DataTable(JasmineModule* obj_in);
    void format(fd io) const;

    inline dataidx def(typeidx type, stridx name, DVal dat) {
        GlobalTable::def({type, name, dat});
        return entries.size() - 1;
    }

    template<typename... Args>
    inline dataidx def(typeidx type, stridx name, const Args&... args) {
        GlobalTable::def({type, name, data(ttable(), type, args...)});
        return entries.size() - 1;
    }

    template<typename... Args>
    inline dataidx defarr(typeidx type, stridx name, const Args&... args) {
        GlobalTable::def({type, name, dataarr(ttable(), type, args...)});
        return entries.size() - 1;
    }

    template<typename... Args>
    inline dataidx deftup(typeidx type, stridx name, const Args&... args) {
        GlobalTable::def({type, name, datatup(ttable(), type, args...)});
        return entries.size() - 1;
    }
};

struct StaticTable : public GlobalTable {
    StaticTable(JasmineModule* obj_in);
    void format(fd io) const;

    inline statidx def(typeidx type, stridx name, DVal dat) {
        GlobalTable::def({type, name, dat});
        return entries.size() - 1;
    }

    template<typename... Args>
    inline statidx def(typeidx type, stridx name, const Args&... args) {
        GlobalTable::def({type, name, data(ttable(), type, args...)});
        return entries.size() - 1;
    }

    template<typename... Args>
    inline statidx defarr(typeidx type, stridx name, const Args&... args) {
        GlobalTable::def({type, name, dataarr(ttable(), type, args...)});
        return entries.size() - 1;
    }

    template<typename... Args>
    inline statidx deftup(typeidx type, stridx name, const Args&... args) {
        GlobalTable::def({type, name, datatup(ttable(), type, args...)});
        return entries.size() - 1;
    }
};

ENDMODULE()

#endif