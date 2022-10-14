#include "jasmine/tab.h"
#include "jasmine/obj.h"

MetaTable::MetaTable(JasmineModule* obj_in, Version ver_in, u16 encoding_in, stridx modname_in):
    obj(obj_in), ver(ver_in), encoding(encoding_in), modname(modname_in), entry(0) {
    entries.alloc = &obj->modspace;
}    

StringTable::StringTable(JasmineModule* obj_in): obj(obj_in) {
    strings.alloc = &obj->modspace;
    strtab.alloc = &obj->modspace;
}

u32 MetaTable::size() const {
    u32 acc = 16;
    for (const auto& e : entries) {
        acc += 8 + e.key.n + e.value.n;
        acc = (acc + 3) / 4 * 4;
    }
    return (acc + 7) / 8 * 8;
}

void MetaTable::write(bytebuf<arena>& buf) const {
    buf.write<u8>(ver.major);
    buf.write<u8>(ver.minor);
    buf.write<u8>(ver.patch);
    buf.write<u8>(encoding);
    buf.writeLEB(modname);
    buf.writeLEB(entry);
    buf.writeULEB(entries.size());
    for (const auto& e : entries) {
        buf.writeULEB(e.key.n);                          // Write key and value sizes.
        u32 i = 0;
        for (; i < e.key.n; i ++) buf.write(e.key[i]);      // Write key string.
        buf.writeULEB(e.value.n);
        u32 j = 0;
        for (; j < e.value.n; j ++) buf.write(e.value[j]);  // Write value string.
    }
}

void MetaTable::read(bytebuf<arena>& buf) {
    ver = Version(buf.read<u8>(), buf.read<u8>(), buf.read<u8>());
    encoding = buf.read<u8>();
    modname = buf.readLEB();
    entry = buf.readLEB();
    u32 nentries = buf.readULEB();
    for (u32 e = 0; e < nentries; e ++) {
        slice<i8> key = obj->makestr(buf.readULEB());
        u32 i = 0;
        for (; i < key.n; i ++) key[i] = buf.read();
        slice<i8> value = obj->makestr(buf.readULEB());
        u32 j = 0;
        for (; j < value.n; j ++) value[j] = buf.read();
        entries.put(key, value);
    }
}

void MetaTable::format(stream& io) const {
    ::write(io, "=== Meta Table ===\n");
    ::write(io, "  Version:     ", ver, '\n');
    ::write(io, "  Encoding:    ", (u16)encoding, '\n');
    ::write(io, "  Name:        ", obj->strings.strings[modname], '\n');
    if (entry >= 0 && entry < obj->funcs.size()) ::write(io, "  Entry Point: ", obj->strings.strings[obj->funcs[entry]->name], '\n');
    if (entries.size() > 0) ::write(io, "  Properties:\n");
    for (const auto& e : entries) {
        ::write(io, "    '", e.key, "': '", e.value, "'\n");
    }
    ::write(io, '\n');
}

u32 StringTable::size() const {
    u32 acc = 4;
    for (const auto& s : strings) {
        acc += 4 + s.n;
        acc = (acc + 3) / 4 * 4;
    }
    return (acc + 7) / 8 * 8;
}

void StringTable::write(bytebuf<arena>& buf) const {
    buf.writeULEB(strings.size());
    for (const auto& s : strings) {
        buf.writeULEB(s.n);
        u32 i = 0;
        for (; i < s.n; i ++) buf.write(s[i]);
    }
}

void StringTable::read(bytebuf<arena>& buf) {
    u32 nstrings = buf.readULEB();
    for (u32 e = 0; e < nstrings; e ++) {
        slice<i8> tmpstr = obj->makestr(buf.readULEB());
        u32 i = 0;
        for (; i < tmpstr.n; i ++) tmpstr[i] = buf.read();
        intern(tmpstr);
    }
}

void StringTable::format(stream& io) const {
    ::write(io, "=== String Table ===\n");
    u32 i = 0;
    for (const_slice<i8> s : strings) {
        ::write(io, "  ");
        ::write_hex(io, i ++);
        ::write(io, ": ", s, '\n');
    }
    ::write(io, '\n');
}