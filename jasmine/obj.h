#ifndef BASIL_JASMINE_OBJ_H
#define BASIL_JASMINE_OBJ_H

#include "jasmine/type.h"
#include "jasmine/insn.h"
#include "jasmine/tab.h"
#include "jasmine/data.h"
#include "jasmine/arch.h"

// Jasmine Object File encoding version.
#define JASMINE_ENCODING 1

enum OptLevel {
    OPT_0, OPT_1, OPT_2, OPT_MAX = OPT_2, OPT_SMALL = OPT_2
};

struct JasmineModule {
    arena modspace;
    StringTable strings;
    MetaTable meta;
    TypeTable types;
    DataTable data;
    StaticTable stat;
    vec<Function*, 8, arena> funcs;
    map<const_slice<i8>, Function*, 8, arena> funcmap;

    inline JasmineModule(bytebuf<arena>& buf): 
        strings(this), meta(this, Version(0, 0, 0), JASMINE_ENCODING, 0), types(this), data(this), stat(this) {
        funcs.alloc = &modspace;
        read(buf);
    }

    inline JasmineModule(Version ver, const_slice<i8> name): 
        strings(this), meta(this, ver, JASMINE_ENCODING, strings.intern(name)), types(this), data(this), stat(this) {
        funcs.alloc = &modspace;
    }

    inline slice<i8> makestr(u32 len) {
        return { new(modspace) i8[len], len };
    }

    inline slice<i8> makestr(u32 len, const i8* str) {
        slice<i8> s = { new(modspace) i8[len], len };
        mcpy(s.ptr, str, s.n);
        return s;
    }

    void write(bytebuf<arena>& buf);
    void read(bytebuf<arena>& buf);
    void formatshort(stream& io);
    void format(stream& io);

    void opt(OptLevel level);
    void target(const Target& arch, OptLevel level);
};

struct JasmineObject {
    arena objspace;
    vec<JasmineModule*, 8, arena> moduleseq;
    map<const_slice<i8>, JasmineModule*, 8, arena> modules;

    inline JasmineObject() {
        moduleseq.alloc = &objspace;
        modules.alloc = &objspace;
    }

    inline JasmineObject(stream& io): JasmineObject() {
        read(io);
    }

    void write(bytebuf<arena>& buf);
    void read(bytebuf<arena>& buf);
    void write(stream& io);
    void read(stream& io);
    void format(stream& io);
};

#endif