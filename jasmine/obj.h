#ifndef BASIL_JASMINE_OBJ_H
#define BASIL_JASMINE_OBJ_H

#include "jasmine/type.h"
#include "jasmine/insn.h"
#include "jasmine/tab.h"
#include "jasmine/data.h"
#include "jasmine/arch.h"

#if defined(LIBCORE_LINUX) && defined(LIBCORE_AMD64)
#include "jasmine/arch/amd64.h"
using DefaultTarget = jasmine::AMD64LinuxTarget;
#elif defined(LIBCORE_OSX) && defined(LIBCORE_AMD64)
#include "jasmine/arch/amd64.h"
using DefaultTarget = jasmine::AMD64DarwinTarget;
#endif

MODULE(jasmine)

// Jasmine Object File encoding version.
#define JASMINE_ENCODING 1

enum OptLevel {
    OPT_0, OPT_1, OPT_2, OPT_MAX = OPT_2, OPT_SMALL = OPT_2
};

struct PassInfo;

struct JasmineModule {
    StringTable strings;
    MetaTable meta;
    TypeTable types;
    DataTable data;
    StaticTable stat;
    vec<Function*, 8> funcs;
    map<const_slice<i8>, Function*, 8> funcmap;
    PassInfo* info;

    inline JasmineModule(bytebuf<>& buf): 
        strings(this), meta(this, Version(0, 0, 0), JASMINE_ENCODING, 0), types(this), data(this), stat(this) {
        read(buf);
    }

    inline JasmineModule(Version ver, const_slice<i8> name): 
        strings(this), meta(this, ver, JASMINE_ENCODING, strings.intern(name)), types(this), data(this), stat(this) {
    }

    inline slice<i8> makestr(u32 len) {
        return { new i8[len], len };
    }

    inline slice<i8> makestr(u32 len, const i8* str) {
        slice<i8> s = { new i8[len], len };
        mcpy(s.ptr, str, s.n);
        return s;
    }

    void write(bytebuf<>& buf);
    void read(bytebuf<>& buf);
    void formatshort(fd io);
    void format(fd io);
    void dumpDOT(fd io);
    void dumpDOT(fd io, PassInfo& info);

    void opt(PassInfo& info, OptLevel level);
    void compile(PassInfo& info, Assembly& as);
};

struct JasmineObject {
    vec<JasmineModule*, 8> moduleseq;
    map<const_slice<i8>, JasmineModule*, 8> modules;

    inline JasmineObject(fd io) {
        read(io);
    }

    void write(bytebuf<>& buf);
    void read(bytebuf<>& buf);
    void write(fd io);
    void read(fd io);
    void format(fd io);
};

ENDMODULE()

#endif