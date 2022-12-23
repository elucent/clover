#ifndef BASIL_BASIL_H
#define BASIL_BASIL_H

#include "core/def.h"
#include "lib/vec.h"
#include "lib/hash.h"

MODULE(basil)

struct Module {
    slice<i8> source;
};

using Symbol = i32;

struct SymbolTable {
    vec<const_slice<i8>, 16> strings;
    map<const_slice<i8>, Symbol, 16> strtab;

    inline Symbol intern(const_slice<i8> str) {
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

    inline const_slice<i8> str(Symbol sym) const {
        return strings[sym];
    }
};

struct Basil {
    SymbolTable symbols;
};

extern Basil* basilInstance;
extern Module* activeModule;

namespace Options {
    enum Mode {
        RUN, READ, EVAL, COMPILE, REPL
    };

    enum Flags {
        NONE = 0,
        VERBOSE = 1
    };

    extern i32 flags;
    extern Mode mode;
    extern vec<const i8*> sourceFiles;
    extern vec<const i8*> directSource;
}

void repl();
void compileModule(const_slice<i8> path);

ENDMODULE()

#endif