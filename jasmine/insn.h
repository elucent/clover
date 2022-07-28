#ifndef BASIL_JASMINE_INSN_H
#define BASIL_JASMINE_INSN_H

#include "core/def.h"
#include "lib/vec.h"
#include "lib/io.h"
#include "jasmine/type.h"
#include "jasmine/tab.h"
#include "jasmine/arch.h"

struct OpMeta {
    bool hasoutput, hastype, isjump;
};

constexpr OpMeta HAS_OUTPUT = { true, true, false }, NO_OUTPUT = { false, true, false }, JUMP = { false, false, true }, RETURN = { false, true, true };

enum Arity : u8 {
    NULLARY, UNARY, VUNARY, BINARY, VBINARY
};

enum Op : u8 {
    /* Nullary */   OP_NOP, OP_VAR, OP_LAST_NULLARY = OP_VAR,
    /* Unary */     OP_NEG, OP_NOT, OP_LOAD, OP_FIND, OP_NEW, OP_CAST, OP_CONV, 
                    OP_ZXT, OP_SXT, OP_RET, OP_PAR, OP_JUMP, OP_LABEL, OP_MOV,
    /* Unary* */    OP_PHI, OP_LAST_UNARY = OP_PHI,
    /* Binary */    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_REM, 
                    OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
                    OP_STORE, OP_FINDF, 
                    OP_EQ, OP_NEQ, OP_LT, OP_LEQ, OP_GT, OP_GEQ,
                    OP_JZ, OP_JNZ,
    /* Binary* */   OP_CALL, OP_LAST_BINARY = OP_CALL
};

constexpr const i8* OP_NAMES[] = {
    "nop", "var",
    "neg", "not", "load", "find", "new", "cast", "conv",
    "zxt", "sxt", "ret", "par", "jump", "label",
    "mov",
    "phi",
    "add", "sub", "mul", "div", "rem",
    "and", "or", "xor", "shl", "shr",
    "store", "findf",
    "eq", "neq", "lt", "leq", "gt", "geq",
    "jz", "jnz",
    "call"
};

constexpr Arity OP_ARITIES[] = {
    NULLARY, NULLARY,
    UNARY, UNARY, UNARY, UNARY, UNARY, UNARY, UNARY,
    UNARY, UNARY, UNARY, UNARY, UNARY, UNARY,
    UNARY,
    VUNARY,
    BINARY, BINARY, BINARY, BINARY, BINARY, 
    BINARY, BINARY, BINARY, BINARY, BINARY, 
    BINARY, BINARY, 
    BINARY, BINARY, BINARY, BINARY, BINARY, BINARY, 
    BINARY, BINARY, 
    VBINARY
};

constexpr OpMeta OP_META[] = {
    NO_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, 
    HAS_OUTPUT, HAS_OUTPUT, RETURN, HAS_OUTPUT, JUMP, NO_OUTPUT, 
    HAS_OUTPUT,
    HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    NO_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    JUMP, JUMP,
    HAS_OUTPUT
};

enum Param : u8 {
    P_NONE = 0, P_REG = 1, P_IMM = 2, P_DATA = 3, P_STATIC = 4, P_LABEL = 5, P_FUNC = 6, P_VAR = 7
};

using localidx = i64;
using labelidx = i64;

inline constexpr u8 argsof(Param lhs, Param rhs) {
    return u8(lhs) << 4 | u8(rhs);
}

union Arg {
    localidx reg;
    i64 imm;
    dataidx data;
    statidx stat;
    labelidx lbl;
    funcidx func;
    stridx str;

    inline Arg() {}
    inline Arg(i64 n): imm(n) {}
};

struct Function;

struct Insn {
    Op op; // Opcode
    u32 nparams, pidx, aidx; // Number of args, index of first param, index of first arg
    typeidx type; // Insn type
    Function* fn;

    inline Insn(Function* fn_in): fn(fn_in) {}

    inline const_slice<Param> params() const;
    inline slice<Param> params();
    inline const_slice<Arg> args() const;
    inline slice<Arg> args();

    inline bool has_output() const {
        return OP_META[op].hasoutput;
    }

    inline bool is_jump() const {
        return OP_META[op].isjump;
    }

    inline typeidx result_type(const TypeTable& tab) const {
        if (op == OP_CALL) return tab.types[type].ret;
        else return type;
    }
};

using InsnVec = vec<Insn, 16, arena>;

struct JasmineModule;

struct Function {
    JasmineModule* obj;
    typeidx type;
    bool isextern = false;
    vec<Insn, 8, arena> insns;
    vec<Param, 16, arena> params;
    vec<Arg, 16, arena> args;
    vec<i64, 8, arena> labels;
    stridx modname, name;
    vec<Slot, 8, arena> slots;
    i64 stack = 0;

    Function(JasmineModule* obj_in);

    void write(bytebuf<arena>& buf) const;
    void read(bytebuf<arena>& buf);
    void format(stream& io, u32 indent = 2) const;
    void formatshort(stream& io) const;

    inline labelidx label() {
        labels.push(-1);
        return labels.size() - 1;
    }
};

inline const_slice<Param> Insn::params() const {
    return { &fn->params[pidx], nparams };
}

inline slice<Param> Insn::params() {
    return { &fn->params[pidx], nparams };
}

inline const_slice<Arg> Insn::args() const {
    return { &fn->args[aidx], nparams };
}

inline slice<Arg> Insn::args() {
    return { &fn->args[aidx], nparams };
}

void format_insn(const Insn& insn, stream& io);
void format_func(const Function& fn, stream& io, u32 indent);

namespace jasm {
    extern Function* targetfn;

    struct ParamArg {
        Param p;
        Arg a;
    };

    inline ParamArg reg(u64 r) {
        ParamArg uarg;
        uarg.p = P_REG;
        uarg.a.reg = r;
        return uarg;
    }

    inline ParamArg imm(i64 i) {
        ParamArg uarg;
        uarg.p = P_IMM;
        uarg.a.imm = i;
        return uarg;
    }

    inline ParamArg lbl(labelidx l) {
        ParamArg uarg;
        uarg.p = P_LABEL;
        uarg.a.lbl = l;
        return uarg;
    }

    inline ParamArg func(funcidx i) {
        ParamArg uarg;
        uarg.p = P_FUNC;
        uarg.a.func = i;
        return uarg;
    }

    funcidx externfunc(JasmineModule& obj, const_slice<i8> name, typeidx type);
    funcidx beginfunc(JasmineModule& obj, const_slice<i8> name, typeidx type);
    void endfunc();

    localidx add(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx sub(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx mul(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx div(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx rem(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx neg(typeidx t, const ParamArg& param);

    localidx band(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx bor(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx bxor(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx bnot(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx shl(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx shr(typeidx t, const ParamArg& param);

    void store(typeidx t, const ParamArg& addr, const ParamArg& val);
    localidx load(typeidx t, const ParamArg& addr);
    localidx mov(typeidx t, const ParamArg& dest, const ParamArg& src);
    localidx find(typeidx t, const ParamArg& src);
    localidx findf(typeidx t, const ParamArg& base, const ParamArg& field);
    localidx mnew(typeidx t);

    localidx cast(typeidx t, const ParamArg& param);
    localidx conv(typeidx t, const ParamArg& param);
    localidx zxt(typeidx t, const ParamArg& param);
    localidx sxt(typeidx t, const ParamArg& param);

    localidx begincall(typeidx t, const ParamArg& param);
    void callarg(const ParamArg& param);
    void endcall();

    void ret(typeidx t, const ParamArg& param);
    localidx par(typeidx t, i64 idx);
    localidx jump(const ParamArg& param);
    void label(labelidx lbl);
    localidx phi(typeidx t, const ParamArg& lhs, const ParamArg& rhs);

    localidx eq(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx neq(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx gt(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx geq(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx lt(typeidx t, const ParamArg& lhs, const ParamArg& rhs);
    localidx leq(typeidx t, const ParamArg& lhs, const ParamArg& rhs);

    void jz(const ParamArg& dest, const ParamArg& cond);
    void jnz(const ParamArg& dest, const ParamArg& cond);

    localidx var(typeidx t);
}

#endif