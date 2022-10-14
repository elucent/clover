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

constexpr OpMeta HAS_OUTPUT = { true, true, false }, NO_OUTPUT = { false, true, false }, JUMP = { false, false, true }, RETURN = { false, true, false };

enum Arity : u8 {
    NULLARY, UNARY, VUNARY, BINARY, VBINARY, TERNARY,
};

enum Op : u8 {
    /* Nullary */   OP_NOP, OP_VAR, OP_LAST_NULLARY = OP_VAR,
    /* Unary */     OP_NEG, OP_NOT, OP_LOAD, OP_ADDR, OP_NEW, OP_CAST, OP_CONV, 
                    OP_ZXT, OP_SXT, OP_RET, OP_ARG, OP_JUMP, OP_LABEL, OP_MOV,
    /* Unary* */    OP_PHI, OP_LAST_UNARY = OP_PHI,
    /* Binary */    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_REM, 
                    OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
                    OP_STORE, OP_FIELD_LOAD, OP_FIELD_ADDR, OP_INDEX_LOAD, OP_INDEX_ADDR,
                    OP_EQ, OP_NEQ, OP_LT, OP_LEQ, OP_GT, OP_GEQ,
                    OP_JZ, OP_JNZ,
    /* Binary* */   OP_CALL, OP_LAST_BINARY = OP_CALL, 
    /* Ternary */   OP_FIELD_STORE, OP_INDEX_STORE, OP_LAST_TERNARY = OP_INDEX_STORE, N_OPCODES = OP_LAST_TERNARY + 1
};

constexpr const i8* OP_NAMES[] = {
    "nop", "var",
    "neg", "not", "ld", "lea", "new", "cast", "conv",
    "zxt", "sxt", "ret", "arg", "jump", "label", "mov",
    "phi",
    "add", "sub", "mul", "div", "rem",
    "and", "or", "xor", "shl", "shr",
    "st", "ld.field", "lea.field", "ld.index", "lea.index",
    "eq", "neq", "lt", "leq", "gt", "geq",
    "jz", "jnz",
    "call",
    "st.field", "st.index"
};

constexpr Arity OP_ARITIES[] = {
    NULLARY, NULLARY,
    UNARY, UNARY, UNARY, UNARY, UNARY, UNARY, UNARY,
    UNARY, UNARY, UNARY, UNARY, UNARY, UNARY, UNARY,
    VUNARY,
    BINARY, BINARY, BINARY, BINARY, BINARY, 
    BINARY, BINARY, BINARY, BINARY, BINARY, 
    BINARY, BINARY, BINARY, BINARY, BINARY, 
    BINARY, BINARY, BINARY, BINARY, BINARY, BINARY, 
    BINARY, BINARY, 
    VBINARY,
    TERNARY, TERNARY
};

constexpr OpMeta OP_META[] = {
    NO_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, 
    HAS_OUTPUT, HAS_OUTPUT, RETURN, HAS_OUTPUT, JUMP, NO_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    NO_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    JUMP, JUMP,
    HAS_OUTPUT,
    NO_OUTPUT, NO_OUTPUT
};

enum Param : u8 {
    P_NONE = 0, P_REG = 1, P_IMM = 2, P_DATA = 3, P_STATIC = 4, P_LABEL = 5, P_FUNC = 6, P_VAR = 7
};

inline constexpr u8 argsof(Param lhs, Param rhs) {
    return u8(lhs) << 4 | u8(rhs);
}

union Arg {
    localidx reg;
    i64 imm;
    float f32imm;
    double f64imm;
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
    u32 pidx; // Number of args, index of first param, index of first arg
    typeidx type; // Insn type
    Op op; // Opcode
    u8 nparams;

    inline Insn() {}

    inline const_slice<Param> params(const Function&) const;
    inline slice<Param> params(Function&);
    inline const_slice<Arg> args(const Function&) const;
    inline slice<Arg> args(Function&);

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

using InsnVec = vec<Insn, 8, arena>;

struct JasmineModule;

struct Function {
    JasmineModule* obj;
    typeidx type;
    bool isextern = false;
    InsnVec insns;
    vec<Param, 16, arena> params;
    vec<Arg, 16, arena> args;
    vec<i64, 8, arena> labels;
    stridx modname, name;

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

inline const_slice<Param> Insn::params(const Function& fn) const {
    return { &fn.params[pidx], nparams };
}

inline slice<Param> Insn::params(Function& fn) {
    return { &fn.params[pidx], nparams };
}

inline const_slice<Arg> Insn::args(const Function& fn) const {
    return { &fn.args[pidx], nparams };
}

inline slice<Arg> Insn::args(Function& fn) {
    return { &fn.args[pidx], nparams };
}

void format_insn(const Function& fn, const Insn& insn, stream& io);
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

    inline ParamArg f32imm(float f) {
        ParamArg uarg;
        uarg.p = P_IMM;
        uarg.a.f32imm = f;
        return uarg;
    }

    inline ParamArg f64imm(double f) {
        ParamArg uarg;
        uarg.p = P_IMM;
        uarg.a.f64imm = f;
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

    void st(typeidx t, const ParamArg& addr, const ParamArg& val);
    localidx ld(typeidx t, const ParamArg& addr);
    localidx lea(typeidx t, const ParamArg& val);
    localidx ld_field(typeidx t, const ParamArg& tup, const ParamArg& ord);
    localidx lea_field(typeidx t, const ParamArg& tup, const ParamArg& ord);
    localidx ld_index(typeidx t, const ParamArg& arr, const ParamArg& idx);
    localidx lea_index(typeidx t, const ParamArg& arr, const ParamArg& idx);
    localidx load(typeidx t, const ParamArg& addr);
    localidx mov(typeidx t, const ParamArg& dest, const ParamArg& src);
    localidx mnew(typeidx t);
    void st_field(typeidx t, const ParamArg& tup, const ParamArg& ord, const ParamArg& src);
    void st_index(typeidx t, const ParamArg& tup, const ParamArg& arr, const ParamArg& idx);

    localidx cast(typeidx t, const ParamArg& param);
    localidx conv(typeidx t, const ParamArg& param);
    localidx zxt(typeidx t, const ParamArg& param);
    localidx sxt(typeidx t, const ParamArg& param);

    localidx begincall(typeidx t, const ParamArg& param);
    void callarg(const ParamArg& param);
    void endcall();

    void ret(typeidx t, const ParamArg& param);
    localidx arg(typeidx t, i64 idx);
    localidx jump(const ParamArg& param);
    void label(labelidx lbl);
    localidx beginphi(typeidx t);
    void phiarg(const ParamArg& param);
    void endphi();

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