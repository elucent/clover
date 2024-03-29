#ifndef BASIL_JASMINE_INSN_H
#define BASIL_JASMINE_INSN_H

#include "core/def.h"
#include "lib/vec.h"
#include "lib/io.h"
#include "jasmine/type.h"
#include "jasmine/tab.h"
#include "jasmine/arch.h"

MODULE(jasmine)

struct OpMeta {
    enum Flags : u8 {
        NONE = 0, OUTPUT = 1, CONTROL = 2, EXITS = 4, CALL = 8, MEMORY = 16, TYPED = 32
    };
    u8 flags;

    inline bool has_output() const {
        return flags & OUTPUT;
    }

    inline bool is_control() const {
        return flags & CONTROL;
    }

    inline bool exits() const {
        return flags & EXITS;
    }

    inline bool is_call() const {
        return flags & CALL;
    }

    inline bool is_memory() const {
        return flags & MEMORY;
    }

    inline bool is_typed() const {
        return flags & TYPED;
    }
};

constexpr OpMeta 
    HAS_OUTPUT = { OpMeta::OUTPUT | OpMeta::TYPED },
    NO_OUTPUT = { OpMeta::NONE | OpMeta::TYPED },
    UNTYPED = { OpMeta::NONE },
    JUMP = { OpMeta::CONTROL },
    BRANCH = { OpMeta::CONTROL | OpMeta::TYPED },
    RETURN = { OpMeta::CONTROL | OpMeta::TYPED | OpMeta::EXITS },
    CALLMETA = { OpMeta::OUTPUT | OpMeta::CALL | OpMeta::TYPED },
    LOAD = { OpMeta::OUTPUT | OpMeta::MEMORY | OpMeta::TYPED },
    STORE = { OpMeta::MEMORY | OpMeta::TYPED };

enum Arity : u8 {
    NULLARY, UNARY, VUNARY, BINARY, VBINARY, TERNARY, QUATERNARY
};

namespace assembler {
    enum Op {
        /* Nullary */   TRAP,                   // Crash.
                        RETVOID,                // No operands or successors.
                        LAST_NULLARY = RETVOID,

        /* Unary */     NOP,                    // Does nothing. No output.
                        VAR,                    // Declares undefined variable of type.
                        NEW,                    // new t : () -> ref. Allocates new instance of t.
                        RET,                    // ret : t -> !. Returns operand from function.
                        JUMP,                   // jump (branch) : () -> (). Jumps to target.
        
        /* Unary* */    PHI,                    // phi t : (t...) -> t. Unifies incoming values.
                        LAST_UNARY = PHI,

        /* Binary */    NEG,                    // neg (Num t) : t -> t. Negates operand.
                        LNOT,                   // lnot (Int t) : t -> t. NOTs operand.
                        BNOT,                   // bnot (Int t) : t -> t. NOTs operand.
                        LD,                     // ld t : ptr|ref -> t. Loads value from pointer or reference.
                        LA,                     // la (ptr|ref t) : t -> t. Loads address from pointer or reference expression.
                        CAST,                   // cast t : u -> t. Bitcasts operand into t.
                        CVT,                    // cvt t : u -> t. Converts operand to t.
                        ZXT,                    // zxt (Int t) : u -> t. Zero-extends narrower integer type to wider integer type.
                        SXT,                    // sxt (Int t) : u -> t. Sign-extends narrower integer type to wider integer type.
                        ARG,                    // arg t : n -> t. Returns nth argument to block.
                        DEF,                    // def t : t -> t. Returns operand.
                        LZC,                    // lzc (Int t) : t -> t. Counts leading zeroes in operand.
                        TZC,                    // tzc (Int t) : t -> t. Counts trailing zeroes in operand.
                        POPC,                   // popc (Int t) : t -> t. Counts set bits in operand.

        /* Binary* */   CALL,                   // call (func f), (ptr|ref p) : (p, f.args) -> f.ret. Calls address with parameters.
                        LAST_BINARY = CALL,

        /* Ternary */   ADD,                    // add (Num t) : (t, t) -> t. Adds operands.
                        SUB,                    // sub (Num t) : (t, t) -> t. Subtracts second operand from first.
                        MUL,                    // mul (Num t) : (t, t) -> t. Multiplies operands.
                        DIV,                    // div (Num t) : (t, t) -> t. Divides first operand by second.
                        REM,                    // rem (Num t) : (t, t) -> t. Finds remainder dividing first operand by second.
                        LAND,                   // land (Int t) : (t, t) -> t. ANDs operands.
                        LOR,                    // lor (Int t) : (t, t) -> t.  ORs operands.
                        LXOR,                   // lxor (Int t) : (t, t) -> t. XORs operands.
                        BAND,                   // band (Int t) : (t, t) -> t. ANDs operands.
                        BOR,                    // bor (Int t) : (t, t) -> t.  ORs operands.
                        BXOR,                   // bxor (Int t) : (t, t) -> t. XORs operands.
                        SHL,                    // shl (Int t) : (t, t) -> t. Shifts first operand left by second.
                        SHR,                    // shr (Int t) : (t, t) -> t. Shifts first operand right by second.
                        MOV,                    // mov t : (t, t) -> void. Moves second operand to first.
                        ST,                     // st (ptr|ref p) t : (p, t) -> (). Stores second operand at first.
                        FLD,                    // fld t (ptr|ref p) : (p, n) -> t.n. Loads nth field of struct pointed at by first operand.
                        FLA,                    // fla t (ptr|ref p) : (p, n) -> p. Loads address of nth field of struct.
                        ILD,                    // ild t, (ptr|ref p) (Int i): (p, i) -> t. Loads ith element of array pointed at by first operand.
                        ILA,                    // ila t, (ptr|ref p) (Int i): (p, i) -> t. Loads address of ith element of array pointed at by first operand.
                        CEQ,                    // ceq t : (t, t) -> u8. Compares equality of operands.
                        CNEQ,                   // cneq t : (t, t) -> u8. Compares inequality of operands.
                        CLT,                    // clt t : (t, t) -> u8. Compares less than of operands.
                        CLEQ,                   // cleq t : (t, t) -> u8. Compares less or equal of operands.
                        CGT,                    // cgt t : (t, t) -> u8. Compares greater of operands.
                        CGEQ,                   // cgeq t : (t, t) -> u8. Compares greater or equal of operands.
                        JZERO,                  // jzero t : (t) -> void. Jumps if zero.
                        JNZERO,                 // jnzero t : (t) -> void. Jumps if nonzero.

                        /* 
                        Atomics
                        CXG,                    // cxg t : (t, t) -> t. Atomic compare and exchange.
                        LDX,                    // Atomic load.
                        FLDX,                   // Atomic load field.
                        ILDX,                   // Atomic load index.
                        STX,                    // Atomic store.
                        FSTX,                   // Atomic store field.
                        ISTX,                   // Atomic store index.
                        */

                        LAST_TERNARY = JNZERO,

        /* Quat. */     FST,                    // fst t (ptr|ref p) : (p, n, t.n) -> (). Stores into nth field of pointed-to struct.
                        IST,                    // ist t (ptr|ref p) (Int i) : (p, i, t.n) -> (). Stores into ith element of pointed-to array.
                        JEQ,
                        JNEQ,
                        JLT,
                        JLEQ,
                        JGT,
                        JGEQ,
                        LAST_QUATERNARY = JGEQ, 
                        N_OPCODES = LAST_QUATERNARY + 1
    };
};

constexpr const i8* OP_NAMES[] = {
    "trap", "retvoid",
    "nop", "var", "new", "ret", "jump", 
    "phi",
    "neg", "lnot", "bnot", "ld", "la", "cast", "cvt",
    "zxt", "sxt", "arg", "def",
    "lzc", "tzc", "popc",
    "call",
    "add", "sub", "mul", "div", "rem",
    "land", "lor", "lxor", "band", "bor", "bxor", "shl", "shr",
    "mov", "st", "fld", "fla", "ild", "ila",
    "ceq", "cneq", "clt", "cleq", "cgt", "cgeq",
    "jzero", "jnzero",
    "fst", "ist", 
    "jeq", "jneq", "jlt", "jleq", "jgt", "jgeq"
};

static_assert(sizeof(OP_NAMES) / sizeof(i8*) == assembler::N_OPCODES, "OP_NAMES out of sync");

constexpr Arity OP_ARITIES[] = {
    NULLARY, NULLARY,

    UNARY, UNARY, UNARY, UNARY, UNARY,
    VUNARY,

    BINARY, BINARY, BINARY, BINARY, BINARY, BINARY, BINARY,
    BINARY, BINARY, BINARY, BINARY,
    BINARY, BINARY, BINARY,

    VBINARY,

    TERNARY, TERNARY, TERNARY, TERNARY, TERNARY, 
    TERNARY, TERNARY, TERNARY, TERNARY, TERNARY, TERNARY, TERNARY, TERNARY, 
    TERNARY, TERNARY, TERNARY, TERNARY, TERNARY, TERNARY, 
    TERNARY, TERNARY, TERNARY, TERNARY, TERNARY, TERNARY,
    TERNARY, TERNARY,

    QUATERNARY, QUATERNARY,
    QUATERNARY, QUATERNARY, QUATERNARY, QUATERNARY, QUATERNARY, QUATERNARY
};

static_assert(sizeof(OP_ARITIES) / sizeof(Arity) == assembler::N_OPCODES, "OP_ARITIES out of sync");

constexpr OpMeta OP_META[] = {
    UNTYPED, UNTYPED,

    UNTYPED, HAS_OUTPUT, HAS_OUTPUT, RETURN, JUMP, 
    HAS_OUTPUT,

    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, LOAD, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, 
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,

    CALLMETA,

    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    NO_OUTPUT, STORE, LOAD, HAS_OUTPUT, LOAD, HAS_OUTPUT,
    HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT, HAS_OUTPUT,
    BRANCH, BRANCH,

    STORE, STORE,
    BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH
};

static_assert(sizeof(OP_META) / sizeof(OpMeta) == assembler::N_OPCODES, "OP_META out of sync");

enum Param : u8 {
    P_NONE = 0, P_TYPE = 1, P_REG = 2, P_BRANCH = 3, P_INT = 4, P_F32 = 5, P_F64 = 6, P_DATA = 7, P_STATIC = 8, P_FUNC = 9, P_INCOMING = 10
};

enum class BranchFlags : u8 {
    NONE = 0, CONDITIONAL = 1, UNLIKELY = 2
};

union Arg {
    localidx reg;
    i64 imm;
    float f32imm;
    double f64imm;
    typeidx type;
    dataidx data;
    statidx stat;
    labelidx lbl;
    funcidx func;
    stridx str;
    struct {
        localidx dest;
        i32 flags;
    } branch;

    inline Arg() {}
    inline Arg(i64 n): imm(n) {}

    inline bool operator==(const Arg& other) const {
        return imm == other.imm;
    }

    inline bool operator!=(const Arg& other) const {
        return imm != other.imm;
    }
};

struct Function;

struct Insn {
    u32 pidx; // Starting index of parameters.
    u8 op; // Opcode
    u8 nparams; // Number of parameters.

    inline Insn() {}

    inline const_slice<Param> params(const Function&) const;
    inline slice<Param> params(Function&);
    inline const_slice<Arg> args(const Function&) const;
    inline slice<Arg> args(Function&);
    inline typeidx type(const Function&) const;

    inline bool has_output() const {
        return OP_META[op].has_output();
    }

    inline bool is_control() const {
        return OP_META[op].is_control();
    }

    inline bool exits() const {
        return OP_META[op].exits();
    }

    inline bool is_call() const {
        return OP_META[op].is_call();
    }

    inline bool is_memory() const {
        return OP_META[op].is_memory();
    }

    inline typeidx result_type(const TypeTable& tab, const Function& fn) const {
        if (op == assembler::CALL) return tab.types[type(fn)].ret;
        else return type(fn);
    }

    inline localidx jump_target(const Function&) const;
};

using InsnVec = vec<Insn, 8, arena>;

struct JasmineModule;
struct PassInfo;

struct Function {
    JasmineModule* obj;
    typeidx type;
    funcidx idx;
    localidx idCounter = 0;
    bool isextern = false;
    InsnVec insns;
    vec<Param, 16> params;
    vec<Arg, 16> args;
    stridx modname, name;
    localidx entrypoint;

    Function(JasmineModule* obj_in, typeidx type, stridx name);

    inline localidx newLocal() {
        return idCounter ++;
    }

    void write(bytebuf<>& buf) const;
    void read(bytebuf<>& buf);
    void format(fd io, u32 indent = 2) const;
    void formatshort(fd io) const;
    void dumpDOT(fd io) const;
    void dumpDOTPrelude(fd io) const;
    void dumpDOTInsns(fd io) const;
    void dumpDOTEpilogue(fd io) const;
    void dumpDOT(fd io, PassInfo& info) const;
};

inline const_slice<Param> Insn::params(const Function& fn) const {
    return { &fn.params[pidx + 1], nparams };
}

inline slice<Param> Insn::params(Function& fn) {
    return { &fn.params[pidx + 1], nparams };
}

inline const_slice<Arg> Insn::args(const Function& fn) const {
    return { &fn.args[pidx + 1], nparams };
}

inline slice<Arg> Insn::args(Function& fn) {
    return { &fn.args[pidx + 1], nparams };
}

inline typeidx Insn::type(const Function& fn) const {
    return fn.args[pidx].type;
}

void format_insn(const Function& fn, const Insn& insn, fd io, bool showLabel = true);
void format_func(const Function& fn, fd io, u32 indent);
void DOTInsnName(fd io, const Function& fn, const Insn& insn, localidx id);

struct Value {
    Param kind;
    Arg data;

    inline bool operator==(const Value& other) const {
        return kind == other.kind && data == other.data;
    }

    inline bool operator!=(const Value& other) const {
        return kind != other.kind || data != other.data;
    }
};

namespace assembler {
    extern Function* targetfn;

    inline Value Reg(localidx r) {
        Value uarg;
        uarg.kind = P_REG;
        uarg.data.reg = 0;
        uarg.data.reg = r;
        return uarg;
    }

    inline Value Branch(localidx r) {
        Value uarg;
        uarg.kind = P_BRANCH;
        uarg.data.branch.dest = r;
        uarg.data.branch.flags = (i32)BranchFlags::NONE;
        return uarg;
    }

    inline Value Branch() {
        return Branch(-1);
    }

    inline Value IncomingBranch(localidx r) {
        Value uarg;
        uarg.kind = P_INCOMING;
        uarg.data.branch.dest = r;
        uarg.data.branch.flags = (i32)BranchFlags::NONE;
        return uarg;
    }

    inline Value Int(i64 i) {
        Value uarg;
        uarg.kind = P_INT;
        uarg.data.reg = 0;
        uarg.data.imm = i;
        return uarg;
    }

    inline Value F32(float f) {
        Value uarg;
        uarg.kind = P_F32;
        uarg.data.reg = 0;
        uarg.data.f32imm = f;
        return uarg;
    }

    inline Value F64(double f) {
        Value uarg;
        uarg.kind = P_F64;
        uarg.data.reg = 0;
        uarg.data.f64imm = f;
        return uarg;
    }

    inline Value Func(funcidx i) {
        Value uarg;
        uarg.kind = P_FUNC;
        uarg.data.reg = 0;
        uarg.data.func = i;
        return uarg;
    }

    void writeTo(Function& fn);
    void finishFunction();

    extern Function* currentFn;

    inline void appendArgs(Function& fn, Insn& insn) {}

    template<typename... Args>
    inline void appendArgs(Function& fn, Insn& insn, const slice<Value>& arg, const Args&... args) {
        for (const Value& v : arg) {
            fn.params.push(v.kind);
            fn.args.push(v.data);
        }
        appendArgs(fn, insn, args...);
    }

    template<typename... Args>
    inline void appendArgs(Function& fn, Insn& insn, const const_slice<Value>& arg, const Args&... args) {
        for (const Value& v : arg) {
            fn.params.push(v.kind);
            fn.args.push(v.data);
        }
        appendArgs(fn, insn, args...);
    }

    template<typename... Args>
    inline void appendArgs(Function& fn, Insn& insn, const Value& arg, const Args&... args) {
        fn.params.push(arg.kind);
        fn.args.push(arg.data);
        appendArgs(fn, insn, args...);
    }

    inline u32 insnArgsCount() {
        return 0;
    }

    template<typename... Args>
    u32 insnArgsCount(const const_slice<Value>& value, const Args&... args) {
        return value.n + assembler::insnArgsCount(args...);
    }

    template<typename... Args>
    u32 insnArgsCount(const slice<Value>& value, const Args&... args) {
        return value.n + assembler::insnArgsCount(args...);
    }

    template<typename... Args>
    u32 insnArgsCount(const Value& value, const Args&... args) {
        return 1 + assembler::insnArgsCount(args...);
    }

    template<typename... Args>
    inline Insn createInsn(Op opcode, typeidx type, const Args&... args) {
        assert(currentFn != nullptr);
        assert(OP_META[opcode].is_typed());
        Insn insn;
        insn.op = opcode;
        insn.nparams = insnArgsCount(args...);
        insn.pidx = currentFn->params.size();
        currentFn->params.push(P_TYPE);
        currentFn->args.push(Arg(type));
        appendArgs(*currentFn, insn, args...);
        if (!OP_META[insn.op].is_control()) {
            insn.nparams ++;
            currentFn->params.push(P_BRANCH);
            currentFn->args.push(Arg(-1));
        }
        return insn;
    }

    template<typename... Args>
    inline Insn createInsn(Function& fn, Op opcode, typeidx type, const Args&... args) {
        assert(OP_META[opcode].is_typed());
        Insn insn;
        insn.op = opcode;
        insn.nparams = insnArgsCount(args...);
        insn.pidx = fn.params.size();
        fn.params.push(P_TYPE);
        fn.args.push(Arg(type));
        appendArgs(fn, insn, args...);
        if (!OP_META[insn.op].is_control()) {
            insn.nparams ++;
            fn.params.push(P_BRANCH);
            fn.args.push(Arg(-1));
        }
        return insn;
    }

    template<typename... Args>
    inline Insn createInsn(Op opcode, const Args&... args) {
        assert(currentFn != nullptr);
        assert(!OP_META[opcode].is_typed());
        Insn insn;
        insn.op = opcode;
        insn.nparams = insnArgsCount(args...);
        insn.pidx = currentFn->params.size();
        currentFn->params.push(P_TYPE);
        currentFn->args.push(Arg(T_VOID));
        appendArgs(*currentFn, insn, args...);
        if (!OP_META[insn.op].is_control()) {
            insn.nparams ++;
            currentFn->params.push(P_BRANCH);
            currentFn->args.push(Arg(-1));
        }
        return insn;
    }

    template<typename... Args>
    inline Insn createInsn(Function& fn, Op opcode, const Args&... args) {
        assert(!OP_META[opcode].is_typed());
        Insn insn;
        insn.op = opcode;
        insn.nparams = insnArgsCount(args...);
        insn.pidx = fn.params.size();
        fn.params.push(P_TYPE);
        fn.args.push(Arg(T_VOID));
        appendArgs(fn, insn, args...);
        if (!OP_META[insn.op].is_control()) {
            insn.nparams ++;
            fn.params.push(P_BRANCH);
            fn.args.push(Arg(-1));
        }
        return insn;
    }

    inline void link(localidx branch, i32 operand, localidx dest) {
        Insn& insn = currentFn->insns[branch];
        assert(insn.params(*currentFn)[operand] == P_BRANCH);
        insn.args(*currentFn)[operand].branch.dest = dest;
    }

    inline void link(localidx branch, i32 operand) {
        link(branch, operand, currentFn->insns.size());
    }

    template<typename... Args>
    inline localidx add(const Args&... args) {
        localidx id = currentFn->newLocal();

        // Implicit successor operand for non-control instructions
        if (id > 0) {
            Insn& prev = currentFn->insns.back();
            if (!OP_META[prev.op].is_control() && prev.args(*currentFn)[prev.args(*currentFn).n - 1].branch.dest == -1) {
                prev.args(*currentFn)[prev.args(*currentFn).n - 1].branch.dest = id;
            }
        }

        Insn insn = createInsn(args...);
        
        currentFn->insns.push(insn);
        assert(currentFn->insns.size() - 1 == id);
        return id;
    }

    template<typename... Args>
    inline localidx addAfter(localidx after, const Args&... args) {
        localidx id = currentFn->newLocal();

        // Implicit successor operand for non-control instructions
        if (id > 0) {
            Insn& prev = currentFn->insns[after];
            if (!OP_META[prev.op].is_control()) {
                prev.args(*currentFn)[prev.args(*currentFn).n - 1].branch.dest = id;
            }
        }

        Insn insn = createInsn(args...);
        
        currentFn->insns.push(insn);
        assert(currentFn->insns.size() - 1 == id);
        return id;
    }

    inline void subst(localidx from, localidx to, Value src, Value dst) {
        for (localidx i = from; i < to; i ++) {
            Insn& insn = currentFn->insns[i];
            for (i32 j = 0; j < insn.args(*currentFn).n; j ++) {
                if (insn.params(*currentFn)[j] == src.kind && insn.args(*currentFn)[j] == src.data) {
                    insn.params(*currentFn)[j] = dst.kind;
                    insn.args(*currentFn)[j] = dst.data;
                }
            }
        }
    }

    inline void follow(localidx from, localidx to) {
        link(from, currentFn->insns[from].args(*currentFn).n - 1, to);
    }

    inline localidx predecessor() {
        return currentFn->insns.size() - 1;
    }

    inline localidx tip() {
        return currentFn->insns.size();
    }
}

ENDMODULE()

#endif