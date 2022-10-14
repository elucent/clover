#include "jasmine/insn.h"
#include "jasmine/obj.h"

Function::Function(JasmineModule* obj_in): obj(obj_in) {
    insns.alloc = &obj->modspace;
    params.alloc = &obj->modspace;
    args.alloc = &obj->modspace;
    labels.alloc = &obj->modspace;
}

void Function::write(bytebuf<arena>& buf) const {
    buf.writeLEB(name);
    buf.writeLEB(type);
    buf.write<u8>(isextern ? 1 : 0);
    if (isextern) buf.writeLEB(modname);
    else {
        buf.writeULEB(insns.size());

        for (const auto& insn : insns) {
            auto params = insn.params(*this);
            auto args = insn.args(*this);
            buf.write<u8>(insn.op);
            buf.writeLEB(insn.type);
            switch (OP_ARITIES[insn.op]) {
                case NULLARY: break;
                case UNARY:
                    buf.write<u8>(argsof(params[0], P_NONE));
                    buf.writeLEB(args[0].reg);
                    break;
                case VUNARY:
                    buf.write<u8>(argsof(P_VAR, P_NONE));
                    buf.writeULEB(args.size());
                    for (u32 i = 0; i < params.size(); i += 2) {
                        Param a = params[i];
                        Param b = i + 1 < params.size() ? params[i + 1] : P_NONE;
                        buf.write<u8>(argsof(a, b));
                    }
                    for (const auto& arg : args) buf.writeLEB(arg.reg);
                    break;
                case BINARY:
                    buf.write<u8>(argsof(params[0], params[1]));
                    buf.writeLEB(args[0].reg);
                    buf.writeLEB(args[1].reg);
                    break;
                case VBINARY:
                    buf.write<u8>(argsof(params[0], P_VAR));
                    buf.writeLEB(args[0].reg);
                    buf.writeULEB(args.size() - 1);
                    for (u32 i = 1; i < params.n; i += 2) {
                        Param a = params[i];
                        Param b = i + 1 < params.n ? params[i + 1] : P_NONE;
                        buf.write<u8>(argsof(a, b));
                    }
                    for (u32 i = 1; i < args.n; i ++) buf.writeLEB(args[i].reg);
                    break;
                case TERNARY:
                    buf.write<u8>(argsof(params[0], params[1]));
                    buf.write<u8>(argsof(P_NONE, params[2]));
                    buf.writeLEB(args[0].reg);
                    buf.writeLEB(args[1].reg);
                    buf.writeLEB(args[2].reg);
                    break;
            }
        }
    }
}

void Function::read(bytebuf<arena>& buf) {
    name = buf.readLEB();
    type = buf.readLEB();
    isextern = buf.read<u8>();
    if (isextern) modname = buf.readLEB();
    else {
        u64 n_insns = buf.readULEB();
        u8 argbits;

        for (u64 i = 0; i < n_insns; i ++) {
            Insn insn;
            insn.op = (Op)buf.read<u8>();
            insn.type = buf.readLEB();
            insn.pidx = params.size();
            switch (OP_ARITIES[insn.op]) {
                case NULLARY: 
                    insn.nparams = 0;
                    break;
                case UNARY:
                    insn.nparams = 1;
                    params.push(Param(buf.read<u8>() >> 4 & 15));
                    args.push(buf.readLEB());
                    if (insn.op == OP_LABEL) {
                        while (labels.size() <= args.back().lbl) labels.push(-1);
                        labels[args.back().lbl] = i;
                        print("! set label ", args.back().lbl, " to ", i, '\n');
                    }
                    break;
                case VUNARY:
                    buf.read<u8>();
                    insn.nparams = buf.readULEB();
                    for (u64 i = 0; i < insn.nparams; i += 2) {
                        u8 argbits = buf.read<u8>();
                        params.push(Param(argbits >> 4 & 15));
                        if (i + 1 < insn.nparams) params.push(Param(argbits & 15));
                    }
                    for (u64 i = 0; i < insn.nparams; i ++) args.push(buf.readLEB());
                    break;
                case BINARY:
                    argbits = buf.read<u8>();
                    params.push(Param(argbits >> 4 & 15));
                    params.push(Param(argbits & 15));
                    insn.nparams = 2;
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    break;
                case VBINARY:
                    argbits = buf.read<u8>();
                    params.push(Param(argbits >> 4 & 15));
                    args.push(buf.readLEB());
                    insn.nparams = 1 + buf.readULEB();
                    for (u64 i = 1; i < insn.nparams; i += 2) {
                        u8 argbits = buf.read<u8>();
                        params.push(Param(argbits >> 4 & 15));
                        if (i + 1 < insn.nparams) params.push(Param(argbits & 15));
                    }
                    for (u64 i = 1; i < insn.nparams; i ++) args.push(buf.readLEB());
                    break;
                case TERNARY:
                    argbits = buf.read<u8>();
                    params.push(Param(argbits >> 4 & 15));
                    params.push(Param(argbits & 15));
                    params.push(Param(buf.read<u8>() & 15)); // Third param.
                    insn.nparams = 3;
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    break;
            }
            insns.push(insn);
        }
    }
}

void format_insn(const Function& fn, const Insn& insn, stream& io) {
    u32 idx = &insn - fn.insns.begin();
    auto params = insn.params(fn);
    auto args = insn.args(fn);
    if (insn.op == OP_LABEL) write(io, '.', args[0].lbl, ':');
    else {
        write(io, "    ");
        if (OP_META[insn.op].hasoutput) write(io, "%", idx, " = "); 
        write(io, OP_NAMES[insn.op], ' ');
        if (OP_META[insn.op].hastype) format_type(fn.obj->types, insn.type, io), write(io, ' ');
        const JasmineModule& obj = *fn.obj;
        bool first = true;
        i32 varct = -1;
        for (u32 i = 0; i < params.n; i ++) {
            if (params[i] != P_NONE && params[i] != P_VAR && !first) write(io, ", ");
            first = false;
            switch (params[i]) {
                case P_REG: write(io, '%', args[i].reg); break;
                case P_IMM: write(io, args[i].imm); break;
                case P_DATA: write(io, args[i].imm); break;
                case P_STATIC: write(io, args[i].imm); break;
                case P_FUNC: write(io, obj.strings.strings[obj.funcs[args[i].func]->name]); break;
                case P_LABEL: write(io, '.', args[i].lbl); break;
                case P_VAR: write(io, '('), varct = insn.nparams - 1, first = true; break;
                default: break;
            }
            if (varct > 0) {
                varct --;
                if (varct == 0) write(io, ')');
            }
        }
    }
}

void Function::format(stream& io, u32 indent) const {
    format_func(*this, io, indent);
}

void Function::formatshort(stream& io) const {
    const Type& ut = obj->types.types[type];
    if (ut.kind != TK_FUN) fatal("Invalid type for function.");
    format_type(obj->types, ut.ret, io);
    ::write(io, ' ', obj->strings.strings[name], '(');
    bool first = true;
    for (typeidx arg : ut.members) {
        if (!first) ::write(io, ", ");
        first = false;
        format_type(obj->types, arg, io);
    }
    ::write(io, ")");
}

void format_func(const Function& fn, stream& io, u32 indent) {
    const Type& ut = fn.obj->types.types[fn.type];
    if (ut.kind != TK_FUN) fatal("Invalid type for function.");
    for (u32 i = 0; i < indent; i ++) write(io, ' ');
    format_type(fn.obj->types, ut.ret, io);
    write(io, ' ', fn.obj->strings.strings[fn.name], '(');
    bool first = true;
    for (typeidx arg : ut.members) {
        if (!first) write(io, ", ");
        first = false;
        format_type(fn.obj->types, arg, io);
    }
    write(io, ") {\n");
    for (const Insn& insn : fn.insns) {
        for (u32 i = 0; i < indent; i ++) write(io, ' ');
        format_insn(fn, insn, io), write(io, '\n');
    }
    for (u32 i = 0; i < indent; i ++) write(io, ' ');
    write(io, "}\n");
}

namespace jasm {
    Function* targetfn = nullptr;
    static u32 nargs = 0;
    static vec<ParamArg, 64, arena> args;
    static typeidx callt, phit;
    
    funcidx externfunc(JasmineModule& obj, const_slice<i8> name, typeidx type) {
        funcidx i = obj.funcs.size();
        Function* fn = new(obj.modspace) Function(&obj);
        fn->type = type;
        fn->name = obj.strings.intern(name);
        fn->isextern = true;
        obj.funcs.push(fn);
        obj.funcmap.put(obj.strings.strings[fn->name], obj.funcs.back());
        return i;
    }

    funcidx beginfunc(JasmineModule& obj, const_slice<i8> name, typeidx type) {
        funcidx i = obj.funcs.size();
        Function* fn = new(obj.modspace) Function(&obj);
        fn->type = type;
        fn->name = obj.strings.intern(name);
        fn->isextern = false;
        obj.funcs.push(fn);
        obj.funcmap.put(obj.strings.strings[fn->name], obj.funcs.back());
        targetfn = obj.funcs[i];
        args.alloc = &targetfn->obj->modspace;
        return i;
    }

    void endfunc() {
        targetfn = nullptr;
    }
    
    localidx nullary(Op op, typeidx t) {
        localidx i = targetfn->insns.size();
        Insn insn;
        insn.op = op;
        insn.pidx = targetfn->args.size();
        insn.nparams = 0;
        insn.type = t;
        targetfn->insns.push(insn);
        return i;
    }

    localidx unary(Op op, typeidx t, const ParamArg& param) {
        localidx i = targetfn->insns.size();
        Insn insn;
        insn.op = op;
        insn.pidx = targetfn->args.size();
        insn.nparams = 1;
        insn.type = t;
        targetfn->params.push(param.p);
        targetfn->args.push(param.a);
        targetfn->insns.push(insn);
        return i;
    }

    localidx binary(Op op, typeidx t, const ParamArg& lhs, const ParamArg& rhs) {
        localidx i = targetfn->insns.size();
        Insn insn;
        insn.op = op;
        insn.pidx = targetfn->args.size();
        insn.nparams = 2;
        insn.type = t;
        targetfn->params.push(lhs.p);
        targetfn->args.push(lhs.a);
        targetfn->params.push(rhs.p);
        targetfn->args.push(rhs.a);
        targetfn->insns.push(insn);
        return i;
    }
    
    localidx add(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_ADD, t, lhs, rhs); }
    localidx sub(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_SUB, t, lhs, rhs); }
    localidx mul(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_MUL, t, lhs, rhs); }
    localidx div(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_DIV, t, lhs, rhs); }
    localidx rem(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_REM, t, lhs, rhs); }
    localidx neg(typeidx t, const ParamArg& param) { return unary(OP_NEG, t, param); }

    localidx band(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_AND, t, lhs, rhs); }
    localidx bor(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_OR, t, lhs, rhs); }
    localidx bxor(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_XOR, t, lhs, rhs); }
    localidx shr(typeidx t, const ParamArg& param) { return unary(OP_NOT, t, param); }
    localidx bnot(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_SHL, t, lhs, rhs); }
    localidx shl(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_SHR, t, lhs, rhs); }

    void store(typeidx t, const ParamArg& addr, const ParamArg& val) { binary(OP_STORE, t, addr, val); }
    localidx mnew(typeidx t) { return nullary(OP_NEW, t); }

    localidx cast(typeidx t, const ParamArg& param) { return unary(OP_CAST, t, param); }
    localidx conv(typeidx t, const ParamArg& param) { return unary(OP_CONV, t, param); }
    localidx zxt(typeidx t, const ParamArg& param) { return unary(OP_ZXT, t, param); }
    localidx sxt(typeidx t, const ParamArg& param) { return unary(OP_SXT, t, param); }
    localidx mov(typeidx t, const ParamArg& param) { return unary(OP_MOV, t, param); }

    localidx beginphi(typeidx t, const ParamArg& param) {
        nargs = 0;
        args.clear();
        args.push(param);
        phit = t;
        return targetfn->insns.size();
    }

    void phiarg(const ParamArg& param) {
        args.push(param);
    }

    void endphi() {
        Insn insn;
        insn.nparams = args.size();
        insn.pidx = targetfn->params.size();
        insn.op = OP_PHI;
        insn.type = callt;
        for (const ParamArg& pa : args) targetfn->args.push(pa.a), targetfn->params.push(pa.p);
        targetfn->insns.push(insn);
    }

    localidx begincall(typeidx t, const ParamArg& param) {
        nargs = 0;
        args.clear();
        args.push(param);
        callt = t;
        return targetfn->insns.size();
    }

    void callarg(const ParamArg& param) {
        args.push(param);
    }

    void endcall() {
        Insn insn;
        insn.nparams = args.size();
        insn.pidx = targetfn->params.size();
        insn.op = OP_CALL;
        insn.type = callt;
        for (const ParamArg& pa : args) targetfn->args.push(pa.a), targetfn->params.push(pa.p);
        targetfn->insns.push(insn);
    }

    void ret(typeidx t, const ParamArg& param) { unary(OP_RET, t, param); }
    localidx arg(typeidx t, i64 idx) { return unary(OP_ARG, t, imm(idx)); }
    localidx jump(const ParamArg& param) { return unary(OP_JUMP, T_PTR, param); }
    void label(labelidx l) { targetfn->labels[l] = targetfn->insns.size(), unary(OP_LABEL, T_PTR, lbl(l)); }
    localidx phi(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_PHI, t, lhs, rhs); }

    localidx eq(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_EQ, t, lhs, rhs); }
    localidx neq(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_NEQ, t, lhs, rhs); }
    localidx gt(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_LT, t, lhs, rhs); }
    localidx geq(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_LEQ, t, lhs, rhs); }
    localidx lt(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_GT, t, lhs, rhs); }
    localidx leq(typeidx t, const ParamArg& lhs, const ParamArg& rhs) { return binary(OP_GEQ, t, lhs, rhs); }

    void jz(const ParamArg& dest, const ParamArg& cond) { binary(OP_JZ, T_I8, dest, cond); }
    void jnz(const ParamArg& dest, const ParamArg& cond) { binary(OP_JNZ, T_I8, dest, cond); }

    localidx var(typeidx t) { return nullary(OP_VAR, t); }
}