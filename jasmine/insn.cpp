#include "jasmine/insn.h"
#include "jasmine/obj.h"

MODULE(jasmine)

Function::Function(JasmineModule* obj_in, typeidx type_in, stridx name_in): obj(obj_in), type(type_in), modname(obj->meta.modname), name(name_in) {
    idx = obj->funcs.size();
    obj->funcs.push(this);
}

inline u8 argsof(Param first, Param second) {
    return (u8)second << 4 | (u8)first;
}

inline void unpackArgs(u8 packed, Param& first, Param& second) {
    first = Param(packed & 0b1111);
    second = Param(packed >> 4 & 0b1111);
}

void Function::write(bytebuf<>& buf) const {
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
            if (OP_META[insn.op].is_typed()) // Write type only if necessary.
                buf.writeLEB(insn.type(*this));
            switch (OP_ARITIES[insn.op]) {
                case UNARY:
                    buf.write<u8>(argsof(params[0], P_NONE));
                    buf.writeLEB(args[0].reg);
                    break;
                case BINARY:
                    buf.write<u8>(argsof(params[0], params[1]));
                    buf.writeLEB(args[0].reg);
                    buf.writeLEB(args[1].reg);
                    break;
                case VBINARY:
                    buf.write<u8>(argsof(params[0], params[1]));
                    buf.writeLEB(args[0].reg);
                    buf.writeLEB(args[1].reg);
                    buf.writeULEB(args.size() - 2);
                    for (u32 i = 2; i < params.n; i += 2) {
                        Param a = params[i];
                        Param b = i + 1 < params.n ? params[i + 1] : P_NONE;
                        buf.write<u8>(argsof(a, b));
                    }
                    for (u32 i = 2; i < args.n; i ++) buf.writeLEB(args[i].reg);
                    break;
                case TERNARY:
                    buf.write<u8>(argsof(params[0], params[1]));
                    buf.write<u8>(argsof(params[2], P_NONE));
                    buf.writeLEB(args[0].reg);
                    buf.writeLEB(args[1].reg);
                    buf.writeLEB(args[2].reg);
                    break;
                case QUATERNARY:
                    buf.write<u8>(argsof(params[0], params[1]));
                    buf.write<u8>(argsof(params[2], params[3]));
                    buf.writeLEB(args[0].reg);
                    buf.writeLEB(args[1].reg);
                    buf.writeLEB(args[2].reg);
                    buf.writeLEB(args[3].reg);
                    break;
            }
        }
    }
}

void Function::read(bytebuf<>& buf) {
    name = buf.readLEB();
    type = buf.readLEB();
    isextern = buf.read<u8>();
    if (isextern) modname = buf.readLEB();
    else {
        u64 n_insns = buf.readULEB();
        u8 argbits;

        for (u64 i = 0; i < n_insns; i ++) {
            Insn insn;
            Param first, second;
            insn.op = (assembler::Op)buf.read<u8>();
            if (OP_META[insn.op].is_typed()) // Read type only if necessary.
                params.push(P_TYPE), args.push(Arg(buf.readLEB()));
            insn.pidx = params.size();
            switch (OP_ARITIES[insn.op]) {
                case UNARY:
                    insn.nparams = 1;
                    unpackArgs(buf.read<u8>(), first, second);
                    params.push(first);
                    args.push(buf.readLEB());
                    break;
                case BINARY:
                    unpackArgs(buf.read<u8>(), first, second);
                    params.push(first);
                    params.push(second);
                    insn.nparams = 2;
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    break;
                case VBINARY:
                    unpackArgs(buf.read<u8>(), first, second);
                    params.push(first);
                    params.push(second);
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    insn.nparams = 2 + buf.readULEB();
                    for (u64 i = 2; i < insn.nparams; i += 2) {
                        unpackArgs(buf.read<u8>(), first, second);
                        params.push(first);
                        if (i + 1 < insn.nparams) params.push(second);
                    }
                    for (u64 i = 2; i < insn.nparams; i ++) args.push(buf.readLEB());
                    break;
                case TERNARY:
                    unpackArgs(buf.read<u8>(), first, second);
                    params.push(first);
                    params.push(second);
                    unpackArgs(buf.read<u8>(), first, second);
                    params.push(first);
                    insn.nparams = 3;
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    break;
                case QUATERNARY:
                    unpackArgs(buf.read<u8>(), first, second);
                    params.push(first);
                    params.push(second);
                    unpackArgs(buf.read<u8>(), first, second);
                    params.push(first);
                    params.push(second);
                    insn.nparams = 4;
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    args.push(buf.readLEB());
                    break;
            }
            insns.push(insn);
        }
    }
}

void format_insn(const Function& fn, const Insn& insn, fd io, bool showLabel) {
    u32 idx = &insn - fn.insns.begin();
    auto params = insn.params(fn);
    auto args = insn.args(fn);
    i64 id = &insn - &fn.insns[0];
    if (showLabel && id >= 0 && id < 0x10000000)
        write(io, id, ": ");
    write(io, OP_NAMES[insn.op], ' ');
    if (OP_META[insn.op].is_typed()) format_type(fn.obj->types, insn.args(fn)[-1].type, io), write(io, ' ');
    const JasmineModule& obj = *fn.obj;
    auto& strings = obj.strings;
    bool first = true;
    i32 varct = -1;
    for (u32 i = 0; i < params.n; i ++) {
        if (insn.op == assembler::CALL && i == 1) write(io, "(");
        else if (insn.op == assembler::CALL && i == params.n - 1) write(io, "), ");
        else if (params[i] != P_NONE && !first) write(io, ", ");
        first = false;
        switch (params[i]) {
            case P_REG: write(io, '%', args[i].reg); break;
            case P_BRANCH: 
                if (args[i].branch.dest == -1)
                    write(io, "UNLINKED!");
                else
                    write(io, "-> ", args[i].branch.dest); 
                break;
            case P_INT: write(io, '#', args[i].imm); break;
            case P_F32: write(io, '#', args[i].f32imm); break;
            case P_F64: write(io, '#', args[i].f64imm); break;
            case P_DATA: write(io, "data ", args[i].imm); break;
            case P_STATIC: write(io, "data ", args[i].imm); break;
            case P_FUNC: write(io, strings.str(obj.funcs[args[i].func]->name)); break;
            default: break;
        }
    }
}

void DOTInsnName(fd io, const Function& fn, const Insn& insn, localidx id) {
    write(io, '"');
    format_insn(fn, insn, io, true);
    write(io, '"');
}

void Function::dumpDOT(fd io) const {
    ::write(io, "digraph \"", obj->strings.str(modname), ":", obj->strings.str(name), "\" {\n");
    ::write(io, "  label=\"", obj->strings.str(modname), ":", obj->strings.str(name), "\";\n");
    ::write(io, "  labelloc=\"t\";\n");
    ::write(io, "  fontsize=24;\n");
    if (insns.size()) {
        ::write(io, "  entry -> ");
        DOTInsnName(io, *this, insns[0], 0);
        ::write(io, '\n');
    }
    for (localidx i = 0; i < insns.size(); i ++) {
        const Insn& insn = insns[i];
        for (i32 j = 0; j < insn.nparams; j ++) {
            if (insn.params(*this)[j] == P_REG) {
                localidx ref = insn.args(*this)[j].reg;
                ::write(io, "  ");
                DOTInsnName(io, *this, insns[ref], ref);
                ::write(io, " -> ");
                DOTInsnName(io, *this, insn, i);
                ::write(io, " [color=red]\n");
            }
            else if (insn.params(*this)[j] == P_BRANCH) {
                localidx ref = insn.args(*this)[j].branch.dest;
                if (ref == -1) continue; // Ignore unlinked branches.
                ::write(io, "  ");
                DOTInsnName(io, *this, insn, i);
                ::write(io, " -> ");
                DOTInsnName(io, *this, insns[ref], ref);
                ::write(io, '\n');
            }
            if (insn.op == assembler::RET) {
                ::write(io, "  ");
                DOTInsnName(io, *this, insn, i);
                ::write(io, " -> exit\n");
            }
        }
    }
    ::write(io, "}\n");
}

void Function::format(fd io, u32 indent) const {
    format_func(*this, io, indent);
}

void Function::formatshort(fd io) const {
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

void format_func(const Function& fn, fd io, u32 indent) {
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

namespace assembler {
    vec<Function*> fnStack;
    bool inited = false;
    Function* currentFn;

    void writeTo(Function& fn) {
        if (!inited) {
            new(&fnStack) vec<Function*>();
            inited = true;
        }
        fnStack.push(&fn);
        currentFn = fnStack.back();
    }

    void finishFunction() {
        if (fnStack.size()) fnStack.pop();

        if (fnStack.size()) currentFn = fnStack.back();
        else currentFn = nullptr;
    }
}

ENDMODULE()