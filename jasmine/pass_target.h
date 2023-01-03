#ifndef BASIL_JASMINE_PASS_TARGET_H
#define BASIL_JASMINE_PASS_TARGET_H

#include "jasmine/insn.h"
#include "jasmine/pass.h"
#include "jasmine/obj.h"
#include "jasmine/arch.h"

MODULE(jasmine)

template<typename Target>
void regalloc(PassInfo& info, Function& fn) {
    assert(info.didPhase(PassInfo::IRG));

    const Type& type = fn.obj->types.types[fn.type];
    Target::place_call(info.callinfo, fn.obj->types, fn.type);

    RegSet gps, fps;
    for (mreg r : Target::gps()) gps.add(r);
    for (mreg r : Target::fps()) fps.add(r);
    for (GraphNode& node : info.rg) if (node.n != -1) 
        node.gps = gps, node.fps = fps;

    for (i32 i = 0; i < fn.insns.size(); i ++) {
        const Insn& insn = fn.insns[i];
        const auto& params = insn.params(fn);
        const auto& args = insn.args(fn);
        if (insn.op == assembler::ARG) {
            info.rg[i].binding = info.callinfo.param(args[0].imm)[0];
            // TODO: multi-register bindings.
        }
        else if (insn.op == assembler::RET) {
            for (i32 j = 0; j < params.n; j ++) if (params[j] == P_REG)
                info.rg[args[j].reg].hint = info.callinfo.ret()[0];
        }
    }

    for (i32 i = 0; i < info.rg.size(); i ++) if (info.live[i].size()) {
        GraphNode& node = info.rg[i];
        if (node.binding.kind != Binding::NONE) {
            if (node.binding.kind == Binding::GP)
                for (i32 j : node.edges(info)) info.rg[j].gps.remove(node.binding.reg);
            else if (node.binding.kind == Binding::FP)
                for (i32 j : node.edges(info)) info.rg[j].fps.remove(node.binding.reg);
        }
    }

    info.stack = 0;
    for (i32 i = 0; i < info.rg.size(); i ++) if (info.live[i].size()) {
        GraphNode& node = info.rg[i];
        if (node.binding.kind == Binding::NONE) {
            bool isfp = fn.insns[i].type(fn) == T_F32 || fn.insns[i].type(fn) == T_F64;
            RegSet& regs = isfp ? node.fps : node.gps;
            if (regs) {
                if (node.hint.kind == Binding::STACK) {
                    node.binding = node.hint;
                    continue;
                }
                mreg r = regs.next(); 
                if (node.hint.kind == (isfp ? Binding::FP : Binding::GP) && regs[node.hint.reg]) {
                    r = node.hint.reg;
                }
                node.binding = isfp ? Binding::fp(r) : Binding::gp(r);
            }
            else node.binding = Binding::stack(info.stack -= 8);
        }
        if (node.binding.kind != Binding::STACK) {
            assert(node.binding.kind != Binding::NONE);
            
            for (i32 j : node.edges(info)) {
                if (node.binding.kind == Binding::FP) info.rg[j].fps.remove(node.binding.reg);
                else info.rg[j].gps.remove(node.binding.reg);
            }
        }
    }

    // for (localidx i = 0; i < info.rg.size(); i ++) {
    //     if (info.rg[i].n != -1) println("Allocated %", i, " to ", info.rg[i].binding);
    // }
}

template<typename Target>
void preisel(PassInfo& info, Function& fn) {
    //
}

template<typename Target>
ASMVal bindingToASMVal(const Binding& binding) {
    if (binding.kind == Binding::GP) return ASMVal::gp(binding.reg);
    else if (binding.kind == Binding::FP) return ASMVal::fp(binding.reg);
    else return ASMVal::mem(Target::fp, binding.offset);
}

template<typename Target>
ASMVal lower(PassInfo& info, Function& fn, const Value& value) {
    switch (value.kind) {
        case P_REG:
            return bindingToASMVal<Target>(info.rg[value.data.reg].binding);
        case P_INT:
            return ASMVal::imm(value.data.imm);
        case P_F32:
            return ASMVal::f32(value.data.f32imm);
        case P_F64:
            return ASMVal::f64(value.data.f64imm);
        case P_FUNC:
            return ASMVal::func(fn.obj->funcs[value.data.func]->name);
        default:
            fatal("Unsupported operand.");
    }
}

#define VAL(n) Value{params[n], args[n]}

template<typename Target, typename T>
T intOrFloatOp(typeidx type, T int8, T int16, T int32, T int64, T float32, T float64) {
    switch (type) {
        case T_I8:
        case T_U8:
            return int8;
        case T_I16:
        case T_U16:
            return int16;
        case T_I32:
        case T_U32:
            return int32;
        case T_I64:
        case T_U64:
            return int64;
        case T_F32:
            return float32;
        case T_F64:
            return float64;
        case T_IWORD:
        case T_UWORD:
            switch (Target::word_size()) {
                case Size::BITS8: return int8;
                case Size::BITS16: return int16;
                case Size::BITS32: return int32;
                case Size::BITS64: return int64;
                default:
                    fatal("Unexpected word size!");
            }
            break;
        case T_PTR:
        case T_REF:
            switch (Target::ptr_size()) {
                case Size::BITS8: return int8;
                case Size::BITS16: return int16;
                case Size::BITS32: return int32;
                case Size::BITS64: return int64;
                default:
                    fatal("Unexpected word size!");
            }
            break;
        default:
            fatal("Expected primitive int or float type.");
    }
}

template<typename Target>
void lower(PassInfo& info, Function& fn, Assembly& as) {
    vec<ASMVal, 64> argbuf;
    for (localidx i = 0; i < fn.insns.size(); i ++) {
        argbuf.clear();
        const Insn& insn = fn.insns[i];
        const auto& params = insn.params(fn);
        const auto& args = insn.args(fn);
        for (i32 i = 0; i < args.n; i ++) if (params[i] != P_BRANCH && params[i] != P_INCOMING) {
            argbuf.push(lower<Target>(info, fn, VAL(i)));
        }

        using namespace assembler;
        ASMVal result;
        if (info.rg[i].n != -1)
            result = lower<Target>(info, fn, Reg(i));
        typeidx insntype = insn.type(fn);

        switch (insn.op) {
            case ARG:
                break;
            case ADD:
                intOrFloatOp<Target>(insntype, Target::add8, Target::add16, Target::add32, Target::add64, Target::fadd32, Target::fadd64)
                    (as, result, argbuf[0], argbuf[1]);
                break;
            case MOV:
                intOrFloatOp<Target>(insntype, Target::mov8, Target::mov16, Target::mov32, Target::mov64, Target::fmov32, Target::fmov64)
                    (as, argbuf[0], argbuf[1]);
                break;
            case RET: {
                ASMVal dest = bindingToASMVal<Target>(info.callinfo.ret()[0]);
                intOrFloatOp<Target>(insntype, Target::mov8, Target::mov16, Target::mov32, Target::mov64, Target::fmov32, Target::fmov64)
                    (as, dest, argbuf[0]);
                Target::ret(as);
                break;
            }
            default:
                fatal("Unsupported opcode.");
        }
    } 
}

ENDMODULE()

#endif