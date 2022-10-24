#include "jasmine/arch.h"
#include "jasmine/insn.h"
#include "jasmine/type.h"
#ifndef BASIL_JASMINE_PASS_TARGET_H
#include "jasmine/pass.h"

// Compute interference graph.
template<typename Target>
void irg(Function& fn, PassInfo& info) {
    info.rg.clear();
    vec<vec<localidx, 4>, 64> ifer;
    set<pair<localidx, localidx>, 64> edges;

    for (i32 i = 0; i < fn.insns.size(); i ++) {
        info.rg.push({});
        ifer.push({});
    }
    
    for (i32 r = 0; r < info.live.size(); r ++) {
        for (const auto& iv : info.live[r]) for (i32 i = iv.start; i < iv.end; i ++) 
            ifer[i].push(r);
    }

    for (i32 i = 0; i < ifer.size(); i ++) {
        const Insn& insn = fn.insns[i];
        RegSet regs = Target::clobbers(insn);
        if (regs) {
            while (mreg r = regs.next()) {
                info.rg.push({});
                info.rg.back().binding = Target::is_fpreg(r) ? Binding::fpreg(r) : Binding::gpreg(r);
                regs.remove(r);
            }
        }
    }

    for (i32 i = 0; i < ifer.size(); i ++) for (i32 j = 0; j < ifer[i].size(); j ++)
        if (i != ifer[i][j]) edges.insert({min(i, ifer[i][j]), max(i, ifer[i][j])});

    for (const auto& e : edges) {
        info.rg[e.first].edges.push(e.second);
        info.rg[e.second].edges.push(e.first);
    }

    // for (const auto& e : edges) {
    //     println("%", e.first, " -> % ", e.second);
    // }
}

// Allocate registers.
template<typename Target>
void regalloc(Function& fn, PassInfo& info) {
    live(fn, info);
    irg<Target>(fn, info);

    const Type& type = fn.obj->types.types[fn.type];
    Target::place_call(info.callinfo, fn.obj->types, fn.type);

    RegSet gpregs, fpregs;
    for (mreg r : Target::gpregs()) gpregs.add(r);
    for (mreg r : Target::fpregs()) fpregs.add(r);
    for (GraphNode& node : info.rg) node.gpregs = gpregs, node.fpregs = fpregs;

    for (i32 i = 0; i < fn.insns.size(); i ++) {
        const Insn& insn = fn.insns[i];
        const auto& params = insn.params(fn);
        const auto& args = insn.args(fn);
        if (insn.op == OP_ARG) {
            info.rg[i].binding = info.callinfo.param(args[0].imm)[0];
        }
        else if (insn.op == OP_RET) {
            for (i32 j = 0; j < params.n; j ++) if (params[j] == P_REG)
                info.rg[args[j].reg].hint = info.callinfo.ret()[0];
        }
    }

    i64 stack = 0;
    for (i32 i = 0; i < info.rg.size(); i ++) if (info.live[i].size()) {
        GraphNode& node = info.rg[i];
        if (node.binding.kind != Binding::NONE) {
            if (node.binding.kind == Binding::GPREG)
                for (i32 j : node.edges) info.rg[j].gpregs.remove(node.binding.reg);
            else if (node.binding.kind == Binding::FPREG)
                for (i32 j : node.edges) info.rg[j].fpregs.remove(node.binding.reg);
        }
    }
    for (i32 i = 0; i < info.rg.size(); i ++) if (info.live[i].size()) {
        GraphNode& node = info.rg[i];
        if (node.binding.kind == Binding::NONE) {
            if (node.gpregs) {
                mreg r = node.gpregs.next();
                if (node.hint.kind == Binding::STACK) {
                    node.binding = node.hint;
                    continue;
                }
                else if (node.hint.kind == Binding::GPREG && node.gpregs[node.hint.reg]) {
                    r = node.hint.reg;
                }
                node.binding = Binding::gpreg(r);
                for (i32 j : node.edges) info.rg[j].gpregs.remove(r);
            }
            else node.binding = Binding::stack(stack -= 8);
        }
    }

    // for (i32 i = 0; i < info.live.size(); i ++) if (info.live[i].size()) {
    //     Binding b = info.rg[i].binding;
    //     println("Allocated local %", i, " to ", b);
    // }
}

// template<typename Target>
// void stackalloc(Function& fn, PassInfo& info) {
//     const TypeTable& tab = fn.obj->types;
//     typeidx ret = tab.types[fn.type].ret;
//     UsageState usage(0, 0, 0);
//     Placement retpos = Target::place_ret(tab, ret);
//     usage += retpos.usage;
//     for (localidx l = 0; l < fn.insns.size(); l ++) {
//         const Insn& i = fn.insns[l];
//         if (i.op == OP_ARG) {
//             Placement p = Target::place_arg(tab, i.type, usage);
//             usage += p.usage;
//             info.slots.push(p.slot);
//         }
//         else if (i.has_output()) {
//             typeidx t = i.result_type(tab);
//             info.stack += tab.native_sizeof<Target>(t);
//             info.slots.push(stack_slot(-info.stack));
//         }
//         else info.slots.push(empty_slot());
//     }
// }

template<typename Target>
MVal to_mval(const Binding& binding) {
    if (binding.kind == Binding::GPREG)
        return MVal::gpreg(binding.reg);
    else if (binding.kind == Binding::FPREG)
        return MVal::fpreg(binding.reg);
    else
        return MVal::mem(Target::fp, binding.offset);
}

template<typename Target>
MVal to_mval(Function& fn, PassInfo& info, typeidx type, Param param, const Arg& arg) {
    switch (param) {
        case P_IMM: switch (type) {
            case T_I8:
            case T_I16:
            case T_I32:
            case T_I64:   
            case T_IWORD:
                assert(arg.imm >= -0x80000000l && arg.imm <= 0x7fffffffl);
                return MVal::imm(arg.imm);
            case T_U8:
            case T_U16:
            case T_U32:
            case T_U64:   
            case T_UWORD:
            case T_PTR:
            case T_REF:
                assert(arg.imm >= 0l && arg.imm <= 0xffffffffl);
                return MVal::imm(arg.imm);
            case T_F32:
                return MVal::f32(arg.f32imm);
            case T_F64:
                return MVal::f64(arg.f64imm);
            default:
                unreachable("Shouldn't have void mvals.");
        }
        case P_REG: {
            const Binding& binding = info.rg[arg.reg].binding;
            return to_mval<Target>(binding);
        }
        case P_LABEL:
            return MVal::label(fn.labels[arg.lbl].name);
        case P_FUNC:
            return MVal::func(fn.name);
        case P_STATIC:
            return MVal::stat(arg.stat);
        case P_DATA:
            return MVal::stat(arg.data);
        case P_NONE:
            unreachable("Shouldn't be processing nones.");
            return MVal();
        case P_VAR:
            unreachable("Shouldn't be processing variadic params at this stage.");
            return MVal();
    }
}

template<typename Target>
Size to_msize(typeidx t) {
    if (t >= 0) return Size::MEMORY;
    switch (-t) {
        case -T_I8:
        case -T_U8:
            return Size::BITS8;
        case -T_I16:
        case -T_U16:
            return Size::BITS16;
        case -T_I32:
        case -T_U32:
            return Size::BITS32;
        case -T_I64:
        case -T_U64:
            return Size::BITS64;
        case -T_F32:
            return Size::FLOAT32;
        case -T_F64:
            return Size::FLOAT64;
        case -T_PTR:
        case -T_REF:
            return Target::ptr_size();
        case -T_IWORD:
        case -T_UWORD:
            return Target::word_size();
        default:
            unreachable("Shouldn't be any other kind.");
            return Size::OTHER;
    }
}

#define SIZED_NULLARY(opcode) template<typename Target> \
void opcode(Assembly& as, Size size) {\
    switch (size) { \
    case Size::BITS8: Target::opcode ## 8 (as); break; \
    case Size::BITS16: Target::opcode ## 16 (as); break; \
    case Size::BITS32: Target::opcode ## 32 (as); break; \
    case Size::BITS64: Target::opcode ## 64 (as); break; \
    case Size::FLOAT32: Target::f ## opcode ## 32 (as); break; \
    case Size::FLOAT64: Target::f ## opcode ## 64 (as); break; \
    default: unreachable("Should only be using this function for word-size stuff."); \
    } \
}

#define SIZED_UNARY(opcode) template<typename Target> \
void opcode(Assembly& as, Size size, MVal dst) {\
    switch (size) { \
    case Size::BITS8: Target::opcode ## 8 (as, dst); break; \
    case Size::BITS16: Target::opcode ## 16 (as, dst); break; \
    case Size::BITS32: Target::opcode ## 32 (as, dst); break; \
    case Size::BITS64: Target::opcode ## 64 (as, dst); break; \
    case Size::FLOAT32: Target::f ## opcode ## 32 (as, dst); break; \
    case Size::FLOAT64: Target::f ## opcode ## 64 (as, dst); break; \
    default: unreachable("Should only be using this function for word-size stuff."); \
    } \
}

#define SIZED_BINARY(opcode) template<typename Target> \
void opcode(Assembly& as, Size size, MVal dst, MVal src) {\
    switch (size) { \
    case Size::BITS8: Target::opcode ## 8 (as, dst, src); break; \
    case Size::BITS16: Target::opcode ## 16 (as, dst, src); break; \
    case Size::BITS32: Target::opcode ## 32 (as, dst, src); break; \
    case Size::BITS64: Target::opcode ## 64 (as, dst, src); break; \
    case Size::FLOAT32: Target::f ## opcode ## 32 (as, dst, src); break; \
    case Size::FLOAT64: Target::f ## opcode ## 64 (as, dst, src); break; \
    default: unreachable("Should only be using this function for word-size stuff."); \
    } \
}

#define SIZED_TERNARY(opcode) template<typename Target> \
void opcode(Assembly& as, Size size, MVal dst, MVal a, MVal b) {\
    switch (size) { \
    case Size::BITS8: Target::opcode ## 8 (as, dst, a, b); break; \
    case Size::BITS16: Target::opcode ## 16 (as, dst, a, b); break; \
    case Size::BITS32: Target::opcode ## 32 (as, dst, a, b); break; \
    case Size::BITS64: Target::opcode ## 64 (as, dst, a, b); break; \
    case Size::FLOAT32: Target::f ## opcode ## 32 (as, dst, a, b); break; \
    case Size::FLOAT64: Target::f ## opcode ## 64 (as, dst, a, b); break; \
    default: unreachable("Should only be using this function for word-size stuff."); \
    } \
}

#define INT_TERNARY(opcode) template<typename Target> \
void opcode(Assembly& as, Size size, MVal dst, MVal a, MVal b) {\
    switch (size) { \
    case Size::BITS8: Target::opcode ## 8 (as, dst, a, b); break; \
    case Size::BITS16: Target::opcode ## 16 (as, dst, a, b); break; \
    case Size::BITS32: Target::opcode ## 32 (as, dst, a, b); break; \
    case Size::BITS64: Target::opcode ## 64 (as, dst, a, b); break; \
    default: unreachable("Invalid size."); \
    } \
}

#define FLOAT_TERNARY(opcode) template<typename Target> \
void opcode(Assembly& as, Size size, MVal dst, MVal a, MVal b) {\
    switch (size) { \
    case Size::FLOAT32: Target::f ## opcode ## 32 (as, dst, a, b); break; \
    case Size::FLOAT64: Target::f ## opcode ## 64 (as, dst, a, b); break; \
    default: unreachable("Invalid size."); \
    } \
}

template<typename Target>
void ccc(Assembly& as, Size size, Condition cond, MVal dst, MVal a, MVal b) {
    switch (size) {
    case Size::BITS8: Target::ccc8(as, cond, dst, a, b); break;
    case Size::BITS16: Target::ccc16(as, cond, dst, a, b); break;
    case Size::BITS32: Target::ccc32(as, cond, dst, a, b); break;
    case Size::BITS64: Target::ccc64(as, cond, dst, a, b); break;
    default: unreachable("Invalid size.");
    }
}

template<typename Target>
void fccc(Assembly& as, Size size, FloatCondition cond, MVal dst, MVal a, MVal b) {
    switch (size) {
    case Size::FLOAT32: Target::fccc32(as, cond, dst, a, b); break;
    case Size::FLOAT64: Target::fccc64(as, cond, dst, a, b); break;
    default: unreachable("Invalid size.");
    }
}

SIZED_BINARY(mov);
SIZED_TERNARY(add);
SIZED_TERNARY(sub);
SIZED_TERNARY(mul);
INT_TERNARY(divi);
INT_TERNARY(divu);
INT_TERNARY(remi);
INT_TERNARY(remu);
FLOAT_TERNARY(div);
FLOAT_TERNARY(rem);
SIZED_TERNARY(neg);
SIZED_TERNARY(push);
SIZED_TERNARY(pop);

template<typename Target>
void lower(Function& fn, PassInfo& info, Assembly& as) {
    MVal vals[3];
    Target::global(as, fn.name);
    for (i32 i = 0; i < fn.insns.size(); i ++) {
        const auto& insn = fn.insns[i];
        const auto& params = insn.params(fn);
        const auto& args = insn.args(fn);
        Size size = to_msize<Target>(insn.type); 
        if (insn.has_output())
            vals[0] = to_mval<Target>(fn, info, insn.type, P_REG, Arg(i));
        if (insn.op > OP_LAST_NULLARY)
            vals[1] = to_mval<Target>(fn, info, insn.type, params[0], args[0]);
        if (insn.op > OP_LAST_UNARY)
            vals[2] = to_mval<Target>(fn, info, insn.type, params[1], args[1]);
        switch (insn.op) {
            case OP_NOP:
            case OP_VAR:
                break;
            case OP_ARG: {
                const_slice<Binding> b = info.callinfo.param(vals[1].payload.imm);
                assert(b.n == 1);
                mov<Target>(as, size, vals[0], to_mval<Target>(b[0]));
                break;
            }
            case OP_RET: {
                const_slice<Binding> b = info.callinfo.ret();
                assert(b.n == 1);
                mov<Target>(as, size, vals[0], to_mval<Target>(b[0]));
                Target::ret(as);
                break;
            }
            case OP_ADD: {
                add<Target>(as, size, vals[0], vals[1], vals[2]);
                break;
            }
            case OP_SUB: {
                sub<Target>(as, size, vals[0], vals[1], vals[2]);
                break;
            }
            case OP_MUL: {
                mul<Target>(as, size, vals[0], vals[1], vals[2]);
                break;
            }
            case OP_LT: {
                if (insn.type == T_F32 || insn.type == T_F64)
                    fccc<Target>(as, size, FCOND_LT, vals[0], vals[1], vals[2]);
                else if (insn.type <= T_U8 && insn.type >= T_UWORD)
                    ccc<Target>(as, size, COND_BELOW, vals[0], vals[1], vals[2]);
                else
                    ccc<Target>(as, size, COND_LT, vals[0], vals[1], vals[2]);
                break;
            }
            case OP_JZ: {
                Target::jz(as, vals[1], vals[2]);
                break;
            }
            case OP_JNZ: {
                Target::jnz(as, vals[1], vals[2]);
                break;
            }
            case OP_MOV: {
                mov<Target>(as, size, vals[0], vals[1]);
                break;
            }
            case OP_LABEL: {
                Target::local(as, vals[1].payload.sym);
                break;
            }
            default:
                unreachable("Unimplemented instruction opcode!");
                break;
        }   
    }
}

#endif