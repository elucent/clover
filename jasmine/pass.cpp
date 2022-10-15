#include "jasmine/pass.h"
#include "jasmine/insn.h"
#include "jasmine/obj.h"
#include "lib/bits.h"
#include "lib/math.h"

PassInfo* makepassinfo() {
    return new PassInfo();
}

void init(Function& fn, PassInfo& info) {
    info.slots.clear();
    info.stack = 0;
}

template<typename Pred>
void rmif(Function& fn, const Pred& pred) {
    vec<localidx, 32> remapping;
    for (i64 i = 0; i < fn.insns.size(); i ++) remapping.push(i);
    i32 w = 0, r = 0;
    for (; r < fn.insns.size(); r ++) {
        auto& insn = fn.insns[r];
        if (!pred(insn)) {
            remapping[r] = w;
            fn.insns[w ++] = (Insn&&)insn;
        }
    }
    for (i64 n = 0; n < w; n ++) {
        auto& insn = fn.insns[n];
        for (i32 i = 0; i < insn.params(fn).n; i ++) {
            if (insn.params(fn)[i] == P_REG) insn.args(fn)[i].reg = remapping[insn.args(fn)[i].reg];
        }
    }
    fn.insns.trim(r - w);
}

template<typename T>
void rm(vec<T>& v, const T& elt) {
    for (T& t : v) if (t == elt) swap(t, v.back()), v.pop();
}

i64 copy_params(Function& dst, const Function& src, Insn& insn) {
    i64 oldp = insn.pidx;
    insn.pidx = dst.params.size();
    for (i32 i = 0; i < insn.nparams; i ++) 
        dst.params.push(src.params[oldp + i]), dst.args.push(src.args[oldp + i]);
    return insn.pidx;
}

void insert(Function& fn, const Function& src, InsnVec& v, i32 pos, const_slice<Insn> insns) {
    for (i32 i = 0; i < insns.n; i ++) 
        v.push(insns[0]);
    for (i32 i = 0; i < v.size(); i ++) for (i32 j = 0; j < v[i].params(fn).n; j ++)
        if (v[i].params(fn)[j] == P_REG && v[i].args(fn)[j].reg >= pos)
            v[i].args(fn)[j].reg += insns.n;
    for (i32 i = pos; i < pos + insns.n; i ++) {
        v[i + insns.n] = v[i];
        v[i] = insns[i - pos];
        copy_params(fn, src, v[i]);
        
        for (i32 j = 0; j < v[i].params(fn).n; j ++) {
            if (v[i].params(fn)[j] == P_REG) v[i].args(fn)[j].reg += pos;
        }
    }
}

bool should_inline(Function& dst, const Function& src, PassInfo& info) {
    return src.insns.size() < 16;   // Unsophisticated heuristic, but good enough for the time being.
}

void inlining(Function& fn, PassInfo& info) {
    const auto& funcs = fn.obj->funcs;
    vec<pair<Param, Arg>, 8> args, rets;
    for (i32 i = 0; i < fn.insns.size(); i ++) {
        if (fn.insns[i].op == OP_CALL && fn.insns[i].params(fn)[0] == P_FUNC) {
            if (fn.obj->funcs[fn.insns[i].args(fn)[0].func] == &fn) continue; // Don't inline recursive calls...yet.

            const Function& src = *funcs[fn.insns[i].args(fn)[0].func];
            if (!should_inline(fn, src, info)) continue;
            const auto& call = fn.insns[i];

            args.clear();
            rets.clear();
            for (i32 j = 1; j < call.params(fn).n; j ++) 
                args.push({ call.params(fn)[j], call.args(fn)[j] });
            insert(fn, src, fn.insns, i, {&src.insns[0], src.insns.size()});

            labelidx lbl = fn.label();
            for (i32 j = i; j < i + src.insns.size(); j ++) {
                if (fn.insns[j].op == OP_ARG) { // Convert parameters to moves.
                    auto& par = fn.insns[j];
                    par.op = OP_MOV;
                    par.params(fn)[0] = args[par.args(fn)[0].imm].first;
                    par.args(fn)[0] = args[par.args(fn)[0].imm].second;
                }
                else if (fn.insns[j].op == OP_RET) {
                    auto& ret = fn.insns[j];
                    rets.push({ ret.params(fn)[0], ret.args(fn)[0] });
                    if (j == i + src.insns.size() - 1) ret.op = OP_NOP, ret.nparams = 0;
                    else {
                        ret.op = OP_JUMP;
                        ret.params(fn)[0] = P_LABEL;
                        ret.args(fn)[0].lbl = lbl;
                    }
                }
            }
            fn.labels[lbl] = i + src.insns.size();
            auto& phi = fn.insns[i + src.insns.size()];
            phi.type = phi.result_type(fn.obj->types);
            phi.op = rets.size() == 1 ? OP_MOV : OP_PHI;
            if (rets.size() == 0) phi.op = OP_NOP;
            phi.nparams = rets.size();
            phi.pidx = fn.params.size();
            for (const auto& r : rets) fn.params.push(r.first), fn.args.push(r.second);
            i += src.insns.size();
        }
    }
}

void cfg(Function& fn, PassInfo& info) {
    info.ib.clear();
    info.bb.clear();
    info.succ.clear();
    info.pred.clear();

    for (i32 i = 0; i < fn.insns.size(); i ++) info.ib.push(-1), info.succ.push({}), info.pred.push({});

    for (i32 i = 0; i < fn.insns.size(); i ++) { // Find successors.
        localidx a = -1, b = -1;
        if (fn.insns[i].is_jump()) {
            if (fn.insns[i].op == OP_JUMP) { // Conditional jump.
                if (fn.insns[i].params(fn)[0] == P_LABEL) 
                    a = fn.labels[fn.insns[i].args(fn)[0].lbl];
            }
            else {
                a = i + 1;
                if (fn.insns[i].params(fn)[0] == P_LABEL) 
                    b = fn.labels[fn.insns[i].args(fn)[0].lbl];
            }
        }
        else if (fn.insns[i].op != OP_RET) a = i + 1;
        info.succ[i] = Succ(a, b);
        if (a >= 0) info.pred[a].push(i);
        if (b >= 0) info.pred[b].push(i);
    }

    info.ib[0] = 0; // Entry block.
    info.bb.push(BasicBlock(0, 1));
    for (i32 i = 0; i < fn.insns.size(); i ++) {
        if (fn.insns[i].is_jump()) for (localidx l : info.succ[i].items()) if (l) { // Don't overwrite block 0.
            if (info.ib[l] != -1) continue; // Don't overwrite blocks we just created.
            info.ib[l] = info.bb.size();
            info.bb.push(BasicBlock(l, l + 1));
        }
    }

    for (i32 i = 1; i < fn.insns.size(); i ++)
        if (info.ib[i] < 0 && fn.insns[i - 1].op != OP_JUMP && fn.insns[i - 1].op != OP_RET) {
            info.ib[i] = info.ib[i - 1];
            if (info.ib[i] >= 0) info.bb[info.ib[i]].end = i + 1;
        }

    for (i32 i = 1; i < fn.insns.size(); i ++) {
        i32 pbb = info.ib[i];
        if (pbb < 0) continue;
        BasicBlock& pred = info.bb[pbb];
        if (fn.insns[i].is_jump() && fn.insns[i].params(fn)[0] == P_LABEL) {
            i32 dst = fn.labels[fn.insns[i].args(fn)[0].lbl];
            i32 dbb = info.ib[dst];
            if (dbb < 0) continue;
            BasicBlock& succ = info.bb[dbb];
            succ.pred.push(pbb);
            pred.succ.push(dbb);
        }
        if (info.ib[i - 1] != info.ib[i] && fn.insns[i - 1].op != OP_JUMP && fn.insns[i - 1].op != OP_RET) {
            i32 bbb = info.ib[i - 1];
            if (bbb < 0) continue;
            BasicBlock& pre_pred = info.bb[bbb];
            pre_pred.succ.push(pbb);
            pred.pred.push(bbb);
        }
    }

    // for (i32 i = 0; i < info.bb.size(); i ++) {
    //     print("bb #", i, '\n');
    //     print("pred:");
    //     for (i32 bb : info.bb[i].pred) print(" #", bb);
    //     print('\n');
    //     for (i32 j = info.bb[i].start; j < info.bb[i].end; j ++)
    //         print("  "), format_insn(fn, fn.insns[j], stdout), print('\n');
    //     print("succ:");
    //     for (i32 bb : info.bb[i].succ) print(" #", bb);
    //     print('\n');
    //     print('\n');
    // }
}

void foldc(Function& fn, PassInfo& info) {
    vec<pair<Param, Arg>> folds;
    bool changed = false;
    for (i32 i = 0; i < fn.insns.size(); i ++) {
        auto& insn = fn.insns[i];
        auto args = insn.args(fn);
        auto params = insn.params(fn);
        folds.push({ P_REG, Arg(i) });
        if (params.n == 1 && params[0] == P_IMM) switch (insn.op) {
            case OP_MOV:
                folds[i].first = P_IMM;
                folds[i].second.imm = args[0].imm;
                break;
            case OP_NEG: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(-args[0].imm); break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(-args[0].imm); break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(-args[0].imm); break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(-args[0].imm); break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(-args[0].imm); break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(-args[0].f32imm); break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(-args[0].f64imm); break;
                default: break;
            } break;
            case OP_NOT: 
                folds[i].first = P_IMM;
                folds[i].second.imm = ~args[0].imm;
                break;
            default: break;
        }
        else if (params.n == 2 && params[0] == P_IMM && params[1] == P_IMM) switch (insn.op) {
            case OP_ADD: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm + args[1].imm); break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm + args[1].imm); break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm + args[1].imm); break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm + args[1].imm); break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm + args[1].imm); break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm + args[1].imm); break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm + args[1].imm); break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm + args[1].imm); break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm + args[1].imm); break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm + args[1].imm); break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm + args[1].f32imm); break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm + args[1].f64imm); break;
                default: break;
            } break;
            case OP_SUB: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm - args[1].imm); break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm - args[1].imm); break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm - args[1].imm); break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm - args[1].imm); break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm - args[1].imm); break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm - args[1].imm); break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm - args[1].imm); break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm - args[1].imm); break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm - args[1].imm); break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm - args[1].imm); break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm - args[1].f32imm); break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm - args[1].f64imm); break;
                default: break;
            } break;
            case OP_MUL: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm * args[1].imm); break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm * args[1].imm); break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm * args[1].imm); break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm * args[1].imm); break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm * args[1].imm); break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm * args[1].imm); break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm * args[1].imm); break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm * args[1].imm); break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm * args[1].imm); break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm * args[1].imm); break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm * args[1].f32imm); break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm * args[1].f64imm); break;
                default: break;
            } break;
            case OP_DIV: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm / args[1].imm); break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm / args[1].imm); break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm / args[1].imm); break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm / args[1].imm); break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm / args[1].imm); break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm) / u8(args[1].imm); break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm) / u16(args[1].imm); break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm) / u32(args[1].imm); break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm) / u64(args[1].imm); break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm) / uword(args[1].imm); break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm / args[1].f32imm); break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm / args[1].f64imm); break;
                default: break;
            } break;
            case OP_REM: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm % args[1].imm); break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm % args[1].imm); break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm % args[1].imm); break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm % args[1].imm); break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm % args[1].imm); break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm) % u8(args[1].imm); break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm) % u16(args[1].imm); break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm) % u32(args[1].imm); break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm) % u64(args[1].imm); break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm) % uword(args[1].imm); break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = frem(args[0].f32imm, args[1].f32imm); break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = frem(args[0].f64imm, args[1].f64imm); break;
                default: break;
            } break;
            case OP_AND:
                folds[i].first = P_IMM;
                folds[i].second.imm = args[0].imm & args[1].imm;
                break;
            case OP_XOR:
                folds[i].first = P_IMM;
                folds[i].second.imm = args[0].imm ^ args[1].imm;
                break;
            case OP_OR:
                folds[i].first = P_IMM;
                folds[i].second.imm = args[0].imm | args[1].imm;
                break;
            case OP_SHL:
                folds[i].first = P_IMM;
                folds[i].second.imm = args[0].imm << args[1].imm;
                break;
            case OP_SHR:
                folds[i].first = P_IMM;
                folds[i].second.imm = args[0].imm >> args[1].imm;
                break;
            case OP_EQ: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm) == i8(args[1].imm) ? 1 : 0; break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm) == i16(args[1].imm) ? 1 : 0; break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm) == i32(args[1].imm) ? 1 : 0; break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm) == i64(args[1].imm) ? 1 : 0; break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm) == iword(args[1].imm) ? 1 : 0; break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm) == u8(args[1].imm) ? 1 : 0; break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm) == u16(args[1].imm) ? 1 : 0; break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm) == u32(args[1].imm) ? 1 : 0; break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm) == u64(args[1].imm) ? 1 : 0; break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm) == uword(args[1].imm) ? 1 : 0; break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm) == float(args[1].f32imm) ? 1 : 0; break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm) == double(args[1].f64imm) ? 1 : 0; break;
                default: break;
            } break;
            case OP_NEQ: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm) != i8(args[1].imm) ? 1 : 0; break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm) != i16(args[1].imm) ? 1 : 0; break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm) != i32(args[1].imm) ? 1 : 0; break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm) != i64(args[1].imm) ? 1 : 0; break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm) != iword(args[1].imm) ? 1 : 0; break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm) != u8(args[1].imm) ? 1 : 0; break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm) != u16(args[1].imm) ? 1 : 0; break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm) != u32(args[1].imm) ? 1 : 0; break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm) != u64(args[1].imm) ? 1 : 0; break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm) != uword(args[1].imm) ? 1 : 0; break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm) != float(args[1].f32imm) ? 1 : 0; break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm) != double(args[1].f64imm) ? 1 : 0; break;
                default: break;
            } break;
            case OP_GT: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm) > i8(args[1].imm) ? 1 : 0; break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm) > i16(args[1].imm) ? 1 : 0; break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm) > i32(args[1].imm) ? 1 : 0; break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm) > i64(args[1].imm) ? 1 : 0; break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm) > iword(args[1].imm) ? 1 : 0; break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm) > u8(args[1].imm) ? 1 : 0; break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm) > u16(args[1].imm) ? 1 : 0; break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm) > u32(args[1].imm) ? 1 : 0; break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm) > u64(args[1].imm) ? 1 : 0; break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm) > uword(args[1].imm) ? 1 : 0; break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm) > float(args[1].f32imm) ? 1 : 0; break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm) > double(args[1].f64imm) ? 1 : 0; break;
                default: break;
            } break;
            case OP_GEQ: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm) >= i8(args[1].imm) ? 1 : 0; break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm) >= i16(args[1].imm) ? 1 : 0; break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm) >= i32(args[1].imm) ? 1 : 0; break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm) >= i64(args[1].imm) ? 1 : 0; break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm) >= iword(args[1].imm) ? 1 : 0; break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm) >= u8(args[1].imm) ? 1 : 0; break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm) >= u16(args[1].imm) ? 1 : 0; break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm) >= u32(args[1].imm) ? 1 : 0; break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm) >= u64(args[1].imm) ? 1 : 0; break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm) >= uword(args[1].imm) ? 1 : 0; break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm) >= float(args[1].f32imm) ? 1 : 0; break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm) >= double(args[1].f64imm) ? 1 : 0; break;
                default: break;
            } break;
            case OP_LT: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm) < i8(args[1].imm) ? 1 : 0; break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm) < i16(args[1].imm) ? 1 : 0; break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm) < i32(args[1].imm) ? 1 : 0; break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm) < i64(args[1].imm) ? 1 : 0; break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm) < iword(args[1].imm) ? 1 : 0; break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm) < u8(args[1].imm) ? 1 : 0; break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm) < u16(args[1].imm) ? 1 : 0; break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm) < u32(args[1].imm) ? 1 : 0; break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm) < u64(args[1].imm) ? 1 : 0; break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm) < uword(args[1].imm) ? 1 : 0; break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm) < float(args[1].f32imm) ? 1 : 0; break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm) < double(args[1].f64imm) ? 1 : 0; break;
                default: break;
            } break;
            case OP_LEQ: switch (i8(insn.type)) {
                case T_I8: folds[i].first = P_IMM, folds[i].second.imm = i8(args[0].imm) <= i8(args[1].imm) ? 1 : 0; break;
                case T_I16: folds[i].first = P_IMM, folds[i].second.imm = i16(args[0].imm) <= i16(args[1].imm) ? 1 : 0; break;
                case T_I32: folds[i].first = P_IMM, folds[i].second.imm = i32(args[0].imm) <= i32(args[1].imm) ? 1 : 0; break;
                case T_I64: folds[i].first = P_IMM, folds[i].second.imm = i64(args[0].imm) <= i64(args[1].imm) ? 1 : 0; break;
                case T_IWORD: folds[i].first = P_IMM, folds[i].second.imm = iword(args[0].imm) <= iword(args[1].imm) ? 1 : 0; break;
                case T_U8: folds[i].first = P_IMM, folds[i].second.imm = u8(args[0].imm) <= u8(args[1].imm) ? 1 : 0; break;
                case T_U16: folds[i].first = P_IMM, folds[i].second.imm = u16(args[0].imm) <= u16(args[1].imm) ? 1 : 0; break;
                case T_U32: folds[i].first = P_IMM, folds[i].second.imm = u32(args[0].imm) <= u32(args[1].imm) ? 1 : 0; break;
                case T_U64: folds[i].first = P_IMM, folds[i].second.imm = u64(args[0].imm) <= u64(args[1].imm) ? 1 : 0; break;
                case T_UWORD: folds[i].first = P_IMM, folds[i].second.imm = uword(args[0].imm) <= uword(args[1].imm) ? 1 : 0; break;
                case T_F32: folds[i].first = P_IMM, folds[i].second.imm = float(args[0].f32imm) <= float(args[1].f32imm) ? 1 : 0; break;
                case T_F64: folds[i].first = P_IMM, folds[i].second.imm = double(args[0].f64imm) <= double(args[1].f64imm) ? 1 : 0; break;
                default: break;
            } break;
            default: break;
        }
        else if (insn.is_jump() && insn.op != OP_JUMP && params[1] == P_IMM) {
            bool taken = false;
            if (args[1].imm || insn.op == OP_JZ) insn.op = OP_JUMP, insn.nparams = 1;
            else insn.op = OP_NOP, insn.nparams = 0;
            changed = true;
        }
    }
    for (i32 i = 0; i < fn.insns.size(); i ++) {
        auto& insn = fn.insns[i];
        auto args = insn.args(fn);
        auto params = insn.params(fn);
        if (folds[i].first == P_IMM) insn.op = OP_NOP, changed = true;
        else for (i32 j = 0; j < params.n; j ++) {
            if (params[j] == P_REG && folds[args[j].reg].first == P_IMM) {
                params[j] = P_IMM, args[j] = folds[args[j].reg].second;
                changed = true;
            }
        }
    }
    if (changed) foldc(fn, info);
}

void dce(Function& fn, PassInfo& info) {
    cfg(fn, info);
    for (const auto& bb : info.bb) {
        if (bb.start != 0 && bb.pred.size() == 0) for (i32 i = bb.start; i < bb.end; i ++)
            fn.insns[i].op = OP_NOP;
    }
    rmif(fn, [](const Insn& insn) -> bool { return insn.op == OP_NOP; });
}

void print_slots(const Function& fn, const Target& arch, stream& io) {
    // fn.formatshort(io);
    // write(io, '\n');
    // for (localidx l = 0; l < fn.slots.size(); l ++) {
    //     if (fn.slots[l].type != SLOT_NONE) switch (fn.slots[l].type) {
    //         case SLOT_NONE: break;
    //         case SLOT_GPREG: write(io, "  %", l, " : ", arch.gpreg_name(fn.slots[l].reg), '\n'); break;
    //         case SLOT_FPREG: write(io, "  %", l, " : ", arch.fpreg_name(fn.slots[l].reg), '\n'); break;
    //         // case SLOT_GPREG_PAIR: write(io, "  %", l, " : {", arch.gpreg_name(fn.slots[l].regpair[0]), ", ", arch.gpreg_name(fn.slots[l].regpair[1]), "}\n"); break;
    //         // case SLOT_FPREG_PAIR: write(io, "  %", l, " : {", arch.fpreg_name(fn.slots[l].regpair[0]), ", ", arch.fpreg_name(fn.slots[l].regpair[1]), "}\n"); break;
    //         // case SLOT_GPFPREG_PAIR: write(io, "  %", l, " : {", arch.gpreg_name(fn.slots[l].regpair[0]), ", ", arch.fpreg_name(fn.slots[l].regpair[1]), "}\n"); break;
    //         // case SLOT_FPGPREG_PAIR: write(io, "  %", l, " : {", arch.fpreg_name(fn.slots[l].regpair[0]), ", ", arch.gpreg_name(fn.slots[l].regpair[1]), "}\n"); break;
    //         case SLOT_STACK: write(io, "  %", l, " : [sp", fn.slots[l].stack, "]\n"); break;
    //     }
    // }
}

struct Interval {
    i32 start, end, hint, next;
};

struct MaybeReg {
    mreg r;
    bool there;

    inline MaybeReg(mreg r_in): r(r_in), there(true) {}
    inline MaybeReg(): there(false) {}

    inline operator bool() const {
        return there;
    }

    inline mreg operator*() const {
        return r;
    }
};

struct RegStack {
    const_slice<mreg> regs;
    bitset<> taken;
    i64 next;

    inline RegStack(const_slice<mreg> regs_in): regs(regs_in), taken(0), next(64) {
        for (mreg r : regs) if (r < next) next = r;
    }

    inline MaybeReg take() {
        if (next == regs.n) return {};
        mreg r = regs[next];
        taken.on(next);
        do ++ next; while (next < regs.n && taken[next]);
        return r;
    }

    inline MaybeReg take(mreg r) {
        if (next == regs.n) return {};
        if (taken[r]) return {};
        taken.on(r);
        if (next == r) do ++ next; while (next < regs.n && taken[next]);
        return r;
    }

    inline void give(mreg r) {
        if (next > r) next = r;
        taken.off(r);
    }
};

struct Intervals {
    vec<vec<LiveInterval, 2>>& live;

    inline Intervals(vec<vec<LiveInterval, 2>>& live_in): live(live_in) {}
};

void live(Function& fn, PassInfo& info) {
    info.live.clear();
    vec<vec<localidx>, 16> bbin;
    vec<localidx, 64> tips;
    for (i32 i = 0; i < fn.insns.size(); i ++) {
        tips.push(-1);
        info.live.push(vec<LiveInterval, 2>());
    }
    for (i32 i = 0; i < info.bb.size(); i ++) 
        bbin.push(vec<localidx>());
    for (i32 bbi = i32(info.bb.size()) - 1; bbi >= 0; bbi --) {
        BasicBlock& bb = info.bb[bbi];
        for (localidx i : bbin[bbi])
            tips[i] = bb.end;
        bbin[bbi].clear();

        for (i32 i = bb.end - 1; i >= bb.start; i --) {
            Insn& insn = fn.insns[i];
            const auto& params = insn.params(fn);
            const auto& args = insn.args(fn);
            // print(" * visiting insn ", i, ":\t");
            // format_insn(fn, insn, stdout);
            // println();
            if (insn.has_output()) {
                // println("   * dieing local %", i);
                info.live[i].push({i, tips[i]});
                tips[i] = -1;
            }
            for (i32 j = 0; j < args.n; j ++) {
                if (params[j] == P_REG && tips[args[j].reg] == -1) {
                    // println("   * using local %", args[j].reg);
                    tips[args[j].reg] = i;
                }
            }
        }

        for (i32 pbb : bb.pred) {
            for (i32 i = 0; i < tips.size(); i ++) if (tips[i] != -1) {
                // println("   * adding use of %", i, " to bb", pbb);
                bbin[pbb].push(i);
                info.live[i].push({bb.start, tips[i]});
                tips[i] = -1;
            }
        }
    }

    for (i32 bbi = i32(info.bb.size()) - 1; bbi >= 0; bbi --) {
        if (bbin[bbi].size() == 0) 
            continue;
        BasicBlock& bb = info.bb[bbi];
        for (i32 i = 0; i < tips.size(); i ++)
            tips[i] = -1;
        for (localidx i : bbin[bbi])
            tips[i] = bb.end;
        bbin[bbi].clear();

        for (i32 i = bb.end - 1; i >= bb.start; i --) {
            Insn& insn = fn.insns[i];
            const auto& params = insn.params(fn);
            const auto& args = insn.args(fn);
            // print(" * visiting insn ", i, ":\t");
            // format_insn(fn, insn, stdout);
            // println();
            if (insn.has_output()) {
                info.live[i].push({i, tips[i]});
                tips[i] = -1;
            }
        }
    }

    // for (i32 i = 0; i < info.live.size(); i ++) {
    //     const auto& l = info.live[i];
    //     if (l.size()) {
    //         print('%', i, " live over ");
    //         bool first = true;
    //         for (const auto& iv : l) {
    //             if (!first) print(", ");
    //             first = false;
    //             print('[', iv.start, "-", iv.end, ']');
    //         }
    //         print('\n');
    //     }
    // }
}

u64 hash(const pair<localidx, localidx>& p) {
    return hash(p.first) * 31ul + hash(p.second);
}

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