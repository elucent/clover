#include "jasmine/pass.h"
#include "jasmine/insn.h"
#include "lib/deque.h"

MODULE(jasmine)

template<typename T>
void initInsnData(const Function& fn, vec<T>& v, const T& item) {
    v.clear();
    for (i32 i = 0; i < fn.insns.size(); i ++)
        v.push(item);
}

template<typename T>
vec<T> initInsnData(const Function& fn, const T& item) {
    vec<T> v;
    for (i32 i = 0; i < fn.insns.size(); i ++)
        v.push(item);
    return v;
}

#define IFUNC(...) [&](Function& fn, localidx id, Insn& insn) { __VA_ARGS__; }
#define PFUNC(...) [&](Function& fn, localidx id, Insn& insn, Param& param, Arg& arg) { __VA_ARGS__; }

template<typename Func>
void forEachExit(Function& fn, localidx id, Insn& insn, const Func& func) {
    auto params = insn.params(fn);
    auto args = insn.args(fn);
    for (i32 i = 0; i < params.n; i ++) if (params[i] == P_BRANCH)
        func(fn, id, insn, params[i], args[i]);
}

template<typename Func>
void forEachArg(Function& fn, localidx id, Insn& insn, const Func& func) {
    auto params = insn.params(fn);
    auto args = insn.args(fn);
    for (i32 i = 0; i < params.n; i ++)
        func(fn, id, insn, params[i], args[i]);
}

template<typename Func>
void forEachInsn(Function& fn, const Func& func) {
    for (localidx i = 0; i < fn.insns.size(); i ++) {
        Insn& insn = fn.insns[i];
        func(fn, i, insn);
    }
}

template<typename Func>
void forEachInsnDFS(Function& fn, const Func& func) {
    vec<bool> visited = initInsnData(fn, false);
    deque<localidx> frontier;
    frontier.pushr(fn.entrypoint);
    visited[fn.entrypoint] = true;
    while (frontier.size()) {
        localidx next = frontier.popr();
        Insn& insn = fn.insns[next];
        func(fn, next, insn);
        forEachExit(fn, next, insn, PFUNC(if (!visited[arg.branch.dest]) {
            frontier.pushr(arg.branch.dest);
            visited[arg.branch.dest] = true;
        }));
    }
}

template<typename Func>
void forEachInsnBFS(Function& fn, const Func& func) {
    vec<bool> visited = initInsnData(fn, false);
    deque<localidx> frontier;
    frontier.pushr(fn.entrypoint);
    visited[fn.entrypoint] = true;
    while (frontier.size()) {
        localidx next = frontier.popl();
        Insn& insn = fn.insns[next];
        func(fn, next, insn);
        forEachExit(fn, next, insn, PFUNC(if (!visited[arg.branch.dest]) {
            frontier.pushr(arg.branch.dest);
            visited[arg.branch.dest] = true;
        }));
    }
}

void reduce(PassInfo& info, Function& fn) {
    //
}

void pie(PassInfo& info, Function& fn) {
    //
}

constexpr const i32 BB_BEGIN = 1, BB_END = 2;

void cfg(PassInfo& info, Function& fn) {
    initInsnData(fn, info.pred, Pred());
    forEachInsn(fn, IFUNC(
        forEachExit(fn, id, insn, PFUNC(info.pred[arg.branch.dest].push(id)));
    ));

    vec<i8> bb_bounds;              // Stores whether each instruction is a start/end of a basic block.
    initInsnData(fn, bb_bounds, '\0'); // Start with no beginnings or ends.
    forEachInsn(fn, IFUNC(
        if (insn.is_control()) {
            bb_bounds[id] |= BB_END;
            forEachExit(fn, id, insn, PFUNC(
                bb_bounds[arg.branch.dest] |= BB_BEGIN;
            ));
        }
        if (info.pred[id].size() == 0)
            bb_bounds[id] |= BB_BEGIN;
    ));
    initInsnData(fn, info.ib, -1);
    forEachInsn(fn, IFUNC(
        if (bb_bounds[id] & BB_BEGIN) {
            localidx ib = info.bb.size();
            info.bb.push({});
            BasicBlock& bb = info.bb.back();
            localidx i = id;
            while (!(bb_bounds[i] & BB_END)) {
                assert(!fn.insns[i].is_control());
                assert(fn.insns[i].params(fn).back() == P_BRANCH);
                bb.seq.push(i);
                info.ib[i] = ib;
                i = fn.insns[i].args(fn).back().branch.dest;
                if (bb_bounds[i] & BB_BEGIN) // Must be a different block's starting point.
                    return;
            }
            bb.seq.push(i);
            info.ib[i] = ib;
        }
    ));

    // for (const auto& bb : info.bb) {
    //     println("BB #", &bb - &info.bb[0]);
    //     for (localidx i : bb.seq) {
    //         print("  ");
    //         format_insn(fn, fn.insns[i], file_stdout);
    //         println();
    //     }
    // }

    info.status |= PassInfo::CFG;
}

void dce(PassInfo& info, Function& fn) {
    //
}

void foldc(PassInfo& info, Function& fn) {
    //
}

void renumber(PassInfo& info, Function& fn) {
    assert(info.status & PassInfo::CFG); // We need basic block information for renumbering.

    vec<localidx> mapping;
    initInsnData(fn, mapping, 0);
    localidx count = 0;
    for (const auto& bb : info.bb)
        for (localidx insn : bb.seq)
            mapping[insn] = count ++;
    forEachInsn(fn, IFUNC(
        forEachArg(fn, id, insn, PFUNC(
            if (param == P_REG)
                arg.reg = mapping[arg.reg];
            else if (param == P_BRANCH)
                arg.branch = { mapping[arg.branch.dest], arg.branch.flags };      
        ))
    ));
    // Update CFG
    for (auto& bb : info.bb)
        for (localidx& insn : bb.seq)
            insn = mapping[insn];
    for (auto& pvec : info.pred)
        for (localidx& insn : pvec)
            insn = mapping[insn];
    // Reorder vector.
    auto tmp = fn.insns;
    for (localidx i = 0; i < fn.insns.size(); i ++)
        fn.insns[mapping[i]] = tmp[i];
}

void sched(PassInfo& info, Function& fn) {
    //
}

void unroll(PassInfo& info, Function& fn) {
    //
}

void tryinline(PassInfo& info, Function& fn) {
    //
}

void live(PassInfo& info, Function& fn) {
    //
}

void irg(PassInfo& info, Function& fn) {
    //
}


void Function::dumpDOT(fd io, PassInfo& info) const {
    dumpDOTPrelude(io);
    if (info.didPhase(PassInfo::CFG)) {
        for (i32 i = 0; i < info.bb.size(); i ++) {
            ::write(io, "  subgraph \"cluster_", i, "\" {\n");
            ::write(io, "    label=\"BB#", i, "\"\n");
            ::write(io, "    labeljust=left\n");
		    ::write(io, "    fontsize=16;\n");
            for (i32 j : info.bb[i].seq) {
                ::write(io, "    ");
                DOTInsnName(io, *this, insns[j], j);
                ::write(io, '\n');
            }
            ::write(io, "  }\n");
        }
    }
    dumpDOTInsns(io);
    dumpDOTEpilogue(io);
}

ENDMODULE()