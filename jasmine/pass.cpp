#include "jasmine/pass.h"
#include "jasmine/insn.h"
#include "lib/deque.h"

u64 hash(const pair<jasmine::localidx, jasmine::localidx>& pair) {
    return ::hash(pair.first) * 71 ^ ::hash(pair.second);
}

MODULE(jasmine)

template<typename T>
void initInsnData(const Function& fn, vec<T, EXPECTED_INSNS>& v, const T& item) {
    v.clear();
    for (i32 i = 0; i < fn.insns.size(); i ++)
        v.push(item);
}

template<typename T>
vec<T, EXPECTED_INSNS> initInsnData(const Function& fn, const T& item) {
    vec<T, EXPECTED_INSNS> v;
    for (i32 i = 0; i < fn.insns.size(); i ++)
        v.push(item);
    return v;
}

#define IFUNC(...) [&](Function& fn, localidx id, Insn& insn) { __VA_ARGS__; }
#define PFUNC(...) [&](Function& fn, localidx id, Insn& insn, Param& param, Arg& arg) { __VA_ARGS__; }
#define PHIFUNC(...) [&](Function& fn, localidx id, Insn& insn, localidx& from, Param& param, Arg& arg) { __VA_ARGS__; }

template<typename Container, typename Pred>
void removeIf(Container& container, const Pred& pred) {
    auto it = container.begin();
    auto writer = it;
    while (it != container.end()) {
        if (!pred(*it)) {
            *writer = *it;
            ++ writer;
        }
        ++ it;
    }
    i32 toRemove = 0;
    while (writer != it) {
        ++ writer;
        ++ toRemove;
    }
    for (i32 i = 0; i < toRemove; i ++)
        container.pop();
}

template<typename Func>
void forEachExit(Function& fn, localidx id, Insn& insn, const Func& func) {
    auto params = insn.params(fn);
    auto args = insn.args(fn);
    for (i32 i = 0; i < params.n; i ++) if (params[i] == P_BRANCH)
        func(fn, id, insn, params[i], args[i]);
}

template<typename Func>
void forEachPhiArg(Function& fn, localidx id, Insn& insn, const Func& func) {
    auto params = insn.params(fn);
    auto args = insn.args(fn);
    for (i32 i = 0; i + 1 < params.n; i += 2) {
        assert(params[i] == P_INCOMING);
        assert(params[i + 1] != P_INCOMING);
        func(fn, id, insn, args[i].branch.dest, params[i + 1], args[i + 1]);
    }
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
void forEachInsn(Function& fn, assembler::Op op, const Func& func) {
    for (localidx i = 0; i < fn.insns.size(); i ++) {
        Insn& insn = fn.insns[i];
        if (insn.op == op) func(fn, i, insn);
    }
}

template<typename Func>
void forEachInsnDFS(Function& fn, const Func& func) {
    auto visited = initInsnData(fn, false);
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
    auto visited = initInsnData(fn, false);
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

void replacePred(PassInfo& info, Function& fn, localidx id, localidx old, localidx replaced) {
    for (localidx& pred : info.pred[id]) if (pred == old) pred = replaced;
}

void insertAfter(PassInfo& info, Function& fn, localidx id, Insn insn) {
    assert(!fn.insns[id].is_control());
    assert(fn.insns[id].params(fn).back() == P_BRANCH);

    if (info.didPhase(PassInfo::CFG)) {
        BasicBlock& bb = info.bb[info.ib[id]];
        localidx prev = id;
        bb.seq.push(fn.insns.size());
        localidx last = bb.seq.size() - 2;
        while (bb.seq[last] != prev) { // Swap new index into place.
            swap(bb.seq[last], bb.seq[last + 1]);
            last --;
        }
    }
    if (info.didPhase(PassInfo::CFG)) forEachExit(fn, id, fn.insns[id], PFUNC(
        localidx succ = arg.branch.dest;
        replacePred(info, fn, succ, id, fn.insns.size());
    ));
    fn.insns.push(insn);
    fn.insns.back().args(fn).back().branch.dest = fn.insns[id].args(fn).back().branch.dest;
    fn.insns[id].args(fn).back().branch.dest = fn.insns.size() - 1;
    info.ib.push(info.ib[id]);
    info.pred.push(vec_of<localidx, 2>(id));
}

template<typename ...Args>
void insertAfter(PassInfo& info, Function& fn, localidx id, const Args&... args) {
    insertAfter(info, fn, id, createInsn(fn, args...));
}

void pie(PassInfo& info, Function& fn) {
    using namespace assembler;
    forEachInsn(fn, PHI, IFUNC(
        localidx phiid = id;
        typeidx phitype = insn.type(fn);
        Insn newInsn = createInsn(fn, VAR, phitype);

        forEachPhiArg(fn, id, insn, PHIFUNC(
            insertAfter(info, fn, from, MOV, phitype, Value{param, arg}, Reg(id));
        ));
        
        newInsn.args(fn).back().branch.dest = fn.insns[id].args(fn).back().branch.dest;
        fn.insns[id] = newInsn; // Replace phi with generic variable.
    ));
}

constexpr const i32 BB_BEGIN = 1, BB_END = 2;

void cfg(PassInfo& info, Function& fn) {
    initInsnData(fn, info.pred, Pred());
    forEachInsn(fn, IFUNC(
        forEachExit(fn, id, insn, PFUNC(info.pred[arg.branch.dest].push(id)));
    ));

    auto bb_bounds = initInsnData(fn, '\0'); // Start with no beginnings or ends.
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

    info.status |= PassInfo::CFG;
}

void removePred(PassInfo& info, Function& fn, localidx insn, localidx toRemove) {
    for (localidx& pred : info.pred[insn]) {
        if (pred == toRemove) {
            swap(pred, info.pred[insn].back());
            info.pred[insn].pop();
        }
    }
}

void dce(PassInfo& info, Function& fn) {
    assert(info.didPhase(PassInfo::CFG));

    using namespace assembler;

    // Find dead blocks and remove them as predecessors of other blocks.
    for (BasicBlock& bb : info.bb) {
        if (bb.seq.size() == 0) continue;
        if (info.pred[bb.seq[0]].size() > 0 || bb.seq[0] == fn.entrypoint) // If this bb is reachable, don't eliminate it.
            continue;
        for (localidx local : bb.seq) {
            Insn& insn = fn.insns[local];
            if (local == bb.seq.back()) forEachExit(fn, local, insn, PFUNC(
                removePred(info, fn, arg.branch.dest, local);
            ));
        }
        bb.seq.clear();
    }
    // Remove dead blocks.
    BasicBlock* writer = &info.bb[0];
    for (BasicBlock& bb : info.bb) {
        if (bb.seq.size()) *writer ++ = bb;
    }
    i32 stillAlive = writer - &info.bb[0];
    while (info.bb.size() > stillAlive) info.bb.pop();

    // Remove nops from control flow graph.
    forEachInsn(fn, NOP, IFUNC(
        localidx nopid = id;
        localidx succ = insn.args(fn).back().branch.dest;
        for (localidx pred : info.pred[nopid]) forEachExit(fn, pred, fn.insns[pred], PFUNC(
            if (arg.branch.dest == nopid)
                arg.branch.dest = succ;
        ));
        info.pred[succ] = info.pred[nopid];
    ));

    // Remove nops from basic blocks.
    for (BasicBlock& bb : info.bb) {
        localidx* writer = &bb.seq[0];
        for (localidx insn : bb.seq) {
            if (fn.insns[insn].op != NOP) *writer ++ = insn;
        }
        i32 stillAlive = writer - &bb.seq[0];
        while (bb.seq.size() > stillAlive) bb.seq.pop();
    }

    info.complete(PassInfo::DCE);
}

void foldc(PassInfo& info, Function& fn) {
    //
}

void renumber(PassInfo& info, Function& fn) {
    assert(info.didPhase(PassInfo::CFG)); // We need basic block information for renumbering.
    assert(info.didPhase(PassInfo::DCE)); // And we need DCE to guarantee NOPs can be removed.

    auto mapping = initInsnData(fn, -1);
    localidx count = 0;
    for (const auto& bb : info.bb)
        for (localidx insn : bb.seq)
            mapping[insn] = count ++;
    forEachInsn(fn, IFUNC(
        forEachArg(fn, id, insn, PFUNC(
            if (param == P_REG)
                arg.reg = mapping[arg.reg];
            else if (param == P_BRANCH || param == P_INCOMING)
                arg.branch = { mapping[arg.branch.dest], arg.branch.flags };      
        ))
    ));
    // Update CFG
    for (auto& bb : info.bb)
        for (localidx& insn : bb.seq)
            insn = mapping[insn];
    auto oldpred = info.pred;
    auto oldib = info.ib;
    for (auto& pvec : info.pred)
        pvec.clear();
    for (localidx i = 0; i < fn.insns.size(); i ++) if (mapping[i] != -1) {
        info.ib[mapping[i]] = oldib[i];
        info.pred[mapping[i]] = oldpred[i];
    }
    // Reorder vector.
    auto tmp = fn.insns;
    for (localidx i = 0; i < fn.insns.size(); i ++) if (mapping[i] != -1)
        fn.insns[mapping[i]] = tmp[i];
    while (fn.insns.size() > count) fn.insns.pop();

    // for (const auto& bb : info.bb) {
    //     println("BB #", &bb - &info.bb[0]);
    //     for (localidx i : bb.seq) {
    //         print("  ");
    //         format_insn(fn, fn.insns[i], file_stdout);
    //         println();
    //     }
    // }

    info.status |= PassInfo::RENUMBER;
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

void toposort(PassInfo& info, Function& fn, vec<i8>& marks, vec<localidx>& sorted, vec<localidx>& fixup, localidx pred, localidx bb) {
    if (marks[bb] == 2)
        return;
    if (marks[bb] == 1) {
        fixup.push(pred);
        marks[bb] = 2;
        return;
    }

    marks[bb] = 1;

    Insn& insn = fn.insns[info.bb[bb].seq.back()];
    forEachExit(fn, info.bb[bb].seq.back(), insn, PFUNC(
        toposort(info, fn, marks, sorted, fixup, bb, info.ib[arg.branch.dest]);
    ));

    if (marks[bb] == 1) {
        marks[bb] = 2;
        sorted.push(bb);
    }
}

void live(PassInfo& info, Function& fn) {
    assert(info.didPhase(PassInfo::CFG));
    assert(info.didPhase(PassInfo::RENUMBER));

    initInsnData(fn, info.live, {});

    auto tips = initInsnData(fn, LiveInterval{-1, -1, false});
    auto bbin = vec<vec<localidx, 4>, 16>({}, info.bb.size());
    auto phi_extra_ins = initInsnData(fn, vec<localidx, 2>());
    auto bbvisited = vec<i8>(0, info.bb.size());

    // Set up frontier with all exit blocks.
    deque<localidx> frontier;
    for (BasicBlock& bb : info.bb) {
        localidx id = bb.seq.back();
        Insn& insn = fn.insns[id];
        if (insn.exits()) {
            frontier.pushr(&bb - &info.bb[0]);
            bbvisited[&bb - &info.bb[0]] = 1;
        }
    }
    while (frontier.size()) {
        localidx bbid = frontier.popl();
        BasicBlock& bb = info.bb[bbid];
        for (localidx local : bbin[bbid])
            tips[local] = { -1, bb.seq.back(), true };

        for (i32 i = i32(bb.seq.size()) - 1; i >= 0; i --) {
            localidx id = bb.seq[i];
            for (const auto& entry : phi_extra_ins[id]) {
                if (tips[entry].end == -1)
                    tips[entry].end = id;
            }
            if (tips[id].end != -1) {                                       // If the variable we def is live, we add
                info.live[id].push({id, tips[id].end, tips[id].inclusive}); // an interval between the tip and the def.
                tips[id].end = -1;
                tips[id].inclusive = false;
            }
            if (bbvisited[bbid] > 1)
                continue; // Don't consider uses on later passes.

            Insn& insn = fn.insns[id];
            if (insn.op == assembler::PHI) for (i32 i = 0; i + 1 < insn.params(fn).n; i ++) {
                if (insn.params(fn)[i + 1] != P_REG) continue;
                localidx pred = insn.args(fn)[i].branch.dest;
                localidx reg = insn.args(fn)[i + 1].reg;
                if (info.ib[reg] != bbid) {
                    BasicBlock& pbb = info.bb[info.ib[reg]];
                    phi_extra_ins[pbb.seq.back()].push(reg);
                    info.live[reg].push({bb.seq[0], id, false});
                    bbvisited[info.ib[reg]] = 0;
                } else if (tips[reg].end == -1) {
                    tips[reg].end = id;
                    tips[reg].inclusive = false;
                    bbin[bbid].push(reg);
                }
            }
            else forEachArg(fn, id, fn.insns[id], PFUNC(
                if (param == P_REG && tips[arg.reg].end == -1){ // If we find a new data dependency
                    tips[arg.reg].end = id;                     // we aren't tracking, we add it.
                    tips[arg.reg].inclusive = false;
                    bbin[bbid].push(arg.reg);
                }
            ));
        }

        // Trim defined locals from bbin.
        removeIf(bbin[bbid], [&](localidx local) -> bool { return tips[local].end == -1; });

        for (localidx var : bbin[bbid]) {
            info.live[var].push({bb.seq[0], tips[var].end, tips[var].inclusive});   // Create intervals for all variables
            tips[var].end = -1;                                                     // that survive this block, and clear tips.
            tips[var].inclusive = false;
        }

        for (localidx pred : info.pred[bb.seq[0]]) {
            if (!bbvisited[info.ib[pred]] || (bbin[bbid].size() && bbvisited[info.ib[pred]] == 1)) {
                // Propagate to basic blocks only if they haven't been visited yet, or we have
                // new incoming locals they should consider.
                bbvisited[info.ib[pred]] ++;
                bbin[info.ib[pred]] = bbin[bbid];   // Propagate basic block inputs to predecessors.
                frontier.pushr(info.ib[pred]);
            }
        }

        bbvisited[bbid] = 2; // Mark as done.
        
        bbin[bbid].clear(); // Clear the local input list, so we don't revisit anything in the future.
    }

    // for (localidx local = 0; local < fn.insns.size(); local ++) {
    //     print("variable %", local, " live from ");
    //     bool first = true;
    //     for (i32 i = i32(info.live[local].size()) - 1; i >= 0; i --) {
    //         if (!first) print(", ");
    //         first = false;
    //         print('[', info.live[local][i].start, ", ", info.live[local][i].end, ']');
    //     }
    //     println();
    // }

    info.complete(PassInfo::LIVENESS);
}

pair<localidx, localidx> edge(localidx first, localidx second) {
    return { min(first, second), max(first, second) };
}

void irg(PassInfo& info, Function& fn) {
    assert(info.didPhase(PassInfo::LIVENESS));

    initInsnData(fn, info.rg, GraphNode{-1, -1});
    auto insnlive = vec<vec<localidx>, EXPECTED_INSNS>(vec<localidx>(), fn.insns.size() * 2); // List of live variables across each instruction.
    set<pair<localidx, localidx>, 64> edges;

    for (const auto& intervals : info.live) {
        for (const auto& i : intervals) {
            i32 j = i.start * 2;
            if (&intervals - &info.live[0] == i.start)
                j ++;
            i32 end = i.end * 2 + 1;
            if (!i.inclusive) end --;
            for (; j <= end; j ++)
                insnlive[j].push(&intervals - &info.live[0]);
        }
    }
    // println("interference by instruction:");
    // for (const auto& list : insnlive) {
    //     print(" ");
    //     for (localidx l : list) print(" %", l);
    //     println();
    // }

    for (i32 i = 0; i < insnlive.size(); i ++) {
        const auto& list = insnlive[i];
        for (i32 j = 0; j < list.size(); j ++) for (i32 k = j + 1; k < list.size(); k ++)
            if (list[j] != list[k]) edges.insert(edge(list[j], list[k]));
    }

    // println("edge list:");
    // for (const auto& edge : edges) {
    //     println("  %", edge.first, " <-> %", edge.second);
    // }

    initInsnData(fn, insnlive, vec<localidx>());
    for (const auto& e : edges) {
        insnlive[e.first].push(e.second);
        insnlive[e.second].push(e.first);
    }

    for (const auto& v : insnlive) {
        localidx start = info.ifer.size();
        for (localidx edge : v)
            info.ifer.push(edge);
        localidx local = &v - &insnlive[0];
        info.rg[local].idx = start;
        info.rg[local].n = info.live[local].size() ? info.ifer.size() - start : -1;
    }

    // for (const auto& node : info.rg) {
    //     if (node.n == -1) continue;
    //     localidx local = &node - &info.rg[0];
    //     print("variable %", local, " interferes with");
    //     for (localidx other : node.edges(info)) {
    //         print(" %", other);
    //     }
    //     println();
    // }

    info.complete(PassInfo::IRG);
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