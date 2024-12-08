#include "jasmine/pass.h"
#include "jasmine/mod.h"

namespace jasmine {
    void containment(PassContext& ctx, Function& fn) {
        JASMINE_PASS(CONTAINMENT);
        ctx.require(LIVENESS);
        ctx.did(INTERFERENCE);
        ctx.did(CONTAINMENT);

        using Set = bitset<128>;

        auto& liveness = *ctx.liveness;
        vec<Set, 64> interferenceList, containmentList;

        // We use a biased representation to add special "edge" variables to
        // the interference graph. These represent the resources needed to
        // execute a set of moves for an edge, and interfere with all variables
        // involved with said moves. Edge indices are identified by being
        // larger than all live range indices.

        u32 edgeOffset = liveness.ranges.size();
        for (u32 i = 0; i < liveness.ranges.size(); i ++)
            interferenceList.push({}), containmentList.push({});
        for (u32 i = 0; i < fn.edgeList.size(); i ++)
            interferenceList.push({}), containmentList.push({});

        // Create new "edge variables" representing each edge, and make any
        // variables involved in an edge move interfere with it.

        bitset<128> edgeVariables;
        for (Edge edge : fn.edges()) {
            edgeVariables.clear();
            for (Move move : edge.moves()) if (move.src.kind == Operand::Var)
                edgeVariables.on(move.src.var);
            auto predRanges = liveness.indicesInBlock(edge.srcIndex());
            u32 srcEnd = edge.src().nodeIndices().size() + 1;
            for (LiveRangeIndex i : predRanges) {
                const LiveRange& range = liveness[i];
                if (range.end != srcEnd) // Only consider ranges live through the end.
                    continue; // TODO: This should be a break.
                if (edgeVariables[range.var])
                    interferenceList[edgeOffset + edge.index()].on(i), interferenceList[i].on(edgeOffset + edge.index());
            }
            edgeVariables.clear();
            for (Move move : edge.moves()) if (move.dest.kind == Operand::Var)
                edgeVariables.on(move.dest.var);
            auto succRanges = liveness.indicesInBlock(edge.destIndex());
            for (LiveRangeIndex i : succRanges) {
                const LiveRange& range = liveness[i];
                if (range.start != 0) // Only consider ranges live at the head.
                    continue;
                if (edgeVariables[range.var])
                    interferenceList[edgeOffset + edge.index()].on(i), interferenceList[i].on(edgeOffset + edge.index());
            }
        }

        bitset<128> live;
        for (Block block : fn.blocks()) {
            live.clear();
            const auto& rangeIndices = liveness.indicesInBlock(block.index());

            LiveRangeIndex last = -1;
            for (LiveRangeIndex index : rangeIndices) {
                const LiveRange& range = liveness[index];
                if (range.start > last) {
                    for (LiveRangeIndex r : live) if (liveness[r].end <= range.start)
                        live.off(r);
                    last = range.start;
                }
                for (u32 r : live)
                    interferenceList[index].on(r), interferenceList[r].on(index);
                live.on(index);
            }
        }

        for (const auto& [i, s] : enumerate(interferenceList)) {
            if (i >= edgeOffset)
                break;
            for (LiveRangeIndex j : s) {
                if (j >= edgeOffset)
                    continue;
                if (liveness[i].start <= liveness[j].start
                    && liveness[i].end >= liveness[j].end)
                    containmentList[i].on(j);
            }
        }

        auto &interference = *ctx.interference, &containment = *ctx.containment;
        interference.clear();
        containment.clear();
        for (const auto& [i, s] : enumerate(interferenceList))
            interference.add(i, s);
        for (const auto& [i, s] : enumerate(containmentList))
            containment.add(i, s);

        if UNLIKELY(config::verboseInterference) {
            for (Block block : fn.blocks()) {
                for (LiveRangeIndex i : liveness.indicesInBlock(block.index())) {
                    LiveRange& ri = liveness[i];
                    print("[GRAPH]\tLive range ", OperandLogger { fn, fn.variableById(ri.var) }, "[bb", block.index(), ":", ri.start, ":", ri.end, "] interferes with:");
                    for (LiveRangeIndex o : interference[i]) {
                        if (o >= edgeOffset) {
                            Edge edge = fn.edge(o - edgeOffset);
                            print(" edge[bb", edge.srcIndex(), "->bb", edge.destIndex(), "]");
                        } else {
                            LiveRange& ro = liveness[o];
                            print(" ", OperandLogger { fn, fn.variableById(ro.var) }, "[bb", block.index(), ":", ro.start, ":", ro.end, "]");
                        }
                    }
                }
            }
        }

        if UNLIKELY(config::verboseContainment) {
            for (Block block : fn.blocks()) {
                for (LiveRangeIndex i : liveness.indicesInBlock(block.index())) {
                    LiveRange& ri = liveness[i];
                    print("[GRAPH]\tLive range ", OperandLogger { fn, fn.variableById(ri.var) }, "[bb", block.index(), ":", ri.start, ":", ri.end, "] contains:");
                    for (LiveRangeIndex o : containment[i]) {
                        LiveRange& ro = liveness[o];
                        print(" ", OperandLogger { fn, fn.variableById(ro.var) }, "[bb", block.index(), ":", ro.start, ":", ro.end, "]");
                    }
                }
            }
        }

        if UNLIKELY(config::verboseInterference || config::verboseContainment)
            println(fn);
    }
}