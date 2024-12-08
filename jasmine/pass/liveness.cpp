#include "jasmine/mod.h"
#include "util/config.h"
#include "util/deque.h"

namespace jasmine {
    struct LivenessState {
        BlockIndex block;
        bitset<128> liveAtTail;
        vec<LiveRange> ranges;
    };

    void liveness(PassContext& ctx, Function& fn) {
        JASMINE_PASS(LIVENESS);
        ctx.require(SSA);
        ctx.did(LIVENESS);

        vec<LivenessState, 16> states;
        for (Block block : fn.blocks())
            states.push({ block.index(), {} });

        bool fixpoint = false;
        deque<BlockIndex> blocks;
        for (Block block : fn.blocks()) {
            if (block.successorIndices().size() == 0)
                blocks.pushl(block.index());
        }
        vec<i32> tips;
        for (auto var : fn.variableList)
            tips.push(-1);
        bool isFirstPass = true;
        bitset<128> backEdgeTargets;
        bitset<128> live;

        const auto& pins = ctx.pins.get();

        while (!fixpoint) {
            fixpoint = true;
            bitset<128> visited;

            // Breadth-first traversal going backwards.
            while (blocks.size()) {
                Block block = fn.block(blocks.popr());
                visited.on(block.index());

                u32 blockLength = block.nodeIndices().size();

                auto& state = states[block.index()];
                state.ranges.clear();

                live.addAll(state.liveAtTail);
                for (u32 var : live)
                    tips[var] = blockLength + 1;

                // Source operands in edges to successors count as uses.

                for (Edge edge : block.successors()) {
                    for (Move move : edge.moves()) if (move.src.kind == Operand::Var) {
                        live.on(move.src.var);
                        tips[move.src.var] = blockLength + 1;
                    }
                }

                // Traverse instructions backwards.

                for (i64 i = i64(blockLength) - 1; i >= 0; i --) {
                    Node n = block.node(i);
                    for (Operand o : n.defs()) {
                        assert(o.kind == Operand::Var);
                        if (tips[o.var] != -1 && !pins.isPinned(o.var)) {
                            state.ranges.push(LiveRange { o.var, (u16)(i + 1), (u16)tips[o.var], -1, -1 });
                            tips[o.var] = -1;
                            live.off(o.var);
                        }
                    }
                    for (Operand o : n.uses()) if (o.kind == Operand::Var && !pins.isPinned(o.var)) {
                        live.on(o.var);
                        if (tips[o.var] == -1)
                            tips[o.var] = i + 1;
                    }
                }

                // Dest operands in edges from predecessors count as defs.

                for (Edge edge : block.predecessors()) {
                    for (Move move : edge.moves()) {
                        assert(move.dest.kind == Operand::Var);
                        if (tips[move.dest.var] != -1) {
                            assert(!pins.isPinned(move.dest.var));
                            state.ranges.push(LiveRange { move.dest.var, 0, (u16)tips[move.dest.var], -1, -1 });
                            tips[move.dest.var] = -1;
                            live.off(move.dest.var);
                        }
                    }
                }

                for (u32 var : live) {
                    assert(tips[var] != -1);
                    state.ranges.push(LiveRange { var, (u16)0, (u16)tips[var], -1, -1 });
                    tips[var] = -1;
                }

                for (Edge edge : block.predecessors()) {
                    Block target = edge.src();
                    auto& targetLiveAtTail = states[target.index()].liveAtTail;
                    if (visited[edge.srcIndex()])
                        backEdgeTargets.on(edge.srcIndex());
                    if (targetLiveAtTail.addAll(live)) {
                        fixpoint = false;
                        if (!visited[target.index()])
                            blocks.pushl(target.index());
                    } else if (isFirstPass) {
                        // If the predecessor didn't change, we won't revisit it, unless this
                        // is our first pass through; we want to make sure we traverse the CFG
                        // at least once.
                        if (!visited[target.index()])
                            blocks.pushl(target.index());
                    }
                }
                live.clear();
            }

            if (!fixpoint) {
                for (u32 backEdgeTarget : backEdgeTargets)
                    blocks.pushl(backEdgeTarget);
                if (blocks.size() == 0) { // If we don't have any back-edges, we can quit after our first pass.
                    assert(isFirstPass);
                    break;
                }
            }

            isFirstPass = false;
        }

        // Thread successor/sibling relations through the live ranges.
        // It's unclear if this is worth the cost to do it here. Probably?

        auto& liveness = *ctx.liveness;
        liveness.clear();
        for (LivenessState& state : states)
            sort(state.ranges, [](const LiveRange& a, const LiveRange& b) -> bool { return a.start < b.start; });
        liveness.clear();
        for (const auto& [i, s] : enumerate(states))
            liveness.addBlock(i, s.ranges);
        
        // Ranges within a block are now guaranteed to be in increasing order
        // by starting instruction. We use this to (somewhat) efficiently
        // thread successor and sibling relations through the liveness graph.

        for (BlockIndex i : indices(fn.blockList)) {
            Block block = fn.block(i);

            #ifndef RELEASE
            for (i32 t : tips) assert(t == -1);
            assert(live.begin() == live.end());
            #endif

            // Stitch together any internal live ranges within the block.

            for (LiveRangeIndex rangeIndex : liveness.indicesInBlock(i)) {
                LiveRange& range = liveness[rangeIndex];
                i32& tip = tips[range.var];
                if (tip == -1) {
                    tips[range.var] = rangeIndex;
                    live.on(range.var);
                } else {
                    LiveRange& prev = liveness[tips[range.var]];
                    if (prev.end == range.start)
                        prev.successor = rangeIndex;
                    tips[range.var] = rangeIndex;
                }
            }

            // Figure out which variables are live past the end of the block.

            for (u32 var : live)
                tips[var] = -1;
            live.clear();

            for (LiveRangeIndex rangeIndex : liveness.indicesInBlock(i)) {
                if (liveness[rangeIndex].end != block.nodeIndices().size() + 1)
                    continue;
                auto var = liveness[rangeIndex].var;
                live.on(var);
                tips[var] = rangeIndex;
            }

            // Now we try to connect variables through to their successors.
            // To be honest... this seems super hairy, probably breaks on
            // certain unstructured control flow. I pretend I do not see it.

            for (Edge edge : fn.block(i).successors()) {
                for (LiveRangeIndex succIndex : liveness.indicesInBlock(edge.destIndex())) {
                    LiveRange& range = liveness[succIndex];
                    if (range.start) // We are only concerned with live ranges at the head of the block.
                        break;
                    if (live[range.var]) {
                        LiveRange& prev = liveness[tips[range.var]];
                        if (prev.successor != -1)
                            liveness[prev.successor].sibling = succIndex;
                        prev.successor = succIndex;
                        edge.addMove({ fn.variableById(range.var), fn.variableById(range.var) });
                    }
                }
            }

            // Clean up for next iteration.

            for (u32 var : live)
                tips[var] = -1;
            live.clear();
        }

        if UNLIKELY(config::verboseLiveness) {
            for (BlockIndex block : indices(fn.blockList)) {
                println("[LIVE]\tLive ranges in block ", block, ":");
                for (const LiveRange& range : liveness.rangesInBlock(block)) {
                    println("[LIVE]\t - ", OperandLogger { fn, fn.variableById(range.var) }, " live from ", range.start, " to ", range.end);
                }
            }
        }
    }
}