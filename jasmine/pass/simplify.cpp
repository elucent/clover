#include "jasmine/mod.h"
#include "util/bits.h"
#include "util/config.h"
#include "util/heap.h"

namespace jasmine {
    void simplify(PassContext& ctx, Function& fn) {
        JASMINE_PASS(SIMPLIFICATION);
        ctx.require(TYPECHECK);

        bitset<256> reachableBlocks, newReachableBlocks, visitedBlocks;
        reachableBlocks.on(fn.entrypoint);

        bool fixpoint = false;
        while (!fixpoint) {
            fixpoint = true;
            for (Block block : fn.blocks()) {
                if (visitedBlocks[block.index()])
                    continue;
                visitedBlocks.on(block.index());

                if (!reachableBlocks[block.index()])
                    continue;
                if (block.nodeIndices().size() == 0)
                    continue;
                for (Operand operand : block.node(block.nodeIndices().size() - 1).operands()) {
                    if (operand.kind != Operand::Branch)
                        continue;
                    newReachableBlocks.on(fn.edge(operand.edge).destIndex());
                }
            }
            newReachableBlocks.removeAll(reachableBlocks);
            if (newReachableBlocks)
                fixpoint = false;
            reachableBlocks |= newReachableBlocks;
            visitedBlocks.removeAll(newReachableBlocks);
            newReachableBlocks.clear();
        }

        vec<EdgeIndex> pred, succ;
        for (Block block : fn.blocks()) {
            if (reachableBlocks[block.index()]) continue;
            pred.clear();
            succ.clear();
            for (EdgeIndex p : block.predecessorIndices()) pred.push(p);
            for (EdgeIndex p : block.successorIndices()) succ.push(p);
            for (EdgeIndex e : pred) fn.removeEdge(e);
            for (EdgeIndex e : succ) fn.removeEdge(e);
            for (Node n : block.nodes()) n.nopify();
        }

        vec<i32, 64> useCounts;
        useCounts.expandTo(fn.variableList.size(), 0);
        for (BlockIndex b : reachableBlocks) {
            Block block = fn.block(b);
            for (Node n : block.nodes()) {
                for (Operand operand : n.uses()) if (operand.kind == Operand::Var)
                    useCounts[operand.var] ++;
            }
            for (Edge e : block.successors()) for (Move m : e.moves()) if (m.src.kind == Operand::Var)
                useCounts[m.src.var] ++;
        }

        vec<NodeIndex, 16> dceNodes;
        vec<EdgeIndex, 16> dceEdges;
        bitset<128> modifiedEdges;
        for (Node n : fn.nodes()) {
            if (hasDef(n.opcode()) && !hasEffects(n.opcode()) && !useCounts[n.operand(0).var]) {
                for (Operand o : n.uses()) if (o.kind == Operand::Var) {
                    if (!--useCounts[o.var] && ctx.has(IRTrait::SSA)) {
                        auto def = (*ctx.defs)[o.var];
                        if (def >= 0)
                            dceNodes.push(def);
                        else if (def < -1)
                            dceEdges.push(-(def + 2));
                    }
                }
                n.nopify();
            }
        }
        for (Edge e : fn.edges()) for (Move& move : e.moves()) {
            if (!useCounts[move.dest.var]) {
                if (move.src.kind == Operand::Var && !--useCounts[move.src.var] && ctx.has(IRTrait::SSA)) {
                    auto def = (*ctx.defs)[move.src.var];
                    if (def >= 0)
                        dceNodes.push(def);
                    else if (def < -1)
                        dceEdges.push(-(def + 2));
                }
                move.src = fn.invalid();
                modifiedEdges.on(e.index());
            }
        }

        while (dceNodes.size() || dceEdges.size()) {
            if (dceNodes.size()) {
                Node n = fn.node(dceNodes.pop());
                assert(hasDef(n.opcode()));
                assert(!useCounts[n.operand(0).var]);
                if (!hasEffects(n.opcode())) {
                    for (Operand o : n.uses()) if (o.kind == Operand::Var) {
                        if (!--useCounts[o.var]) {
                            auto def = (*ctx.defs)[o.var];
                            if (def >= 0)
                                dceNodes.push((*ctx.defs)[o.var]);
                            else if (def < -1)
                                dceEdges.push(-(def + 2));
                        }
                    }
                    n.nopify();
                }
            }
            if (dceEdges.size()) {
                Edge e = fn.edge(dceEdges.pop());
                for (Move& move : e.moves()) if (!useCounts[move.dest.var]) {
                    if (move.src.kind == Operand::Var && !--useCounts[move.src.var]) {
                        auto def = (*ctx.defs)[move.src.var];
                        if (def >= 0)
                            dceNodes.push(def);
                        else if (def < -1)
                            dceEdges.push(-(def + 2));
                    }
                    move.src = fn.invalid();
                    modifiedEdges.on(e.index());
                }
            }
        }

        for (EdgeIndex ei : modifiedEdges) {
            Edge e = fn.edge(ei);
            e.removeMovesIf([](Move move) -> bool { return move.src.kind == Operand::Invalid; });
        }
    }
}