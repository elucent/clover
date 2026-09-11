#include "jasmine/pass.h"

namespace jasmine {
    void breakCriticalEdges(PassContext& ctx, Function& fn) {
        JASMINE_PASS(CRITICAL_EDGE_BREAKING);
        ctx.require(TYPECHECK);

        vec<BlockIndex> order;
        fn.scheduleInPreorder(order);

        for (BlockIndex bi : order) {
            Block b = fn.block(bi);
            if (b.successorIndices().size() < 2)
                continue; // If a block has only one successor, it can emit its edge inline.

            bool allFitInSuccessors = true;
            for (Edge e : b.successors()) {
                Block succ = e.dest();
                if (succ.predecessorIndices().size() >= 2) {
                    // This is a critical edge, and we need to add a block.
                    Block moves = fn.addBlock();
                    moves.addPredecessor(e);
                    b = fn.block(bi);
                    succ = e.dest();
                    succ.removePredecessor(e);
                    e.setDest(moves);
                    fn.addEdge(moves, succ);
                }
            }
        }
    }
}