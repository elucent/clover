#include "jasmine/mod.h"

namespace jasmine {
    void findNaturalLoops(PassContext& ctx, Function& fn) {
        JASMINE_PASS(NATURAL_LOOPS);
        ctx.require(DOMINATORS);
        ctx.did(NATURAL_LOOPS);

        const auto& dominators = *ctx.dominators;

        // First, find all the back-edges in the function - we only consider
        // natural loops in reducible control flow, so back-edges are simply
        // all edges that target a dominator.

        vec<EdgeIndex> backEdges;
        for (Block block : fn.blocks()) for (Edge succ : block.successors()) if (dominators[succ.dest()].dominates(block)) {
            backEdges.push(succ.index());
        }

        // Next, we track the unique blocks in the function targeted by at
        // least one back-edge. Each of these is the header to a different
        // loop in our function.

        bitset<256> loopHeaders;
        for (EdgeIndex edge : backEdges)
            loopHeaders.on(fn.edge(edge).destIndex());
        
        // Initialize the loop data structures, reiterating the back-edges to
        // track the list of them per-loop.

        auto& loops = *ctx.loops;
        assert(loops.size() == 0);

        for (BlockIndex block : loopHeaders) {
            loops.push(new Loop());
            loops.back()->index = loops.size() - 1;
            loops.back()->headerIndex = block;
            for (EdgeIndex edge : backEdges) {
                if (fn.edge(edge).destIndex() == block)
                    loops.back()->backEdges.push(edge);
            }
            loops.back()->blocks.on(block); // The header is always part of the loop.
        }

        // Now, we find the members of each loop - we do a reverse-reachability
        // analysis, from the source of each backedge, to the loop header. All
        // reachable blocks in between must belong to the loop identified by
        // that header.

        for (Loop* loop : loops) {
            vec<BlockIndex> frontier;
            for (EdgeIndex edge : loop->backEdges)
                frontier.push(fn.edge(edge).srcIndex());
            while (frontier.size()) {
                BlockIndex next = frontier.pop();
                if (loop->blocks[next])
                    continue;
                loop->blocks.on(next);
                for (Edge edge : fn.block(next).predecessors()) {
                    if (edge.srcIndex() != loop->headerIndex && !loop->blocks[edge.srcIndex()])
                        frontier.push(edge.srcIndex());
                }
            }
        }

        // Find the nearest loop enclosing each block, and find the parent of
        // each loop if it exists.

        auto& blockLoops = *ctx.blockLoops;
        blockLoops.expandTo(fn.blockList.size(), -1);
        for (auto [i, l] : enumerate(loops))
            blockLoops[l->headerIndex] = i;
        for (Block block : fn.blocks()) {
            BlockIndex idom = block.index();
            bool lookingForParent = loopHeaders[idom]; // If we're already at a loop header, we're looking for the enclosing loop instead.
            do {
                idom = dominators[idom].immediateDominator();
                if (loopHeaders[idom]) {
                    if (lookingForParent && idom != block.index())
                        loops[blockLoops[block.index()]]->parent = loops[blockLoops[idom]];
                    else
                        blockLoops[block.index()] = blockLoops[idom];
                    break;
                }
            } while (idom != dominators[block].immediateDominator());
        }

        if UNLIKELY(config::verboseNaturalLoops) {
            println("Found ", loops.size(), " loops:");
            for (auto [i, loop] : enumerate(loops)) {
                println(" - Loop ", i + 1, ":");
                println("    - Header: bb", loop->headerIndex);
                if (loop->preHeaderIndex)
                    println("    - Pre-header: bb", loop->preHeaderIndex);
                println("    - Blocks: bb", SeqFormat(", bb", loop->blocks));
                if (loop->parent)
                    println("    - Parent: Loop ", loop->parent->index + 1);
            }
        }
    }
}