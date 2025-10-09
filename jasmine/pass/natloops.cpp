#include "jasmine/mod.h"

namespace jasmine {
    void computeDepth(Loop* loop) {
        if (!loop->parent) {
            loop->depth = 0;
            return;
        }
        if (loop->parent->depth < 0)
            computeDepth(loop->parent);
        loop->depth = loop->parent->depth + 1;
    }

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
            loops.back()->isLeaf = true;
            loops.back()->depth = -1;
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
        for (auto [i, l] : enumerate(loops)) {
            blockLoops[l->headerIndex] = i;
        }

        bitset<128> blockSet;
        for (Loop* loop : loops) {
            blockSet.clear();
            blockSet.addAll(loop->blocks);
            blockSet.off(loop->headerIndex);

            for (BlockIndex bi : blockSet) if (blockLoops[bi] != -1) {
                // We found a loop header in our own loop, so it must belong to
                // one of our children.

                Loop* child = loops[blockLoops[bi]];
                blockSet.removeAll(child->blocks);
            }

            // We should have excluded all of the blocks belonging to our child
            // loops. The remainder must belong to us.
            for (BlockIndex bi : blockSet)
                blockLoops[bi] = loop->index;
        }

        // Now that we've populated the loops for each block, finding the
        // immediate parent loop of each loop is as simple as finding the loop
        // of the immediate dominator of each loop header.

        for (Loop* loop : loops) {
            auto idom = dominators[loop->headerIndex].immediateDominator();
            if (idom && blockLoops[*idom] != -1) {
                loop->parent = loops[blockLoops[*idom]];
                continue;
            }

            // Otherwise, the loop's parent should already have been
            // initialized to nullptr.
            assert(!loop->parent);
        }

        // We need one last cleanup: if two or more loops share a header, we
        // require that the blockLoops for the header block is the innermost
        // loop.

        for (Loop* loop : loops) {
            if (blockLoops[loop->headerIndex] != loop->index) {
                Loop* parent = loop->parent;
                while (parent && blockLoops[loop->headerIndex] != parent->index)
                    parent = parent->parent;
                if (parent) // The current owner of the block is our parent.
                    blockLoops[loop->headerIndex] = loop->index;

                // Otherwise, for us to share a header with them, they must be
                // our child. We don't change the current assignment since they
                // must be a more deeply nested loop than us.
            }
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