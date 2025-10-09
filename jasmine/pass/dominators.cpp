#include "jasmine/mod.h"

namespace jasmine {
    void findDominators(PassContext& ctx, Function& fn) {
        JASMINE_PASS(DOMINATORS);
        ctx.did(DOMINATORS);
        auto& dominators = *ctx.dominators;

        // Initialize our immediate dominators array.

        auto& idoms = dominators.idoms;
        idoms.expandTo(fn.blockList.size(), -1);
        for (Block block : fn.blocks()) if (block.predecessorIndices().size() == 0) {
            idoms[block.index()] = block.index(); // Should handle both the entrypoint and any unreachable blocks.
        }

        // Compute the reverse postorder of the CFG.

        vec<BlockIndex> reversePostorder;
        fn.scheduleInReversePostorder(reversePostorder);

        // Compute immediate dominators and dominator sets via
        // "A Simple, Fast Dominance Algorithm" (Cooper, Harvey, Kennedy)
        // http://www.hipersoft.rice.edu/grads/publications/dom14.pdf

        // Logic to quickly find the immediate dominator of two prospective
        // immediate dominators.

        auto intersect = [&](BlockIndex a, BlockIndex b) {
            while (a != b) {
                while (a > b)
                    a = idoms[a];
                while (b > a)
                    b = idoms[b];
            }
            return a;
        };

        // Set each block's idom to the intersection of its predecessors'
        // idoms until fixpoint.

        bool fixpoint = false;
        bool firstIteration = true;
        bitset<256> visited;
        while (!fixpoint) {
            fixpoint = true;
            visited.clear();
            for (BlockIndex index : reversePostorder) {
                Block block = fn.block(index);
                if UNLIKELY(block.predecessorIndices().size() == 0) {
                    if (firstIteration)
                        idoms[index] = index, fixpoint = false;
                    visited[index] = true;
                    continue; // If a block has no predecessors, it can't be dominated by anything.
                }
                if (block.predecessorIndices().size() == 1) {
                    if (firstIteration)
                        idoms[index] = block.predecessor(0).srcIndex(), fixpoint = false;
                    visited[index] = true;
                    continue; // If a block has only one predecessor, that must be its immediate dominator.
                }
                maybe<BlockIndex> idom;
                for (auto predEdge : block.predecessors()) if (visited[predEdge.srcIndex()]) {
                    BlockIndex pred = predEdge.srcIndex();
                    if (!idom)
                        idom = some<BlockIndex>(pred);
                    else
                        idom = some<BlockIndex>(intersect(*idom, pred));
                }

                assert(idom);
                if (*idom != idoms[index])
                    idoms[index] = *idom, fixpoint = false;
                visited[index] = true;
            }
            firstIteration = false;
        }

        // Use the computed immediate dominators to compute dominance sets.

        vec<biasedset<256>> dominatorSets;
        dominatorSets.expandTo(fn.blockList.size());
        for (BlockIndex index : indices(fn.blockList)) {
            auto& set = dominatorSets[index];
            set.on(index);
            while (idoms[index] != index) {
                set.on(idoms[index]);
                index = idoms[index];
            }
        }
        for (const auto& [i, s] : enumerate(dominatorSets))
            dominators.adjacency.add(i, s);

        if UNLIKELY(config::verboseDominators) {
            for (BlockIndex index : indices(fn.blockList)) {
                print("Block bb", index);
                if (idoms[index] != -1)
                    print(" has idom bb", idoms[index], " and is dominated by");
                else
                    print(" is dominated by");
                if (!dominators.adjacency[index].empty())
                    print(" bb");
                println(SeqFormat(", bb", dominators.adjacency[index]));
            }
        }
    }
}