#include "jasmine/mod.h"
#include "util/bits.h"
#include "util/config.h"
#include "util/sort.h"

namespace jasmine {
    void cleanup(PassContext& ctx, Function& fn) {
        JASMINE_PASS(CLEANUP);

        vec<i32, 64> nodeRenumberings, blockRenumberings, edgeRenumberings;
        i32 newBlocks = 0, newNodes = 0, newEdges = 0;
        bitset<256> blockReachability;

        // Compute reachable blocks via preorder traversal of the CFG.
        vec<BlockIndex> blockTraversal;
        fn.scheduleInPreorder(blockTraversal);
        for (BlockIndex b : blockTraversal)
            blockReachability.on(b);

        // Give new indices to all reachable blocks. It's important that this maintain
        // the existing order, so we can use cleanup on post-lowering code.
        for (Block b : fn.blocks()) {
            if (blockReachability[b.index()])
                blockRenumberings.push(newBlocks ++);
            else
                blockRenumberings.push(-1);
        }

        // Give new indices to all edges between reachable blocks.
        for (Edge e : fn.edges()) {
            if (blockReachability[e.srcIndex()] && blockReachability[e.destIndex()])
                edgeRenumberings.push(newEdges ++);
            else
                edgeRenumberings.push(-1);
        }

        // Give new indices to all non-nop nodes.
        for (Node n : fn.nodes()) {
            if (n.opcode() != Opcode::NOP)
                nodeRenumberings.push(newNodes ++);
            else
                nodeRenumberings.push(-1);
        }

        bool removedAnyNodes = newNodes != fn.nodeList.size() || config::neverSkipCleanup, 
            removedAnyBlocks = newBlocks != fn.blockList.size() || config::neverSkipCleanup,
            removedAnyEdges = newEdges != fn.edgeList.size() || config::neverSkipCleanup;

        // Fix up block references in edges.
        if (removedAnyBlocks) for (Edge e : fn.edges()) {
            e.header().src = blockRenumberings[e.header().src];
            e.header().dest = blockRenumberings[e.header().dest];
        }

        // Fix up node and edge references in blocks.
        for (Block b : fn.blocks()) {
            if (removedAnyEdges) {
                for (EdgeIndex& pred : b.predecessorIndices()) {
                    auto newPred = edgeRenumberings[pred];
                    if (newPred == -1)
                        b.removePredecessor(pred);
                    else
                        pred = newPred;
                }
                for (EdgeIndex& succ : b.successorIndices()) {
                    auto newSucc = edgeRenumberings[succ];
                    if (newSucc == -1)
                        b.removeSuccessor(succ);
                    else
                        succ = newSucc;
                }

                // We assume only the last node can be a branch.
                for (Operand& o : b.last().operands()) if (o.kind == Operand::Branch)
                    o.edge = edgeRenumberings[o.edge];
            }
            
            if (removedAnyNodes) {
                b.removeIf([&](Node node) -> bool { return node.opcode() == Opcode::NOP; });
                for (NodeIndex& n : b.nodeIndices())
                    n = nodeRenumberings[n];
            }
        }

        // Remove all desired nodes, blocks, and edges from the function's top-level lists.
        if (removedAnyBlocks) {
            BlockIndex i = 0;
            fn.blockList.removeIf([&](u32) -> bool { return !blockReachability[i ++]; });
        }
        if (removedAnyEdges) {
            EdgeIndex i = 0;
            fn.edgeList.removeIf([&](u32) -> bool { return edgeRenumberings[i ++] == -1; });
        }
        if (removedAnyNodes) {
            NodeIndex i = 0;
            fn.nodeList.removeIf([&](u32) -> bool { return fn.node(i ++).opcode() == Opcode::NOP; });
        }

        // We are done renumbering, so we can reuse these vectors.
        // We sort the indices in order of increasing storage address. We'll
        // use this to efficiently compact the storage of each of these types
        // in the function we are cleaning.
        if (removedAnyNodes) {
            nodeRenumberings.clear();
            for (u32 i = 0; i < fn.nodeList.size(); i ++)
                nodeRenumberings.push(i);
            sort(nodeRenumberings, [&](NodeIndex a, NodeIndex b) -> bool { return fn.nodeList[a] < fn.nodeList[b]; });
        }
        if (removedAnyBlocks) {
            blockRenumberings.clear();
            for (u32 i = 0; i < fn.blockList.size(); i ++)
                blockRenumberings.push(i);
            sort(blockRenumberings, [&](BlockIndex a, BlockIndex b) -> bool { return fn.blockList[a] < fn.blockList[b]; });
        }
        if (removedAnyEdges) {
            edgeRenumberings.clear();
            for (u32 i = 0; i < fn.edgeList.size(); i ++)
                edgeRenumberings.push(i);
            sort(edgeRenumberings, [&](EdgeIndex a, EdgeIndex b) -> bool { return fn.edgeList[a] < fn.edgeList[b]; });
        }

        // Now, we shift all the words back.
        if (removedAnyNodes) {
            NodeWord* writer = fn.nodeWords.data();
            for (NodeIndex n : nodeRenumberings) {
                Node node = fn.node(n);
                u32 head = node.headidx;
                u32 size = node.wordCount();
                fn.nodeList[n] = writer - fn.nodeWords.data(); // Adjust head.
                for (u32 i = 0; i < size; i ++)
                    *writer ++ = fn.nodeWords[head + i];
            }
            fn.nodeWords._size = writer - fn.nodeWords.data();
        }
        if (removedAnyBlocks) {
            BlockWord* writer = fn.blockWords.data();
            for (BlockIndex n : blockRenumberings) {
                Block block = fn.block(n);
                u32 head = block.headidx;
                u32 size = block.wordCount();
                fn.blockList[n] = writer - fn.blockWords.data(); // Adjust head.
                for (u32 i = 0; i < size; i ++)
                    *writer ++ = fn.blockWords[head + i];
            }
            fn.blockWords._size = writer - fn.blockWords.data();
        }
        if (removedAnyEdges) {
            EdgeWord* writer = fn.edgeWords.data();
            for (EdgeIndex n : edgeRenumberings) {
                Edge edge = fn.edge(n);
                u32 head = edge.headidx;
                u32 size = edge.wordCount();
                fn.edgeList[n] = writer - fn.edgeWords.data(); // Adjust head.
                for (u32 i = 0; i < size; i ++)
                    *writer ++ = fn.edgeWords[head + i];
            }
            fn.edgeWords._size = writer - fn.edgeWords.data();
        }
    }
}