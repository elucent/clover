#include "jasmine/pass.h"
#include "jasmine/mod.h"

namespace jasmine {
    void scheduleNodesTopologically(Function& fn, const vec<i32, 128>& defs, vec<NodeIndex, 32>& schedule, NodeIndex ni, const bitset<256>& loopNodes, bitset<256>& visited) {
        Node node = fn.node(ni);
        for (Operand o : node.uses()) if (o.kind == Operand::Var) {
            auto def = defs[o.var];
            if (def >= 0 && loopNodes[def] && !visited[def]) {
                visited.on(def);
                scheduleNodesTopologically(fn, defs, schedule, def, loopNodes, visited);
            }
        }
        schedule.push(ni);
    }

    void ensurePreHeader(Function& fn, Loop* loop, PassContext::BlockLoops& blockLoops, bitset<128>& isPreHeader) {
        if (loop->parent && !loop->parent->preHeaderIndex)
            ensurePreHeader(fn, loop->parent, blockLoops, isPreHeader);

        Block header = loop->header(fn);

        // See if we have a unique predecessor outside our loop already.

        i32 singlePredecessor = -1;
        for (Edge pred : header.predecessors()) {
            if (loop->blocks[pred.srcIndex()])
                continue;
            if (singlePredecessor == -1)
                singlePredecessor = pred.srcIndex();
            else
                singlePredecessor = -2;
        }

        if (singlePredecessor >= 0 && !isPreHeader[singlePredecessor]) {
            // If we already have a block that always precedes our header, and
            // isn't the preheader of any other loop, then use that.
            loop->preHeaderIndex = some<u32>(singlePredecessor);
            isPreHeader.on(*loop->preHeaderIndex);
            assert(!loop->parent || header.index() != loop->parent->headerIndex);
            return;
        }

        Block preHeader = fn.addBlock();
        loop->preHeaderIndex = some<u32>(preHeader.index());
        blockLoops.push(loop->parent ? loop->parent->index : -1);
        if (loop->parent && header.index() == loop->parent->headerIndex) {
            // If we share a header with our parent, we need to change their
            // header to our new preheader. We only need to look at our
            // immediate parent, because if its parent was in a similar
            // situation, it should have already moved its backedges.
            loop->parent->headerIndex = preHeader.index();
        }
        for (Loop* parent = loop->parent; parent; parent = parent->parent)
            parent->blocks.on(preHeader.index());

        vec<EdgeIndex, 8> toRemove;
        for (Edge pred : header.predecessors()) if (!loop->blocks[pred.srcIndex()]) {
            // All predecessors that originate outside our loop are moved to
            // the preheader. Note that this includes backedges from parent
            // loops that share our header.

            pred.setDest(preHeader);
            toRemove.push(pred.index());
            preHeader.addPredecessor(pred);
            pred.src().removeSuccessor(header.index());
            pred.src().addSuccessor(preHeader.index());
        }
        for (EdgeIndex e : toRemove)
            header.removePredecessor(e);

        preHeader.addNode(Opcode::BR, VOID, fn.branch(preHeader, header));

        if (fn.entrypoint == header.index())
            fn.entrypoint = preHeader.index();
    }

    void hoistLoopInvariantCode(PassContext& ctx, Function& fn) {
        JASMINE_PASS(HOIST_LOOP_INVARIANTS);
        ctx.require(DOMINATORS, NATURAL_LOOPS, EFFECTS);

        auto& dominators = *ctx.dominators;
        auto& loops = *ctx.loops;
        auto& blockLoops = *ctx.blockLoops;
        auto& effectsByLoop = *ctx.effectsByLoop;
        auto& effectsByInstruction = *ctx.effectsByInstruction;
        auto& effectsByBlock = *ctx.effectsByBlock;
        auto& defs = *ctx.defs;

        // Ensure each loop has a unique preheader block that we can hoist
        // instructions into.

        bitset<128> isPreHeader;

        for (Loop* loop : loops)
            ensurePreHeader(fn, loop, blockLoops, isPreHeader);

        // Figure out which variables are defined in which loops. We count
        // any variable defined by an instruction in the loop, and any variable
        // defined by a predecessor edge of any block in the loop. In the same
        // traversal, we also compute the set of nodes used in any loop.

        bitset<256> loopNodes;
        vec<BlockIndex, 128> nodeBlocks;
        nodeBlocks.expandTo(fn.nodeList.size(), -1);

        vec<biasedset<128>> definedInLoop;
        definedInLoop.expandTo(loops.size());

        for (Loop* loop : loops) for (BlockIndex bi : loop->blocks) {
            Block block = fn.block(bi);
            for (Node node : block.nodes()) {
                if (hasDef(node.opcode()))
                    definedInLoop[loop->index].on(node.def(0).var);
                loopNodes.on(node.index());
                nodeBlocks[node.index()] = block.index();
            }
            if (block.predecessorIndices().size()) {
                // Just check the first predecessor, since they all must define
                // the same variables.
                for (Move move : block.predecessor(0).moves())
                    definedInLoop[loop->index].on(move.dest.var);
            }
        }

        // Figure out which nodes in loops are leaves, that is to say no other
        // node in any loop uses their result.

        bitset<256> leafNodes;
        leafNodes.addAll(loopNodes);

        for (NodeIndex ni : loopNodes) {
            Node node = fn.node(ni);
            for (auto use : node.uses()) if (use.kind == Operand::Var) {
                i32 def = defs[use.var];
                if (def != -1 && loopNodes[def])
                    leafNodes.off(def);
            }
        }

        // Starting from the leaf nodes, compute a topological order of all
        // nodes that appear in loops. This will be the order we try to hoist
        // them in.

        vec<NodeIndex, 32> topologicalOrder;
        bitset<256> visited;
        for (auto i : leafNodes) {
            visited.on(i);
            scheduleNodesTopologically(fn, defs, topologicalOrder, i, loopNodes, visited);
        }

        // Now, try hoisting each node.

        vec<biasedset<128>> toRemove;
        toRemove.expandTo(fn.blockList.size());

        for (NodeIndex ni : topologicalOrder) {
            Node node = fn.node(ni);

            if (isBranch(node.opcode()))
                continue; // We don't hoist branches here.

            LoopIndex li = blockLoops[nodeBlocks[ni]];
            assert(li != -1);

            Loop* loop = loops[li];
            Loop* targetLoop = nullptr;
            Loop* iterLoop = loop;
            while (iterLoop) {
                // We can't hoist a node if it depends on any variable defined
                // in the loop.
                bool canHoist = true;
                for (Operand use : node.uses()) if (use.kind == Operand::Var && definedInLoop[iterLoop->index][use.var])
                    canHoist = false;
                if (!canHoist)
                    break;

                // We can't hoist a node if it reads anything the loop writes,
                // or if it writes anything the loop also writes. It's fine if
                // it writes something the loop reads, since if the loop
                // doesn't also write that thing, we know the value won't have
                // otherwise changed between the loop body and preheader.
                if (effectsByLoop.writesOf(iterLoop->index).intersects(effectsByInstruction.readsOf(node.index())))
                    break;
                if (effectsByLoop.writesOf(iterLoop->index).intersects(effectsByInstruction.writesOf(node.index())))
                    break;

                targetLoop = iterLoop;
                iterLoop = iterLoop->parent;
            }

            // If targetLoop is not null, we found a loop to hoist to.

            if (!targetLoop)
                continue;

            if (hasDef(node.opcode())) {
                definedInLoop[loop->index].off(node.def(0).var);
                definedInLoop[targetLoop->index].on(node.def(0).var);
            }
            Block dst = fn.block(*targetLoop->preHeaderIndex);
            fn.addInsertion(dst, dst.nodeIndices().size() - 1);
            fn.addNodeToInsertion(node);
            toRemove[nodeBlocks[ni]].on(ni);

            if UNLIKELY(config::verboseLICM)
                println("[LICM]\tHoisted node ", node, " from bb", nodeBlocks[ni], " (loop ", loop->index, ") to bb", dst.index(), " (loop ", targetLoop->index, ")");
        }

        fn.executeInsertions();

        for (const auto& [bi, s] : enumerate(toRemove)) {
            if (s.empty())
                continue;
            Block block = fn.block(bi);
            block.removeIf([&](Node node) -> bool { return s[node.index()]; });
        }

        ctx.invalidate(EFFECTS); // Effects have now changed, since we've reordered a bunch of nodes.
    }
}