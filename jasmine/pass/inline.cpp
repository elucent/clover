#include "jasmine/mod.h"

namespace jasmine {
    struct InlineAction {
        enum Kind : u32 {
            VisitBlock, DecreaseDepth
        };
        Kind kind : 2;
        BlockIndex block : 30;
    };

    void inlineCalls(PassContext& ctx, Function& fn) {
        JASMINE_PASS(INLINING);
        ctx.require(TYPECHECK);

        Module* mod = fn.mod;
        u32 size = fn.nodeList.size();
        u32 depth = 0;

        i32 maxInlinableSize = config::maxInlinableSize;
        i32 maxInliningDepth = config::maxInliningDepth;

        auto shouldInline = [&](Function& other) -> bool {
            return (maxInlinableSize == -1 || other.nodeList.size() < maxInlinableSize) && other.isLeaf;
        };

        vec<InlineAction> order;
        vec<BlockIndex> reversePostorder;
        fn.scheduleInPostorder(reversePostorder);
        for (BlockIndex block : reversePostorder)
            order.push({ .kind = InlineAction::VisitBlock, .block = block });
        reversePostorder.clear();

        vec<BlockIndex> inlinedBlocks;
        vec<EdgeIndex> inlinedEdges;
        vec<i32> inlinedVariables;
        while (order.size()) {
            auto action = order.pop();
            if (action.kind == InlineAction::DecreaseDepth) {
                depth --;
                continue;
            }

            assert(action.kind == InlineAction::VisitBlock);
            Block block = fn.block(action.block);
            for (auto [i, node] : enumerate(block.nodes())) if (isCall(node.opcode())) {
                i32 calleeName = node.use(0).sym;
                auto it = mod->functionMap.find(calleeName);
                if (it == mod->functionMap.end())
                    continue; // Can't inline calls to functions we don't know about.
                Function* callee = mod->functions[it->value];
                if (!shouldInline(*callee)) {
                    if UNLIKELY(config::verboseInlining)
                        println("Didn't inline call ", node, " in\n", block);
                    continue;
                }
                Node call = node;
                Operand returnVariable = call.opcode() == Opcode::CALL_VOID ? fn.invalid() : node.operand(0);
                if UNLIKELY(config::verboseInlining)
                    println("Inlining call ", call, " in\n", block);

                // Make a copy of the inlined function's blocks, edges, and
                // variables.

                inlinedBlocks.clear();
                inlinedEdges.clear();
                inlinedVariables.clear();
                for (Block block : callee->blocks()) {
                    if (block.index() == callee->entrypoint) {
                        inlinedBlocks.push(-1);
                        continue;
                    }
                    Block newBlock = fn.addBlock();
                    if UNLIKELY(config::verboseInlining)
                        println(" - Added block bb", newBlock.index(), " to track inlined block bb", block.index());
                    for (Node node : block.nodes())
                        newBlock.addNode(fn.addNode(node));
                    inlinedBlocks.push(newBlock.index());
                }
                inlinedEdges.expandTo(callee->edgeList.size(), -1);
                for (BlockIndex index : indices(inlinedBlocks)) {
                    Block block = callee->block(index);
                    if (block.index() == callee->entrypoint)
                        continue;
                    for (Edge edge : block.successors()) {
                        Edge newEdge = fn.addEdge(inlinedBlocks[edge.srcIndex()], inlinedBlocks[edge.destIndex()]);
                        if UNLIKELY(config::verboseInlining)
                            println(" - Adding edge bb", newEdge.srcIndex(), " -> bb", newEdge.destIndex(), " to track inlined edge bb", edge.srcIndex(), " -> bb", edge.destIndex());
                        for (Move move : edge.moves())
                            newEdge.addMove(move);
                        inlinedEdges[edge.index()] = newEdge.index();
                    }
                }
                for (auto [i, v] : enumerate(callee->variableList)) {
                    inlinedVariables.push(fn.variable().var);
                    fn.variableList[inlinedVariables.back()].type = v.type;
                }

                // Next, split off a successor block from the current block. This will
                // hold all the instructions after the call.
                
                Block successor = fn.addBlock();
                if UNLIKELY(config::verboseInlining)
                    println(" - Added successor block bb", successor.index());
                for (NodeIndex node : block.nodeIndices().drop(i + 1))
                    successor.addNode(fn.node(node));
                Node successorEnd = fn.node(successor.nodeIndices().last());
                for (Operand& operand : successorEnd.operands()) if (operand.kind == Operand::Branch) {
                    Edge edge = fn.edge(operand.edge);
                    Edge newEdge = fn.addEdge(successor, edge.destIndex());
                    edge.dest().removePredecessor(edge.index());
                    operand.edge = newEdge.index();
                }
                block.removeAllSuccessors();

                // Now trim off the call and all subsequent nodes from our current block.
                // Then, pass parameters, and add all nodes from the inlined callee's
                // entrypoint block. This saves us a spare block.

                block.shrinkTo(i);
                inlinedBlocks[callee->entrypoint] = block.index();
                for (auto [i, operand] : enumerate(call.uses().drop(1)))
                    block.addNode(Opcode::MOV, fn.typeContext()[call.type()].arguments()[i], fn.variableById(inlinedVariables[callee->parameters[i].operand.var]), operand);
                u32 firstCalleeEntryNode = block.nodeIndices().size();
                Block calleeEntry = callee->block(callee->entrypoint);
                for (Node node : calleeEntry.nodes())
                    block.addNode(fn.addNode(node));
                assert(calleeEntry.predecessorIndices().size() == 0);
                for (Edge edge : calleeEntry.successors()) {
                    Edge newEdge = fn.addEdge(block, inlinedBlocks[edge.destIndex()]);
                    for (Move move : edge.moves())
                        newEdge.addMove(move);
                    inlinedEdges[edge.index()] = newEdge.index();
                }

                // Update uses of the old edges and variables to the new ones,
                // and additionally replace all return points with branches to
                // the successor.

                if UNLIKELY(config::verboseInlining)
                    println("Function before updates:\n", fn);

                for (BlockIndex index : inlinedBlocks) {
                    Block block = fn.block(index);
                    auto bias = index == inlinedBlocks[calleeEntry.index()] ? firstCalleeEntryNode : 0;
                    auto nodes = block.nodeIndices().drop(bias);
                    for (auto [i, n] : enumerate(nodes)) {
                        Node node = fn.node(n);
                        if UNLIKELY(config::verboseInlining)
                            println("Processing node ", node);
                        for (Operand& operand : node.operands()) {
                            if (operand.kind == Operand::Var)
                                operand = fn.variableById(inlinedVariables[operand.var]);
                            else if (operand.kind == Operand::Branch) {
                                assert(operand.edge < inlinedEdges.size());
                                assert(inlinedEdges[operand.edge] < fn.edgeList.size());
                                if UNLIKELY(config::verboseInlining)
                                    println(" - Replaced edge ", operand.edge, " bb", fn.edge(operand.edge).srcIndex(), " -> bb", fn.edge(operand.edge).destIndex(), " with new edge ", inlinedEdges[operand.edge], " bb", fn.edge(inlinedEdges[operand.edge]).srcIndex(), " -> bb", fn.edge(inlinedEdges[operand.edge]).destIndex());
                                operand = fn.branch(inlinedEdges[operand.edge]);
                            }
                        }
                        if (node.opcode() == Opcode::RET) {
                            if (returnVariable.kind != Operand::Invalid) {
                                assert(node.opcode() == Opcode::CALL);
                                fn.addInsertion(block, i + bias);
                                fn.addNodeToInsertion(fn.addNode(Opcode::MOV, callee->returnType, returnVariable, node.operand(0)));
                            }
                            Edge returnEdge = fn.addEdge(block, successor);
                            if UNLIKELY(config::verboseInlining)
                                println(" - Added new edge ", returnEdge.index(), " bb", returnEdge.srcIndex(), " -> bb", returnEdge.destIndex(), " in place of return");
                            block.replaceNode(i + bias, Opcode::BR, VOID, fn.branch(returnEdge));
                            continue;
                        }
                    }
                }
                fn.executeInsertions();

                if UNLIKELY(config::verboseInlining)
                    println("Function after inlining:\n", fn);

                // Finally, we add the remaining work to the worklist.

                order.push({ .kind = InlineAction::VisitBlock, .block = successor.index() });
                order.push({ .kind = InlineAction::DecreaseDepth, .block = 0 });
                for (BlockIndex i : inlinedBlocks)
                    order.push({ .kind = InlineAction::VisitBlock, .block = i });
                depth ++;

                // We just split this block into two pieces, so we shouldn't
                // have anything left to iterate in it.
                break;
            }
        }
    }
}