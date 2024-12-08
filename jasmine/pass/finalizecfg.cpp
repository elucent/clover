#include "jasmine/mod.h"
#include "util/bits.h"
#include "util/config.h"
#include "util/heap.h"

namespace jasmine {
    void finalizeCFG(PassContext& ctx, Function& fn) {
        JASMINE_PASS(FINALIZE_CFG);
        ctx.require(LOWER);
        ctx.did(FINALIZE);

        for (Block block : fn.blocks()) if (block.nodeIndices().size()) {
            Node branch = block.node(block.nodeIndices().size() - 1);
            if (branch.opcode() == Opcode::BR) {
                Edge edge = fn.edge(branch.operand(0).edge);
                Block dest = edge.dest();
                if (dest.predecessorIndices().size() == 1 && fn.edge(dest.predecessorIndices()[0]).srcIndex() == block.index()) {
                    block.dims().length --; // Remove the branch.
                    for (Node n : dest.nodes())
                        block.addNode(n);
                    for (Edge edge : dest.successors())
                        fn.addEdge(block.index(), edge.dest().index());
                    fn.removeEdge(edge.index());
                    dest.dims().length = 2;
                    dest.header().pred = dest.header().succ = 0;
                }
            }
        }

        auto& schedule = *ctx.schedule;

        vec<BlockIndex> order;
        bitset<256> visited;
        order.push(fn.entrypoint);
        visited.on(fn.entrypoint);
        while (order.size()) {
            Block block = fn.block(order.pop());
            schedule.push(block.index());
            auto indices = block.nodeIndices();
            for (i64 i = i64(indices.size()) - 1; i >= 0; i --) if (isBranch(block.node(i).opcode())) {
                for (Operand operand : block.node(i).operands()) if (operand.kind == Operand::Branch) {
                    BlockIndex dest = fn.edge(operand.edge).destIndex();
                    if (visited[dest])
                        continue;
                    visited.on(dest);
                    order.push(dest);
                }
            }
        }

        visited.clear();
        for (auto [i, b] : enumerate(schedule)) if (i < schedule.size() - 1) {
            Block block = fn.block(b);
            if (block.nodeIndices().size() == 0)
                continue;
            Node branch = block.node(block.nodeIndices().size() - 1);
            if (branch.opcode() == Opcode::BR && fn.edge(branch.operand(0).edge).destIndex() == schedule[i + 1])
                block.dims().length --;
        }
    }
}