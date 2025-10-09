#include "jasmine/pass.h"
#include "util/deque.h"
#include "jasmine/mod.h"
#include "jasmine/pass/reduce.h"

namespace jasmine {
    struct ValueNumbering {
        vec<i32, 32> numbers; // Maps each defined variable to a value numbering.

        struct LazyTable {
            using Table = vec<NodeIndex, 8>;
            Table* pointer;

            inline LazyTable():
                pointer(nullptr) {}

            PREVENT_COPYING(LazyTable);
            PREVENT_MOVING(LazyTable);

            inline ~LazyTable() {
                if (pointer)
                    delete pointer;
            }
        };

        LazyTable opcodeTables[NUM_OPCODES];
    };

    void computeForNode(PassContext& ctx, PassContext::Defs& defs, Function& fn, Node node, deque<NodeIndex, 32>& frontier) {
        if (node.arity() == 0)
            return;

        vec<u8, 16> signature;
        u8 size = 0;

        frontier.clear();
        frontier.pushr(node.index());
        signature.push(signTreeElement(node.opcode())), size ++;
        while (frontier.size()) {
            Node node = fn.node(frontier.popl());
            if (size >= TreeSignatureLength)
                break;

            for (Operand o : node.uses()) {
                if (size >= TreeSignatureLength)
                    break;
                switch (o.kind) {
                    case Operand::Var:
                        if (defs[o.var] >= 0) {
                            NodeIndex index = defs[o.var];
                            frontier.pushr(index);
                            signature.push(signTreeElement(fn.node(index).opcode())), size ++;
                        } else {
                            // Var will never intentionally match a pattern (other than wildcard), but we
                            // want to give it different bits than any opcode to rule out false positives.
                            signature.push(signTreeElement(TreeElement::Var)), size ++;
                        }
                        break;
                    case Operand::IntConst:
                        signature.push(signTreeElement(TreeElement::IntConst)), size ++;
                        break;
                    case Operand::F32Const:
                        signature.push(signTreeElement(TreeElement::F32Const)), size ++;
                        break;
                    case Operand::F64Const:
                        signature.push(signTreeElement(TreeElement::F64Const)), size ++;
                        break;
                    default:
                        signature.push(0), size ++;
                        break;
                }
            }
        }
        while (size > TreeSignatureLength)
            signature.pop(), -- size; // Truncate anything after the expected length.
        while (size < TreeSignatureLength)
            signature.push(0), ++ size; // Pad out to expected length.

        ctx.treeSignatures[node.index()] = load<TreeSignature>(signature.data());

        // if UNLIKELY(config::verboseStrengthReduction) {
        //     auto sig = ctx.treeSignatures[node.index()];
        //     println("[STRED]\tUse/def tree signature of instruction ", node, " is {0x", hex<u64>(sig.low, 16), "ull, 0x", hex<u64>(sig.high, 16), "ull}");
        // }
    }

    extern void populateDatabase(ReductionRules& rules);

    struct NodeLocation {
        BlockIndex block;
        u32 indexInBlock;
    };

    void scheduleNodesTopologically(PassContext& ctx, PassContext::Defs& defs, Function& fn, bitset<128>& seen, vec<NodeIndex, 64>& order, NodeIndex index) {
        // There should never be cyclic dependencies once SSA is enforced, so
        // we can do super naive depth-first-search.
        if (seen[index])
            return;
        seen.on(index);
        auto uses = fn.node(index).uses();
        for (Operand operand : uses) if (operand.kind == Operand::Var) {
            auto def = defs[operand.var];
            if LIKELY(def >= 0)
                scheduleNodesTopologically(ctx, defs, fn, seen, order, def);
        }

        order.push(index);
    }

    NOINLINE void reduceStrength(PassContext& ctx, Function& fn) {
        JASMINE_PASS(STRENGTH_REDUCTION);
        ctx.require(SSA);
        recordDefs(ctx, fn);

        static u8 ruleDatabaseBytes[sizeof(ReductionRules)];
        ReductionRules& ruleDatabase = *bitcast<ReductionRules*>(&ruleDatabaseBytes);

        if UNLIKELY(!ctx.rulesInited) {
            ctx.rulesInited = true;
            new (&ruleDatabase) ReductionRules;
            populateDatabase(ruleDatabase);
        }

        auto& defs = *ctx.defs;
        vec<Operand, 64> defOperands;
        for (u32 i = 0; i < defs.size(); i ++) {
            if (defs[i] >= 0)
                defOperands.push(fn.node(defs[i]).defs()[0]);
            else
                defOperands.push(fn.invalid());
        }
        deque<NodeIndex, 32> frontier;
        ctx.treeSignatures.clear();
        ctx.treeSignatures.expandBy(fn.nodeList.size());
        for (Node n : fn.nodes()) if (ruleDatabase.doesOpcodeHaveRules[(u32)n.opcode()])
            computeForNode(ctx, defs, fn, n, frontier);

        vec<NodeLocation, 64> locations;
        locations.expandBy(fn.nodeList.size());
        for (u32 i = 0; i < fn.blockList.size(); i ++) {
            Block block = fn.block(i);
            auto indices = block.nodeIndices();
            for (u32 j = 0; j < indices.size(); j ++)
                locations[indices[j]] = { i, j };
        }

        vec<NodeIndex, 64> order;
        bitset<128> seenNodes, changedDef;
        for (NodeIndex index = 0; index < fn.nodeList.size(); index ++) if (!seenNodes[index])
            scheduleNodesTopologically(ctx, defs, fn, seenNodes, order, index);
        reverse(order);
        for (NodeIndex i : order)
            assert(i < fn.nodeList.size());

        vec<biasedset<128>> userNodes, userEdges;
        auto recomputeUses = [&]() {
            userNodes.clear();
            userEdges.clear();
            userNodes.expandTo(fn.variableList.size());
            userEdges.expandTo(fn.variableList.size());
            for (Node node : fn.nodes()) {
                for (Operand o : node.uses()) if (o.kind == Operand::Var)
                    userNodes[o.var].on(node.index());
            }
            for (Edge edge : fn.edges()) for (Move move : edge.moves()) {
                if (move.src.kind == Operand::Var)
                    userEdges[move.src.var].on(edge.index());
            }
        };
        recomputeUses();

        bool fixpoint = false;
        u32 iteration = 0;
        while (!fixpoint) {
            fixpoint = true;
            if UNLIKELY(config::verboseStrengthReduction)
                println("[STRED]\tBeginning iteration ", iteration, ", ", order.size(), " nodes to investigate");

            while (order.size()) {
                NodeIndex index = order.pop();
                seenNodes.off(index);
                Node node = fn.node(index);

                TreeSignature sig = ctx.treeSignatures[index];
                Opcode currentOpcode = node.opcode();
                Operand currentDef = fn.invalid();
                if LIKELY(hasDef(node.opcode()))
                    currentDef = node.defs()[0];
                auto& rules = ruleDatabase.ensureRuleSet(currentOpcode);
                bool eliminatedNode = false;
                bool addedAnyInsertion = false;
                bool anySuccessfulTransform = false;
                for (const auto& rule : rules.rules) {
                    if (!sig.matches(rule.signature))
                        continue;
                    auto loc = locations[index];
                    array<i8, 256> buffer;
                    const_slice<i8> nodeStr;
                    if UNLIKELY(config::verboseStrengthReduction)
                        nodeStr = prints(buffer, node);
                    u32 numNodesBefore = fn.nodeList.size();
                    auto result = rule.reduction(ctx, fn, fn.block(loc.block), loc.indexInBlock, node);
                    if (result) {
                        if UNLIKELY(config::verboseStrengthReduction)
                            print("[STRED]\tApplied rule ", cstring(rule.name), " to node ", nodeStr);
                        fixpoint = false;
                        anySuccessfulTransform = true;
                        if (result.addedInsertion())
                            addedAnyInsertion = true, fixpoint = false;
                        if (result.isNode()) {
                            seenNodes.on(result.newNode);
                            order.push(result.newNode);
                            auto newNode = fn.node(result.newNode);
                            if UNLIKELY(config::verboseStrengthReduction)
                                println(" and got ", newNode);
                            if LIKELY(hasDef(newNode.opcode())) {
                                defs[newNode.defs()[0].var] = newNode.index();
                                currentDef = newNode.defs()[0];
                            }
                            computeForNode(ctx, defs, fn, newNode, frontier);
                            if (newNode.opcode() != currentOpcode)
                                break;
                            sig = ctx.treeSignatures[newNode.index()];
                            node = newNode;
                            continue;
                        } else {
                            assert(result.isOperand());
                            if UNLIKELY(config::verboseStrengthReduction)
                                println(" and got ", OperandLogger { fn, result.operand });
                            changedDef.on(currentDef.var);
                            defOperands[currentDef.var] = result.operand;
                            eliminatedNode = true;
                            break;
                        }
                    }
                }
                if (anySuccessfulTransform && currentDef.kind == Operand::Var) {
                    for (i32 i : userNodes[currentDef.var]) {
                        if (eliminatedNode) {
                            for (Operand& use : fn.node(i).uses()) if (use == currentDef)
                                use = defOperands[currentDef.var];
                        }
                        if UNLIKELY(config::verboseStrengthReduction)
                            println("[STRED]\tUpdated node ", fn.node(i));
                        if (!addedAnyInsertion) {
                            computeForNode(ctx, defs, fn, fn.node(i), frontier);
                            if (!seenNodes[i]) {
                                seenNodes.on(i);
                                order.push(i);
                            }
                        }
                    }
                    if (eliminatedNode) for (i32 i : userEdges[currentDef.var]) {
                        for (Move& move : fn.edge(i).moves()) if (move.src == currentDef)
                            move.src = defOperands[currentDef.var];
                        if UNLIKELY(config::verboseStrengthReduction)
                            println("[STRED]\tUpdated edge ", fn.edge(i));
                    }
                }
            }

            if (fn.insertions.size()) {
                recomputeUses();
                fn.forEachInsertedNode([&](Node node) {
                    if LIKELY(hasDef(node.opcode())) {
                        for (i32 i : userNodes[node.defs()[0].var]) {
                            if (!seenNodes[i]) {
                                seenNodes.on(i);
                                order.push(i);
                            }
                        }
                    }
                });
                fn.executeInsertions();
            }
        }
    }
}