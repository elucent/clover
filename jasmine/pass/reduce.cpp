#include "jasmine/pass.h"
#include "util/deque.h"
#include "jasmine/mod.h"
#include "jasmine/pass/reduce.h"

namespace jasmine {
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

    void reduceStrength(PassContext& ctx, Function& fn) {
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
        for (NodeIndex i : order)
            assert(i < fn.nodeList.size());

        vec<u32, 64> uses;
        vec<NodeIndex, 64> userNodes;
        {
            vec<vec<NodeIndex, 4>> useTable;
            for (i32 i = 0; i < fn.variableList.size(); i ++)
                useTable.push({});
            for (Node node : fn.nodes()) {
                for (Operand o : node.uses()) if (o.kind == Operand::Var)
                    useTable[o.var].push(node.index());
            }
            for (i32 i = 0; i < fn.variableList.size(); i ++) {
                uses.push(userNodes.size());
                for (NodeIndex n : useTable[i])
                    userNodes.push(n);
            }
        }

        auto usesOf = [&](i32 var) -> slice<NodeIndex> {
            NodeIndex* start = userNodes.data() + uses[var];
            NodeIndex* end;
            if UNLIKELY(var == uses.size() - 1)
                end = userNodes.end();
            else
                end = userNodes.data() + uses[var + 1];
            return { start, end - start };
        };

        bool fixpoint = false;
        u32 iteration = 0;
        while (!fixpoint) {
            fixpoint = true;
            seenNodes.clear(); // We'll use this to track which nodes changed from now on.

            if UNLIKELY(config::verboseStrengthReduction)
                println("[STRED]\tBeginning iteration ", ++ iteration, ", current IR is:\n", fn);

            for (NodeIndex index : order) {
                bool shouldRecompute = false;
                Node node = fn.node(index);
                if UNLIKELY(config::verboseStrengthReduction) {
                    bool willChangeUse = false;
                    for (auto& use : node.uses()) if (use.kind == Operand::Var && changedDef[use.var])
                        willChangeUse = true;
                    if (willChangeUse)
                        print("[STRED]\tTransformed ", node, " to ");
                }
                bool changedAnyUse = false;
                for (auto& use : node.uses()) if (use.kind == Operand::Var) {
                    if (changedDef[use.var]) {
                        changedAnyUse = true;
                        shouldRecompute = true;
                        use = defOperands[use.var];
                    } else {
                        auto def = defs[use.var];
                        if UNLIKELY(def == -1)
                            continue;
                        if (seenNodes[def])
                            shouldRecompute = true;
                    }
                }
                if UNLIKELY(config::verboseStrengthReduction && changedAnyUse)
                    println(node, " via copy propagation");
                if (shouldRecompute)
                    computeForNode(ctx, defs, fn, node, frontier);

                TreeSignature sig = ctx.treeSignatures[index];
                Opcode currentOpcode = node.opcode();
                Operand currentDef;
                if LIKELY(hasDef(node.opcode()))
                    currentDef = node.defs()[0];
                auto& rules = ruleDatabase.ensureRuleSet(currentOpcode);
                for (const auto& rule : rules.rules) {
                    if (!sig.matches(rule.signature))
                        continue;
                    auto loc = locations[index];
                    array<i8, 128> buffer;
                    const_slice<i8> nodeStr;
                    if UNLIKELY(config::verboseStrengthReduction)
                        nodeStr = prints(buffer, node);
                    auto result = rule.reduction(ctx, fn, fn.block(loc.block), loc.indexInBlock, node);
                    if (result) {
                        fixpoint = false;
                        if UNLIKELY(config::verboseStrengthReduction)
                            print("[STRED]\tApplied rule ", cstring(rule.name), " to node ", nodeStr);
                        if (result.isNode()) {
                            seenNodes.on(result.newNode);
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
                            break;
                        }
                    }
                }
            }

            changedDef.clear();

            fn.executeInsertions();
        }
    }
}