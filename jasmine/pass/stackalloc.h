#ifndef JASMINE_PASS_STACKALLOC_H
#define JASMINE_PASS_STACKALLOC_H

#include "jasmine/pass/helpers.h"

namespace jasmine {
    template<typename Target>
    void allocateStackNew(PassContext& ctx, Function& fn) {
        RegSet gps = Target::gps(), fps = Target::fps();

        bitset<128> isVarUsed;
        for (Parameter parameter : fn.parameters)
            isVarUsed.on(parameter.operand.var);
        for (Block block : fn.blocks()) for (Node node : block.nodes()) for (Operand operand : node.operands()) {
            if (operand.kind == Operand::Var)
                isVarUsed.on(operand.var);
        }

        vec<Operand> varSlots;
        varSlots.expandTo(fn.variableList.size(), fn.invalid());

        i64 total8Aligned = 0, total16Aligned = 0, total32Aligned = 0, total64Aligned = 0;
        for (u32 var : isVarUsed) {
            Repr repr = ctx.targetSpecificPasses->repr(fn.variableList[var].type);
            if (isVarUsed[var]) switch (repr.alignment()) {
                case 0:
                case 1:
                    total8Aligned += repr.size();
                    break;
                case 2:
                    total16Aligned += (repr.size() + 1u) / 2u;
                    break;
                case 4:
                    total32Aligned += (repr.size() + 3u) / 4u;
                    break;
                case 8:
                    total64Aligned += (repr.size() + 7u) / 8u;
                    break;
                default:
                    unreachable("Invalid alignment '", repr.alignment(), "' in repr.");
            }
        }

        total16Aligned *= 2;
        total32Aligned *= 4;
        total64Aligned *= 8;

        i64 first8Aligned = -(total16Aligned + total32Aligned + total64Aligned);
        i64 first16Aligned = -(total32Aligned + total64Aligned);
        i64 first32Aligned = -total64Aligned;

        auto& allocations = *ctx.allocations;

        i64 num8Aligned = 0, num16Aligned = 0, num32Aligned = 0, num64Aligned = 0;
        for (u32 var : isVarUsed) {
            Repr repr = ctx.targetSpecificPasses->repr(fn.variableList[var].type);
            if (isVarUsed[var]) switch (repr.alignment()) {
                case 0:
                case 1:
                    num8Aligned += repr.size();
                    varSlots[var] = fn.memory(Target::fp, first8Aligned - num8Aligned);
                    break;
                case 2:
                    num16Aligned += (repr.size() + 1u) / 2u * 2u;
                    varSlots[var] = fn.memory(Target::fp, first16Aligned - num16Aligned);
                    break;
                case 4:
                    num32Aligned += (repr.size() + 3u) / 4u * 4u;
                    varSlots[var] = fn.memory(Target::fp, first32Aligned - num32Aligned);
                    break;
                case 8:
                    num64Aligned += (repr.size() + 7u) / 8u * 8u;
                    varSlots[var] = fn.memory(Target::fp, -num64Aligned);
                    break;
                default:
                    unreachable("Invalid alignment '", repr.alignment(), "' in repr.");
            }
        }

        allocations.stack = total64Aligned + total32Aligned + total16Aligned + total8Aligned;
        allocations.stack = roundUpToNearest(allocations.stack, 16);

        for (BlockIndex bi = 0; bi < fn.blockList.size(); bi ++) {
            Block b = fn.block(bi);

            for (u32 i = 0; i < b.nodeIndices().size(); i ++) {
                Node n = b.node(i);
                fn.addInsertion(b, i);
                RegSet used;
                RegSet* valid = ctx.targetSpecificPasses->validRegsFor(n);
                for (const auto& [i, o] : enumerate(n.operands())) {
                    if (o.kind == Operand::Var) {
                        RegSet available = valid[i].without(used);
                        assert(!available.empty());
                        Operand reg = fn.reg(available.next());
                        used.add(reg.reg);
                        if (i == 0 && hasDef(n.opcode()))
                            continue;
                        fn.addNodeToInsertion(fn.addNode(Opcode::LOAD, fn.variableList[o.var].type, reg, varSlots[o.var]));
                        n.operand(i) = reg;
                    }
                }
                if (hasDef(n.opcode())) {
                    assert(n.operand(0).kind == Operand::Var);
                    RegSet available = valid[0];
                    assert(!available.empty());
                    Operand reg = fn.reg(available.next());
                    fn.addInsertion(b, i, Function::Late);
                    fn.addNodeToInsertion(fn.addNode(Opcode::STORE, fn.variableList[n.operand(0).var].type, varSlots[n.operand(0).var], reg));
                    n.operand(0) = reg;
                }
            }
        }

        if UNLIKELY(config::verboseRegalloc) {
            for (u32 var : isVarUsed) {
                println("Allocated variable ", OperandLogger { fn, fn.variableById(var) }, " to ", OperandLogger { fn, allocations.allocations[var] });
            }
        }
    }

    template<typename Target>
    void TargetSpecificPasses<Target>::allocateStackOnly(PassContext& ctx, Function& fn) {
        JASMINE_PASS(STACK_ALLOCATION);

        ctx.require(TYPECHECK);
        ctx.did(ALLOCATE);

        if (config::useNewLowering)
            return allocateStackNew<Target>(ctx, fn);

        auto& allocations = *ctx.allocations;
        allocations.initialize(ctx, fn, AllocationResult::AllocateVariables, AllocationResult::SharedScratchRegisters);

        for (mreg r : gpScratches) if (allocations.sharedGpScratches.count < 3)
            allocations.sharedGpScratches.push(r);
        for (mreg r : fpScratches) if (allocations.sharedFpScratches.count < 3)
            allocations.sharedFpScratches.push(r);

        bitset<128> isVarUsed;
        for (Parameter parameter : fn.parameters)
            isVarUsed.on(parameter.operand.var);
        for (Block block : fn.blocks()) for (Node node : block.nodes()) for (Operand operand : node.operands()) {
            if (operand.kind == Operand::Var)
                isVarUsed.on(operand.var);
        }
        for (Edge edge : fn.edges()) {
            for (Move move : edge.moves()) {
                if (move.src.kind == Operand::Var)
                    isVarUsed.on(move.src.var);
                if (move.dest.kind == Operand::Var)
                    isVarUsed.on(move.dest.var);
            }
            allocations.edgeAllocations[edge.index()].first = fn.reg(gpScratches[0]);
            allocations.edgeAllocations[edge.index()].second = fn.reg(fpScratches[0]);
        }

        i64 total8Aligned = 0, total16Aligned = 0, total32Aligned = 0, total64Aligned = 0;
        for (u32 var : isVarUsed) {
            Repr repr = this->repr(fn.variableList[var].type);
            if (isVarUsed[var]) switch (repr.alignment()) {
                case 0:
                case 1:
                    total8Aligned += repr.size();
                    break;
                case 2:
                    total16Aligned += (repr.size() + 1u) / 2u;
                    break;
                case 4:
                    total32Aligned += (repr.size() + 3u) / 4u;
                    break;
                case 8:
                    total64Aligned += (repr.size() + 7u) / 8u;
                    break;
                default:
                    unreachable("Invalid alignment '", repr.alignment(), "' in repr.");
            }
        }

        total16Aligned *= 2;
        total32Aligned *= 4;
        total64Aligned *= 8;

        i64 first8Aligned = -(total16Aligned + total32Aligned + total64Aligned);
        i64 first16Aligned = -(total32Aligned + total64Aligned);
        i64 first32Aligned = -total64Aligned;

        i64 num8Aligned = 0, num16Aligned = 0, num32Aligned = 0, num64Aligned = 0;
        for (u32 var : isVarUsed) {
            Repr repr = this->repr(fn.variableList[var].type);
            if (isVarUsed[var]) switch (repr.alignment()) {
                case 0:
                case 1:
                    num8Aligned += repr.size();
                    allocations.allocations[var] = fn.memory(Target::fp, first8Aligned - num8Aligned);
                    break;
                case 2:
                    num16Aligned += (repr.size() + 1u) / 2u * 2u;
                    allocations.allocations[var] = fn.memory(Target::fp, first16Aligned - num16Aligned);
                    break;
                case 4:
                    num32Aligned += (repr.size() + 3u) / 4u * 4u;
                    allocations.allocations[var] = fn.memory(Target::fp, first32Aligned - num32Aligned);
                    break;
                case 8:
                    num64Aligned += (repr.size() + 7u) / 8u * 8u;
                    allocations.allocations[var] = fn.memory(Target::fp, -num64Aligned);
                    break;
                default:
                    unreachable("Invalid alignment '", repr.alignment(), "' in repr.");
            } else
                allocations.allocations[var] = fn.invalid();
        }

        allocations.stack = total64Aligned + total32Aligned + total16Aligned + total8Aligned;
        allocations.stack = roundUpToNearest(allocations.stack, 16);

        if UNLIKELY(config::verboseRegalloc) {
            for (u32 var : isVarUsed) {
                println("Allocated variable ", OperandLogger { fn, fn.variableById(var) }, " to ", OperandLogger { fn, allocations.allocations[var] });
            }
        }
    }
}

#endif