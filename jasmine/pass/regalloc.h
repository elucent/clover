#ifndef JASMINE_PASS_REGALLOC_H
#define JASMINE_PASS_REGALLOC_H

#include "jasmine/pass/helpers.h"
#include "util/deque.h"
#include "util/io.h"
#include "asm/arch.h"

namespace jasmine {
    template<typename Target>
    struct VariableLocation {
        enum Kind { NONE, GP, FP, STACK };
        union {
            struct { Kind kind : 2; mreg gp : 6; i32 stack : 24; };
            struct { Kind : 2; mreg fp : 6; i32 : 24; };
        };

        inline static VariableLocation None() {
            VariableLocation loc;
            loc.kind = NONE;
            loc.gp = 0;
            loc.stack = 0;
            return loc;
        }

        inline static VariableLocation Gp(mreg r) {
            VariableLocation loc;
            loc.kind = GP;
            loc.gp = r;
            loc.stack = 0;
            return loc;
        }

        inline static VariableLocation Fp(mreg r) {
            VariableLocation loc;
            loc.kind = FP;
            loc.gp = r;
            loc.stack = 0;
            return loc;
        }

        inline static VariableLocation Stack(i32 stack) {
            VariableLocation loc;
            loc.kind = STACK;
            loc.gp = 0;
            loc.stack = stack;
            return loc;
        }

        inline Operand operand(Function& fn) const {
            switch (kind) {
                case GP:
                    return fn.gp(gp);
                case FP:
                    return fn.fp(fp);
                case STACK:
                    return fn.memory(Target::fp, stack);
                default:
                    unreachable("Tried to turn a none location into an operand!");
            }
        }

        inline bool isGp() const { return kind = GP; }
        inline bool isFp() const { return kind = FP; }
    };

    struct SlotInfo {
        u8 alignment;
        i32 indexWithinAlignment;

        inline Operand realSlot(Function& fn, mreg fp, u32 n64, u32 n32, u32 n16) {
            switch (alignment) {
                case 1:
                    return fn.memory(fp, -i64(n64 * 8 + n32 * 4 + n16 * 2 + indexWithinAlignment));
                case 2:
                    return fn.memory(fp, -i64(n64 * 8 + n32 * 4 + indexWithinAlignment * 2));
                case 4:
                    return fn.memory(fp, -i64(n64 * 8 + indexWithinAlignment * 4));
                case 8:
                    return fn.memory(fp, -i64(indexWithinAlignment * 8));
                default:
                    unreachable("Unexpected SlotInfo alignment ", (u32)alignment);
            }
        }
    };

    struct GraphNode {
        bool assigned;
        mreg hint;
        u16 overlappingCalls;
        u32 range;
        u8 necessaryScratches;
        bool isMemory;
        BlockIndex block;
        RegSet allowed;
    };

    template<typename Target>
    void allocateRegistersAdvanced(PassContext& ctx, TargetSpecificPasses<Target>& targetCtx, Function& fn, RegisterAllocationMode mode) {
        ctx.require(CONTAINMENT);
        ctx.require(CLOBBERS);
        ctx.did(ALLOCATE);

        auto& allocations = *ctx.allocations;
        allocations.initialize(ctx, fn, AllocationResult::AllocateLiveRanges, AllocationResult::PerInstructionScratchRegisters);

        auto& liveness = *ctx.liveness;
        auto& interference = *ctx.interference;
        const auto& pins = *ctx.pins;

        auto computePriorityInBlock = [&](BlockIndex block, LiveRangeIndex range) -> float {
            float priority = liveness[range].length();
            if (ctx.has(FREQUENCY))
                priority *= (*ctx.blockFrequency)[block];
            return priority;
        };

        vec<float> priorities;
        for (u32 i = 0; i < fn.blockList.size(); i ++) {
            for (u32 j : liveness.indicesInBlock(i))
                priorities.push(computePriorityInBlock(i, j));
        }
        assert(priorities.size() == liveness.ranges.size());

        using GraphNodeIndex = LiveRangeIndex;
        auto compare = [&](GraphNodeIndex a, GraphNodeIndex b) -> bool {
            return priorities[a] > priorities[b];
        };

        vec<GraphNode> graph;
        auto& clobberList = *ctx.clobberList;
        auto& scratchesPerNode = *ctx.scratchesPerNode;
        auto& callInsns = *ctx.callInsns;
        auto& callSites = *ctx.callSites;
        vec<i8> variableHints;
        for (u32 i = 0; i < fn.variableList.size(); i ++)
            variableHints.push(-1);
        vec<NodeIndex> returns;

        // Hint parameters/return values of calls towards their native locations.

        for (const CallSite& callSite : callSites) {
            Node node = fn.node(callSite.node);
            const auto& funcType = fn.typeContext()[node.type()];
            if (funcType.returnType() != VOID) {
                Operand result = node.defs()[0];
                if (result.kind == Operand::Var && variableHints[result.var] == -1 && callSite.returnValue.isReg()) {
                    // TODO: Should we use register pair return value hinting here?
                    if UNLIKELY(config::verboseRegalloc)
                        println("Hinted ", OperandLogger { fn, result }, " to ", OperandLogger { fn, callSite.returnValue }, " due to use as result in ", node);
                    variableHints[result.var] = callSite.returnValue.gp;
                }
            }
            for (auto [i, o] : enumerate(node.uses().drop(1))) { // Drop 1 for the callee.
                if (o.kind == Operand::Var && variableHints[o.var] == -1 && callSite.parameters[i].isReg()) {
                    if UNLIKELY(config::verboseRegalloc)
                        println("Hinted ", OperandLogger { fn, o }, " to ", OperandLogger { fn, callSite.parameters[i] }, " due to use as parameter in ", node);
                    variableHints[o.var] = callSite.parameters[i].gp;
                }
            }
        }

        void* parameterState = Target::start_placing_parameters();
        Operand returnOperand;
        if (fn.returnType != VOID) {
            auto returnPlacement = targetCtx.placeReturnValue(fn, fn.returnType, parameterState);
            returnOperand = operandFromPlacement(fn, returnPlacement);
        }
        for (const auto& param : fn.parameters) if (!isCompound(param.type) || isFunction(fn, param.type)) {
            Repr repr = targetCtx.repr(param.type);
            auto placement = Target::place_scalar_parameter(parameterState, repr);
            Operand parameterOperand = operandFromPlacement(fn, placement);
            if (param.operand.kind == Operand::Var && variableHints[param.operand.var] == -1 && parameterOperand.isReg()) {
                if UNLIKELY(config::verboseRegalloc)
                    println("Hinted ", OperandLogger { fn, param.operand }, " to ", OperandLogger { fn, parameterOperand }, " since it's a parameter of this function");
                variableHints[param.operand.var] = parameterOperand.gp;
            }
        }
        Target::finish_placing_parameters(parameterState);

        // Hint variables that we return towards their native return locations.

        for (Block block : fn.blocks()) if (block.successorIndices().size() == 0) {
            Node last = block.last();
            if (last.opcode() != Opcode::RET)
                continue;
            if (last.type() == VOID)
                continue;
            Operand use = last.uses()[0];
            if (use.kind == Operand::Var && variableHints[use.var] == -1 && returnOperand.isReg()) {
                if UNLIKELY(config::verboseRegalloc)
                    println("Hinted ", OperandLogger { fn, use }, " to ", OperandLogger { fn, returnOperand }, " due to use in ", last);
                variableHints[use.var] = returnOperand.gp;
            }
        }

        // Build a graph out of the live ranges, including default hints.

        for (Block block : fn.blocks()) {
            const auto& rangeIndices = liveness.indicesInBlock(block.index());
            for (u32 i : rangeIndices) {
                RegSet allowed;
                if (isFPType(fn.variableList[liveness[i].var].type))
                    allowed = Target::fps();
                else {
                    // Subtlety here - we need to allow these for memory
                    // variables too, since even if they don't get assigned to
                    // registers, instructions they're live across might need
                    // to know about free registers for scratches.
                    allowed = Target::gps();
                }
                u16 overlappingCalls = 0;
                u8 necessaryScratches = 0;
                for (u32 j = liveness.ranges[i].start + 1; j < liveness.ranges[i].end; j ++) {
                    NodeIndex node = block.nodeIndices()[j - 1];
                    if UNLIKELY(config::verboseRegalloc)
                        println("Clobber list for ", fn.node(node), " is ", SeqFormat(", ", clobberList[node]));
                    if UNLIKELY(!clobberList[node].empty())
                        allowed -= clobberList[node];
                    if UNLIKELY(callInsns[node] != -1) {
                        overlappingCalls ++;
                        Node call = fn.node(node);
                        if (call.operand(1).kind == Operand::Var && liveness[i].var == call.operand(1).var) {
                            for (Operand operand : callSites[callInsns[node]].parameters) {
                                if (operand.isReg())
                                    allowed.remove(operand.gp);
                                else if (operand.kind == Operand::RegPair) {
                                    allowed.remove(operand.ra);
                                    allowed.remove(operand.rb);
                                } else if (operand.kind == Operand::Memory)
                                    allowed.remove(operand.base);
                            }
                        }
                    }
                    if UNLIKELY(config::verboseRegalloc)
                        println("Scratches needed for ", fn.node(node), " is ", (u32)scratchesPerNode[node]);
                    necessaryScratches = max(necessaryScratches, scratchesPerNode[node]);
                }
                priorities[i] += overlappingCalls * 10;
                graph.push(GraphNode {
                    .assigned = false,
                    .hint = overlappingCalls ? mreg(-1) : variableHints[liveness[i].var],
                    .overlappingCalls = overlappingCalls,
                    .range = i,
                    .necessaryScratches = necessaryScratches,
                    .isMemory = isCompound(fn.variableList[liveness[i].var].type) && !isFunction(fn, fn.variableList[liveness[i].var].type),
                    .block = block.index(),
                    .allowed = allowed,
                });
            }
        }

        u32 edgeOffset = liveness.ranges.size();
        auto isEdge = [&](LiveRangeIndex index) -> bool {
            return index >= edgeOffset;
        };

        vec<RegSet> availablePerEdge;
        for (u32 i : indices(fn.edgeList))
            availablePerEdge.push(Target::gps());

        heap<GraphNodeIndex, decltype(compare)> order(move(compare));
        for (u32 i = 0; i < graph.size(); i ++)
            order.push(i);

        i32& stack = allocations.stack;

        // Allocate registers to ranges in priority order. Once a location is picked for a node,
        // we set its hint to the assigned register, or -1 if it was not assigned a register.

        vec<Operand> stackSlots;
        stackSlots.expandBy(fn.variableList.size(), fn.invalid());

        for (i32 pin : pins.pins) {
            Repr repr = targetCtx.repr(fn.variableList[pin].type);
            while (stack % repr.alignment())
                stack ++;
            allocations.allocationForPinned(pin) = fn.memory(Target::fp, -(stack += repr.size()));
            if UNLIKELY(config::verboseRegalloc)
                println("Assigned pinned variable ", OperandLogger { fn, fn.variableById(pin) }, " to ", OperandLogger { fn, allocations.allocationForPinned(pin) });
        }

        while (order.size()) {
            GraphNode& node = graph[order.pop()];
            LiveRange& range = liveness[node.range];
            if (range.start == range.end)
                continue;

            auto stackAlloc = [&]() {
                node.assigned = true;
                Repr repr = targetCtx.repr(fn.variableList[range.var].type);
                while (stack % repr.alignment())
                    stack ++;
                node.hint = -1;
                allocations.allocations[node.range] = fn.memory(Target::fp, -(stack += repr.size()));
                if UNLIKELY(config::verboseRegalloc)
                    println("Assigned range ", OperandLogger { fn, fn.variableById(range.var) }, "[bb", node.block, ":", range.start, ":", range.end, "] to ", OperandLogger { fn, allocations.allocations[node.range] });
            };

            if (node.isMemory) {
                const LiveRange& range = liveness[node.range];
                if (stackSlots[range.var].kind == Operand::Invalid) {
                    Repr repr = targetCtx.repr(fn.variableList[range.var].type);
                    while (stack % repr.alignment())
                        stack ++;
                    stackSlots[range.var] = fn.memory(Target::fp, -(stack += repr.size()));
                    if UNLIKELY(config::verboseRegalloc)
                        println("Assigned memory variable ", OperandLogger { fn, fn.variableById(range.var) }, " to ", OperandLogger { fn, stackSlots[range.var] });
                }
                node.assigned = true;
                node.hint = -1;
                allocations.allocations[node.range] = stackSlots[liveness[node.range].var];
            } else if (node.allowed.empty())
                stackAlloc();
            else {
                node.assigned = true;
                mreg assignment = node.allowed.next();
                if (node.hint == -1 && range.successor != -1 && graph[range.successor].assigned) {
                    LiveRange& succ = liveness[range.successor];
                    if (allocations.allocations[range.successor].kind == Operand::GP)
                        node.hint = allocations.allocations[range.successor].gp;
                    else if (allocations.allocations[range.successor].kind == Operand::FP)
                        node.hint = allocations.allocations[range.successor].fp;
                }
                if (node.hint != -1 && node.allowed[node.hint])
                    assignment = node.hint;
                else if (node.overlappingCalls == 0 && node.allowed & Target::caller_saves())
                    assignment = (node.allowed & Target::caller_saves()).next();
                else if (node.overlappingCalls >= 1 && node.allowed & Target::callee_saves())
                    assignment = (node.allowed & Target::callee_saves()).next();

                bool didFail = false;
                for (u32 adj : interference[node.range]) {
                    if (isEdge(adj)) {
                        if UNLIKELY(availablePerEdge[adj - edgeOffset].without(assignment).empty()) {
                            didFail = true;
                            if UNLIKELY(config::verboseRegalloc)
                                println("Failed to assign variable ", OperandLogger { fn, fn.variableById(range.var) }, " because it would exhaust edge[bb", fn.edge(adj - edgeOffset).srcIndex(), "->bb", fn.edge(adj - edgeOffset).destIndex(), "]");
                            break;
                        }
                    } else if UNLIKELY(!graph[adj].isMemory && graph[adj].allowed.without(assignment).size() < graph[adj].necessaryScratches) {
                        didFail = true;
                        if UNLIKELY(config::verboseRegalloc)
                            println("Failed to assign variable ", OperandLogger { fn, fn.variableById(range.var) }, " because it would exhaust range ", OperandLogger { fn, fn.variableById(liveness[graph[adj].range].var) }, "[bb", graph[adj].block, ":", liveness[graph[adj].range].start, ":", liveness[graph[adj].range].end, "]");
                        break;
                    }
                }
                if (didFail) {
                    // Allocating this register would potentially exhaust the
                    // available scratches for an edge or instruction in our
                    // live range. So, we have to bail, and spill.
                    stackAlloc();
                    continue;
                }

                if UNLIKELY(config::verboseRegalloc) {
                    println("Assigned range ", OperandLogger { fn, fn.variableById(range.var) }, "[bb", node.block, ":", range.start, ":", range.end, "] to ", OperandLogger { fn, Target::is_gp(assignment) ? fn.gp(assignment) : fn.fp(assignment) });
                    print(" - Removing ", OperandLogger { fn, Target::is_gp(assignment) ? fn.gp(assignment) : fn.fp(assignment) }, " from");
                }
                for (u32 adj : interference[node.range]) {
                    if (isEdge(adj)) {
                        availablePerEdge[adj - edgeOffset].remove(assignment);
                        if UNLIKELY(config::verboseRegalloc)
                            print(" edge[bb", fn.edge(adj - edgeOffset).srcIndex(), "->bb", fn.edge(adj - edgeOffset).destIndex(), "]");
                    } else {
                        graph[adj].allowed.remove(assignment);
                        if UNLIKELY(config::verboseRegalloc)
                            print(" ", OperandLogger { fn, fn.variableById(liveness[graph[adj].range].var) }, "[bb", graph[adj].block, ":", liveness[graph[adj].range].start, ":", liveness[graph[adj].range].end, "]");
                    }
                }
                if UNLIKELY(config::verboseRegalloc)
                    println();
                if (range.successor != -1 && !graph[range.successor].assigned)
                    graph[range.successor].hint = assignment;
                if (Target::callee_saves()[assignment])
                    allocations.calleeSaves.add(assignment);
                node.hint = assignment;
                node.allowed.remove(assignment);
                allocations.allocations[node.range] = Target::is_gp(assignment) ? fn.gp(assignment) : fn.fp(assignment);
            }
        }

        for (const GraphNode& node : graph) {
            if (node.hint == -1 || !Target::caller_saves()[node.hint])
                continue;
            Block block = fn.block(node.block);
            LiveRange range = liveness[node.range];
            for (u16 i = range.start + 1; i < range.end; i ++) if UNLIKELY(callInsns[block.nodeIndices()[i - 1]] != -1) {
                CallSite& callSite = callSites[callInsns[block.nodeIndices()[i - 1]]];
                callSite.liveAcross.add(node.hint);
            }
        }

        for (Block block : fn.blocks()) {
            const auto& rangeIndices = liveness.indicesInBlock(block.index());
            bitset<64> visited;
            for (u32 i : rangeIndices) {
                for (u32 j = max<u16>(1, liveness.ranges[i].start); j <= liveness.ranges[i].end; j ++) {
                    visited.on(j - 1);
                    if (j - 1 == block.nodeIndices().size())
                        continue;
                    NodeIndex node = block.nodeIndices()[j - 1];
                    auto& scratches = allocations.scratchesByInstruction[node];
                    if (scratches.size())
                        continue;
                    RegSet allowed = graph[i].allowed;
                    if UNLIKELY(fn.node(node).opcode() == Opcode::RET) {
                        if (returnOperand.isReg())
                            allowed.remove(returnOperand.gp);
                        else if (returnOperand.kind == Operand::RegPair)
                            allowed.remove(returnOperand.ra), allowed.remove(returnOperand.rb);
                        else if (returnOperand.kind == Operand::Memory) {
                            // The base of the result is encoded in the return placement's offset.
                            allowed.remove(returnOperand.offset);
                        }
                    }
                    RegSet calleeSaves = allowed & Target::callee_saves();
                    RegSet callerSaves = allowed & Target::caller_saves();
                    for (u8 k = 0; k < scratchesPerNode[node]; k ++) {
                        if (!callerSaves.empty()) {
                            allocations.scratchesByInstruction[node].push(callerSaves.next());
                            callerSaves.remove(callerSaves.next());
                        } else {
                            assert(!calleeSaves.empty());
                            allocations.scratchesByInstruction[node].push(calleeSaves.next());
                            allocations.calleeSaves.add(calleeSaves.next());
                            calleeSaves.remove(calleeSaves.next());
                        }
                    }
                    if UNLIKELY(config::verboseRegalloc) {
                        print("Reserved scratches");
                        for (u8 scratch : allocations.scratchesByInstruction[node])
                            print(" ", Target::reg_name(scratch));
                        println(" for instruction ", fn.node(node));
                    }
                }
            }
            for (u32 i = 0; i < block.nodeIndices().size(); i ++) if (!visited[i]) {
                // Handle nodes that weren't covered by any live range. They
                // should have free reign to use any registers they want.

                NodeIndex node = block.nodeIndices()[i];
                auto& scratches = allocations.scratchesByInstruction[node];
                assert(!scratches.size()); // Should not have visited it before.

                RegSet allowed = Target::gps() | Target::fps();
                if UNLIKELY(fn.node(node).opcode() == Opcode::RET) {
                    if (returnOperand.isReg())
                        allowed.remove(returnOperand.gp);
                    else if (returnOperand.kind == Operand::RegPair)
                        allowed.remove(returnOperand.ra), allowed.remove(returnOperand.rb);
                }
                RegSet calleeSaves = allowed & Target::callee_saves();
                RegSet callerSaves = allowed & Target::caller_saves();
                for (u8 k = 0; k < scratchesPerNode[node]; k ++) {
                    if (!callerSaves.empty()) {
                        allocations.scratchesByInstruction[node].push(callerSaves.next());
                        callerSaves.remove(callerSaves.next());
                    } else {
                        assert(!calleeSaves.empty());
                        allocations.scratchesByInstruction[node].push(calleeSaves.next());
                        calleeSaves.remove(calleeSaves.next());
                    }
                }
                if UNLIKELY(config::verboseRegalloc) {
                    print("Reserved scratches");
                    for (u8 scratch : allocations.scratchesByInstruction[node])
                        print(" ", Target::reg_name(scratch));
                    println(" for instruction ", fn.node(node));
                }
            }
        }

        allocations.edgeAllocations.expandTo(fn.edgeList.size(), fn.invalid());
        for (Edge edge : fn.edges()) {
            assert(!availablePerEdge[edge.index()].empty());
            if (availablePerEdge[edge.index()] & Target::caller_saves())
                allocations.edgeAllocations[edge.index()] = fn.gp((availablePerEdge[edge.index()] & Target::caller_saves()).next());
            else
                allocations.edgeAllocations[edge.index()] = fn.gp(availablePerEdge[edge.index()].next());
            if UNLIKELY(config::verboseRegalloc) {
                println("Assigned edge bb", edge.srcIndex(), " -> bb", edge.destIndex(), " to ", OperandLogger { fn, allocations.edgeAllocations[edge.index()] });
            }
        }

        stack = roundUpToNearest(stack, 16);
    }

    template<typename Target>
    void TargetSpecificPasses<Target>::allocateRegisters(PassContext& ctx, Function& fn, RegisterAllocationMode mode) {
        JASMINE_PASS(REGISTER_ALLOCATION);
        switch (mode) {
            case RegisterAllocationMode::ADVANCED:
                allocateRegistersAdvanced(ctx, *this, fn, mode);
                break;
            default:
                unreachable("Unimplemented register allocation mode.");
        }
    }
}

#endif