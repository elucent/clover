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
        u32 necessaryScratches : 3;
        u32 isMemory : 1;
        i32 link : 28;
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
                u32 blockLength = block.nodeIndices().size();
                for (u32 j = liveness.ranges[i].start; j <= liveness.ranges[i].end; j ++) {
                    if (j == 0)
                        continue;
                    if (j == blockLength)
                        break;
                    NodeIndex node = block.nodeIndices()[j - 1];
                    if UNLIKELY(config::verboseRegalloc)
                        println("  Clobber list for ", fn.node(node), " is ", SeqFormat(", ", clobberList[node]));
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
                        println("  Scratches needed for ", fn.node(node), " is ", (u32)scratchesPerNode[node]);
                    necessaryScratches = max(necessaryScratches, scratchesPerNode[node]);
                }
                if UNLIKELY(config::verboseRegalloc)
                    println("Total necessary scratches for ", OperandLogger { fn, fn.variableById(liveness[i].var) }, "[bb", block.index(), ":", liveness[i].start, ":", liveness[i].end, "] is ", (u32)necessaryScratches);
                priorities[i] += overlappingCalls * 2;
                if (liveness[i].start == 0)
                    priorities[i] += block.predecessorIndices().size() * 5;
                graph.push(GraphNode {
                    .assigned = false,
                    .hint = overlappingCalls ? mreg(-1) : variableHints[liveness[i].var],
                    .overlappingCalls = overlappingCalls,
                    .range = i,
                    .necessaryScratches = necessaryScratches,
                    .isMemory = isCompound(fn.variableList[liveness[i].var].type) && !isFunction(fn, fn.variableList[liveness[i].var].type),
                    .link = -1,
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
                if (node.allowed.without(assignment).size() < node.necessaryScratches)
                    didFail = true;
                else for (u32 adj : interference[node.range]) {
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
                    println(" - Necessary scratches = ", node.necessaryScratches, ", remaining allowed = ", node.allowed.without(assignment).size());
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
    struct SinglePassRegisterAllocator {
        TargetSpecificPasses<Target>* passes;
        PassContext& ctx;
        Function& fn;
        Pins& pins;
        Operand returnOperand;
        Operand magicTag;

        // This represents the common state of the register allocator: which
        // registers are we allowed to allocate, and what variable if any is
        // currently occupying each register. We reset this after each block
        // and repopulate it when we investigate the next one.
        RegSet validGps, validFps;
        RegSet reversedValidGps, reversedValidFps;
        RegSet allClobbers;
        RegSet freeRegs;
        i32 regBindings[64];
        biasedset<128> liveInSlot;
        bitset<128> completedBlocks;
        vec<BlockIndex> postorder;
        vec<bitset<128>> gen, kill, liveIn; // Block-wide liveness information.

        // To help free up registers whenever possible, we track the dominating
        // defs of each variable.
        vec<vec<pair<BlockIndex, NodeIndex>, 4>, 32> dominatingDefs;

        void computeDominatingDefs() {
            const auto& dominators = *ctx.dominators;

            fn.scheduleInPostorder(postorder);

            for (const auto& param : fn.parameters) {
                assert(param.operand.kind == Operand::Var); // Later, we'll store regalloc hints here. But we shouldn't yet.
                dominatingDefs[param.operand.var].push({ fn.entrypoint, 0 });
            }

            const auto& clobbers = *ctx.clobberList;
            for (BlockIndex b : reversed(postorder)) for (auto [i, n] : enumerate(fn.block(b).nodes())) {
                allClobbers |= clobbers[n.index()];
                if (hasDef(n.opcode())) {
                    Operand def = n.def(0);
                    if (dominatingDefs[def.var].size() == 0)
                        dominatingDefs[def.var].pushUnchecked({ b, (u32)i });
                    else {
                        auto& defs = dominatingDefs[def.var];
                        bool foundAnyDominator = false;
                        for (auto e : defs) if (dominators[e.first].dominates(b)) {
                            if (e.first == b && e.second > i)
                                continue;
                            foundAnyDominator = true;
                            break;
                        }
                        if (!foundAnyDominator) {
                            defs.removeIf([&](pair<BlockIndex, NodeIndex> e) -> bool {
                                return dominators[b].dominates(e.first);
                            });
                            dominatingDefs[def.var].push({ b, (u32)i });
                        }
                    }
                }
                for (Operand o : n.uses()) if (o.kind == Operand::Var)
                    gen[b].on(o.var);
            }

            for (const auto& [var, defs] : enumerate(dominatingDefs)) for (auto e : defs)
                kill[e.first].on(var);

            if UNLIKELY(config::verboseRegalloc) {
                for (const auto& [i, defs] : enumerate(dominatingDefs)) {
                    if (!defs.size())
                        continue;
                    println("[ALLOC]\tDominating defs of variable ", OperandLogger { fn, fn.variableById(i) }, " are: ");
                    for (auto e : defs) println("[ALLOC]\t - ", fn.block(e.first).node(e.second), " in .bb", e.first);
                }
            }

            #ifndef RELEASE
                for (const auto& defs : dominatingDefs) {
                    for (auto e : defs) for (auto o : defs)
                        assert(e.first == o.first || !dominators[e.first].dominates(o.first));
                }

                for (const auto& param : fn.parameters) {
                    assert(dominatingDefs[param.operand.var].size() == 1);
                    assert(dominatingDefs[param.operand.var][0].first == fn.entrypoint);
                    assert(dominatingDefs[param.operand.var][0].second == 0);
                }
            #endif

            for (u32 k = 0; k < 2; k ++) for (BlockIndex bi : postorder) {
                Block block = fn.block(bi);
                for (Edge pred : block.predecessors())
                    liveIn[bi] |= liveIn[pred.srcIndex()];
                liveIn[bi] |= gen[bi];
                liveIn[bi].removeAll(kill[bi]);
            }
        }

        bool isFirstDef(BlockIndex b, u32 indexInBlock, i32 var) {
            const auto& defs = dominatingDefs[var];
            switch (defs.size()) {
                case 0:
                    return false;
                case 1:
                    return defs[0].first == b && defs[0].second == indexInBlock;
                default:
                    for (auto e : defs) if (e.first == b && e.second == indexInBlock)
                        return true;
                    return false;
            }
        }

        // This associates each variable with a specific operand, its location
        // at the current point in the allocation process.
        vec<Operand, 32> variableLocations;
        vec<Operand, 32> variableSlots;

        Operand slotForEdge(i32 edge) {
            auto& allocations = *ctx.allocations;
            if (allocations.edgeAllocations[edge].kind == Operand::Invalid) {
                Repr repr = passes->repr(U64);
                while (allocations.stack % repr.alignment())
                    allocations.stack ++;
                allocations.stack += repr.size();
                allocations.edgeAllocations[edge] = fn.memory(Target::fp, -allocations.stack);
                if UNLIKELY(config::verboseRegalloc)
                    println("[ALLOC]\tReserved stack slot ", OperandLogger { fn, allocations.edgeAllocations[edge] }, " for edge .bb", fn.edge(edge).srcIndex(), " -> .bb", fn.edge(edge).destIndex());
            }
            return allocations.edgeAllocations[edge];
        }

        Operand slotFor(i32 var) {
            if (variableSlots[var].kind == Operand::Invalid) {
                auto& allocations = *ctx.allocations;
                TypeIndex type = fn.variableList[var].type;
                Repr repr = passes->repr(type);
                while (allocations.stack % repr.alignment())
                    allocations.stack ++;
                allocations.stack += repr.size();
                variableSlots[var] = fn.memory(Target::fp, -allocations.stack);
                if UNLIKELY(config::verboseRegalloc)
                    println("[ALLOC]\tReserved stack slot ", OperandLogger { fn, variableSlots[var] }, " for variable ", OperandLogger { fn, fn.variableById(var) });
            }
            return variableSlots[var];
        }

        void evict(Block block, u32 indexInBlock, i32 var) {
            Operand loc = variableLocations[var];
            assert(loc.isReg());
            assert(regBindings[loc.gp] == var);
            freeRegs.add(loc.gp);
            regBindings[loc.gp] = -1;

            // Since we allocate in reverse, evicting a variable means emitting
            // a load, after the instruction that caused it to be evicted.

            fn.addInsertion(block, indexInBlock + 1, Function::Late);
            Operand slot = slotFor(var);
            fn.addNodeToInsertion(fn.addNode(Opcode::LOAD, fn.variableList[var].type, variableLocations[var], slot, magicTag));
            variableLocations[var] = slot;
            liveInSlot.on(var);

            if UNLIKELY(config::verboseRegalloc)
                println("[ALLOC]\tEvicted variable ", OperandLogger { fn, fn.variableById(var) }, " from register ", OperandLogger { fn, loc }, " to its canonical slot ", OperandLogger { fn, variableLocations[var] });
        }

        void evictGp(Block block, u32 indexInBlock, RegSet exclude) {
            for (mreg r : reversedValidGps) if (!exclude[63 - r]) {
                assert(!freeRegs[63 - r]);
                assert(regBindings[63 - r] != -1);
                evict(block, indexInBlock, regBindings[63 - r]);
                return;
            }
            unreachable("Could not find a valid register to evict.");
        }

        void evictFp(Block block, u32 indexInBlock, RegSet exclude) {
            for (mreg r : reversedValidFps) if (!exclude[63 - r]) {
                assert(!freeRegs[63 - r]);
                assert(regBindings[63 - r] != -1);
                evict(block, indexInBlock, regBindings[63 - r]);
                return;
            }
            unreachable("Could not find a valid register to evict.");
        }

        mreg allocateGp(Block block, u32 indexInBlock, RegSet preferred, RegSet exclude) {
            RegSet available = freeRegs & validGps & ~exclude;
            if (available.empty()) {
                evictGp(block, indexInBlock, exclude);
                available = freeRegs & validGps & ~exclude;
                assert(!available.empty());
            }
            mreg result;
            if ((available & preferred).size())
                result = (available & preferred).next();
            else if ((available & Target::caller_saved_gps()).size())
                result = (available & Target::caller_saved_gps()).next();
            else
                result = available.next();
            if (Target::callee_saved_gps()[result])
                ctx.allocations->calleeSaves.add(result);
            assert(freeRegs[result]);
            assert(regBindings[(u32)result] == -1);
            return result;
        }

        mreg allocateFp(Block block, u32 indexInBlock, RegSet preferred, RegSet exclude) {
            RegSet available = freeRegs & validFps & ~exclude;
            if (available.empty()) {
                evictFp(block, indexInBlock, exclude);
                available = freeRegs & validFps & ~exclude;
                assert(!available.empty());
            }
            mreg result;
            if ((available & preferred).size())
                result = (available & preferred).next();
            else if ((available & Target::caller_saved_fps()).size())
                result = (available & Target::caller_saved_fps()).next();
            else
                result = available.next();
            if (Target::callee_saved_fps()[result])
                ctx.allocations->calleeSaves.add(result);
            assert(freeRegs[result]);
            assert(regBindings[(u32)result] == -1);
            return result;
        }

        void reallocate(Block block, u32 indexInBlock, i32 var, RegSet exclude, RegSet preferred = RegSet()) {
            Operand previousLoc = variableLocations[var];
            assert(previousLoc.isReg());
            unbind(var);

            Operand loc = allocate(block, indexInBlock, var, exclude, preferred);

            // We rebound the variable, now we just need to figure out what
            // kind of move to generate.
            fn.addInsertion(block, indexInBlock + 1, Function::Late);

            // Again, because we generate in reverse, the "previous" location
            // is the location of the value after the current instruction. So
            // we actually need to move from our new location to that.
            TypeIndex type = fn.variableList[var].type;
            if (loc.kind == Operand::Memory)
                fn.addNodeToInsertion(fn.addNode(Opcode::STORE, type, previousLoc, loc, magicTag));
            else
                fn.addNodeToInsertion(fn.addNode(Opcode::MOV, type, previousLoc, loc));
        }

        void unbind(i32 var) {
            if (variableLocations[var].isReg()) {
                mreg reg = variableLocations[var].gp;
                assert(regBindings[(u32)reg] == var);
                assert(!freeRegs[reg]);

                if UNLIKELY(config::verboseRegalloc)
                    println("[ALLOC]\tUnbound variable ", OperandLogger { fn, fn.variableById(var) }, " from register ", OperandLogger { fn, variableLocations[var] });

                regBindings[(u32)reg] = -1;
                freeRegs.add(reg);
            } else if (variableLocations[var].kind == Operand::Memory) {
                liveInSlot.off(var);
                if UNLIKELY(config::verboseRegalloc)
                    println("[ALLOC]\tUnbound variable ", OperandLogger { fn, fn.variableById(var) }, " from stack slot ", OperandLogger { fn, variableLocations[var] });
            }
            variableLocations[var] = fn.invalid();
        }

        Operand tryAllocate(Block block, u32 indexInBlock, i32 var, RegSet exclude, RegSet preferred = RegSet()) {
            return allocate(block, indexInBlock, var, exclude, preferred, true);
        }

        Operand allocate(Block block, u32 indexInBlock, i32 var, RegSet exclude, RegSet preferred = RegSet(), bool mayFail = false) {
            auto& allocations = *ctx.allocations;

            if (variableLocations[var].isReg())
                return variableLocations[var];

            TypeIndex type = fn.variableList[var].type;

            if (variableLocations[var].kind == Operand::Memory && (pins.isPinned(var) || (!isFPType(type) && !isGPType(fn, type))))
                return variableLocations[var];

            if (!pins.isPinned(var)) {
                if (isFPType(type)) {
                    if (mayFail && (freeRegs & validFps & ~exclude).empty()) {
                        liveInSlot.on(var);
                        variableLocations[var] = slotFor(var);
                        if UNLIKELY(config::verboseRegalloc)
                            println("[ALLOC]\tAllocated variable ", OperandLogger { fn, fn.variableById(var) }, " to stack slot ", OperandLogger { fn, variableLocations[var] });
                        return variableLocations[var];
                    }

                    auto reg = allocateFp(block, indexInBlock, preferred, exclude);

                    if (variableLocations[var].kind == Operand::Memory) {
                        // This variable was either previously spilled to the
                        // stack, or we're in a loop and this is the first time
                        // this variable is being allocated to this reg,
                        // meaning we need to conservatively spill it to ensure
                        // it's live for any backedges. In either case, we need
                        // to ensure it ends up in its slot after this
                        // instruction.
                        fn.addInsertion(block, indexInBlock + 1, Function::Early);
                        fn.addNodeToInsertion(fn.addNode(Opcode::STORE, type, variableLocations[var], fn.fp(reg), magicTag));
                    }

                    variableLocations[var] = fn.fp(reg);
                    freeRegs.remove(reg);
                    regBindings[(u32)reg] = var;

                    if UNLIKELY(config::verboseRegalloc)
                        println("[ALLOC]\tAllocated variable ", OperandLogger { fn, fn.variableById(var) }, " to register ", OperandLogger { fn, variableLocations[var] });

                    return variableLocations[var];
                } else if (isGPType(fn, type)) {
                    if (mayFail && (freeRegs & validGps & ~exclude).empty()) {
                        liveInSlot.on(var);
                        variableLocations[var] = slotFor(var);
                        if UNLIKELY(config::verboseRegalloc)
                            println("[ALLOC]\tAllocated variable ", OperandLogger { fn, fn.variableById(var) }, " to stack slot ", OperandLogger { fn, variableLocations[var] });
                        return variableLocations[var];
                    }

                    auto reg = allocateGp(block, indexInBlock, preferred, exclude);

                    if (variableLocations[var].kind == Operand::Memory) {
                        // Ditto for gp types.
                        fn.addInsertion(block, indexInBlock + 1, Function::Early);
                        fn.addNodeToInsertion(fn.addNode(Opcode::STORE, type, variableLocations[var], fn.gp(reg), magicTag));
                    }

                    variableLocations[var] = fn.gp(reg);
                    freeRegs.remove(reg);
                    regBindings[(u32)reg] = var;

                    if UNLIKELY(config::verboseRegalloc)
                        println("[ALLOC]\tAllocated variable ", OperandLogger { fn, fn.variableById(var) }, " to register ", OperandLogger { fn, variableLocations[var] });

                    return variableLocations[var];
                }
            }

            // Must be a type that's too big to allocate, or a pinned variable.
            // In either case, put them in a stack slot.
            auto slot = slotFor(var);
            variableLocations[var] = slot;
            liveInSlot.on(var);
            if UNLIKELY(config::verboseRegalloc)
                println("[ALLOC]\tAllocated ", pins.isPinned(var) ? "pinned" : "aggregate", " variable ", OperandLogger { fn, fn.variableById(var) }, " to stack slot ", OperandLogger { fn, variableLocations[var] });
            return variableLocations[var];
        }

        // This represents the per-block state of the register allocator, or
        // more specifically the state in which we expect our variables at the
        // moment we enter a block we've already allocated.

        struct Binding {
            i32 var;
            Operand val;
        };

        struct Bindings {
            u32 remainingEdges = 0;
            bool isBackedge = false; // Technically redundant, but lets us do better assertions.
            vec<Binding, 8> bindings;
            biasedset<128> liveInSlot;

            inline void reset() {
                remainingEdges = 0;
                isBackedge = false;
                bindings.clear();
                liveInSlot.clear();
            }

            void bind(i32 var, Operand val) {
                bindings.push({ var, val });
            }
        };

        vec<Bindings*> bindingsPool;

        Bindings* acquireBindings() {
            if (bindingsPool.size())
                return bindingsPool.pop();
            return new Bindings();
        }

        void returnBindings(Bindings* bindings) {
            bindings->reset();
            bindingsPool.push(bindings);
        }

        Bindings* currentBindings(Block block, const_slice<i8> pos) {
            Bindings* bindings = acquireBindings();
            bindings->remainingEdges = block.predecessorIndices().size();

            for (i32 i = 0; i < 64; i ++) if (regBindings[i] != -1)
                bindings->bind(regBindings[i], Target::is_gp(i) ? fn.gp(i) : fn.fp(i));

            bindings->liveInSlot.clear();
            bindings->liveInSlot |= liveInSlot;

            if UNLIKELY(config::verboseRegalloc) {
                print("[ALLOC]\tBindings at ", pos, " of block .bb", block.index(), ": ");
                bool first = true;
                for (Binding& binding : bindings->bindings) {
                    if (!first) print(", ");
                    first = false;
                    print(OperandLogger { fn, fn.variableById(binding.var) }, " = ", OperandLogger { fn, binding.val });
                }
                for (i32 var : liveInSlot) {
                    if (!first) print(", ");
                    first = false;
                    print(OperandLogger { fn, fn.variableById(var) }, " = ", OperandLogger { fn, slotFor(var) });
                }
                println();
            }

            return bindings;
        }

        void resetAtBlockHead() {
            for (i32 i = 0; i < 64; i ++) if (regBindings[i] != -1)
                unbind(regBindings[i]);
            for (i32 var : liveInSlot)
                unbind(var);
        }

        // These are the bindings associated with each edge.
        vec<Bindings*, 16> edgeBindings;

        void initializeAtBlockTail(Block block) {
            auto& dominators = *ctx.dominators;
            vec<pair<EdgeIndex, Binding>> rebinds;
            vec<EdgeIndex, 4> backEdges;
            for (Edge succ : block.successors()) {
                Bindings* bindings = edgeBindings[succ.index()];
                if (dominators[succ.destIndex()].dominates(block.index())) {
                    // This is a backedge. Since we should only visit blocks
                    // after visiting all their successors, this edge shouldn't
                    // have had a binding assigned yet.
                    assert(!bindings);

                    if UNLIKELY(config::verboseRegalloc)
                        println("[ALLOC]\tFound backedge from .bb", block.index(), " to .bb", succ.destIndex());

                    // Because of this, instead of letting our successor tell
                    // us what to do, we actually need to tell our successor
                    // what our assignments were. We do this at the end of this
                    // function, once we've finished initializing variable
                    // locations based on our non-backedge successors. Then we
                    // pre-populate the Bindings of that edge, with the
                    // expectation that the target block (i.e. the loop header)
                    // will handle generating moves at its own head position.
                    backEdges.push(succ.index());
                    continue;
                }

                assert(bindings);
                assert(!bindings->isBackedge);

                edgeBindings[succ.index()] = nullptr;
                for (Binding binding : bindings->bindings) {
                    assert(binding.val.isReg());

                    if (regBindings[binding.val.gp] != -1)
                        rebinds.push({ succ.index(), binding });
                    else if (variableLocations[binding.var].kind == Operand::Invalid) {
                        if UNLIKELY(config::verboseRegalloc)
                            println("[ALLOC]\tSet location of variable ", OperandLogger { fn, fn.variableById(binding.var) }, " to incoming value ", OperandLogger { fn, binding.val }, " from block .bb", succ.destIndex());
                        variableLocations[binding.var] = binding.val;
                        assert(freeRegs[binding.val.gp]);
                        assert(regBindings[(u32)binding.val.gp] == -1);
                        freeRegs.remove(binding.val.gp);
                        regBindings[(u32)binding.val.gp] = binding.var;
                    } else if (variableLocations[binding.var] != binding.val) {
                        Operand src = variableLocations[binding.var], dest = binding.val;
                        TypeIndex type = fn.variableList[binding.var].type;
                        if (isFunction(fn, type)) // Needed to avoid any compound types in the move.
                            type = PTR;
                        if (src.isReg())
                            src.regType = type;
                        dest.regType = type;
                        succ.addMove(src, dest);
                    }
                }

                for (i32 var : bindings->liveInSlot) {
                    if (variableLocations[var].isReg()) {
                        Operand src = variableLocations[var], dest = slotFor(var);
                        TypeIndex type = fn.variableList[var].type;
                        if (isFunction(fn, type)) // Needed to avoid any compound types in the move.
                            type = PTR;
                        src.regType = type;
                        succ.addMove(src, dest);
                    } else if (variableLocations[var].kind == Operand::Invalid) {
                        variableLocations[var] = slotFor(var), liveInSlot.on(var);
                        if UNLIKELY(config::verboseRegalloc)
                            println("[ALLOC]\tSet location of variable ", OperandLogger { fn, fn.variableById(var) }, " to incoming value ", OperandLogger { fn, variableLocations[var] }, " from block .bb", succ.destIndex());
                    }
                }

                if (!-- bindings->remainingEdges)
                    returnBindings(bindings);
            }

            // If we encountered any conflicts, we just handle those here,
            // picking new registers/stack slots as necessary.
            for (auto p : rebinds) {
                if (variableLocations[p.second.var].kind == Operand::Invalid)
                    variableLocations[p.second.var] = allocate(block, 0, p.second.var, RegSet(), RegSet());
                Operand src = variableLocations[p.second.var], dest = p.second.val;
                if (src == dest)
                    continue;
                TypeIndex type = fn.variableList[p.second.var].type;
                if (isFunction(fn, type)) // Needed to avoid any compound types in the move.
                    type = PTR;
                if (src.isReg())
                    src.regType = type;
                if (dest.isReg())
                    dest.regType = type;
                fn.edge(p.first).addMove(src, dest);
            }

            auto& allocations = *ctx.allocations;
            for (Edge succ : block.successors()) {
                if (allocations.edgeAllocations[succ.index()].isReg() && !freeRegs[allocations.edgeAllocations[succ.index()].gp]) {
                    if (!(freeRegs & validGps).empty())
                        allocations.edgeAllocations[succ.index()] = fn.gp((freeRegs & validGps).next());
                    else
                        allocations.edgeAllocations[succ.index()] = slotForEdge(succ.index());
                }
            }

            // Finally, if we had any backedges, tell them (via assigning a
            // Bindings to them) about our allocation choices.
            if (backEdges.size()) {
                // This is where we utilize our approximate liveness
                // information - we know any variable live into the loop header
                // might be live into this point. So we need to guarantee those
                // variables are allocated to something.
                for (EdgeIndex ei : backEdges) {
                    Edge e = fn.edge(ei);
                    for (i32 var : liveIn[e.destIndex()]) {
                        Operand operand = tryAllocate(block, block.nodeIndices().size(), var, RegSet());
                        if UNLIKELY(config::verboseRegalloc)
                            println("[ALLOC]\tAllocated variable ", OperandLogger { fn, fn.variableById(var) }, " to ", OperandLogger { fn, operand }, " at .bb", block.index(), " since it is live-in to the loop header .bb", e.destIndex());
                    }
                }

                Bindings* bindings = currentBindings(block, cstring("tail"));

                // Unlike a normal bindings set, since this represents the
                // values at our tail position, we want this to persist for
                // each of our backedges specifically, not our predecessors.
                bindings->remainingEdges = backEdges.size();
                bindings->isBackedge = true;

                for (EdgeIndex ei : backEdges) {
                    assert(!edgeBindings[ei]);
                    edgeBindings[ei] = bindings;
                }
            }
        }

        void handleBackEdge(Block block, Edge edge) {
            Bindings* bindings = edgeBindings[edge.index()];
            assert(bindings); // Should have been created by the backedge source.
            assert(bindings->isBackedge);

            if UNLIKELY(config::verboseRegalloc)
                println("[ALLOC]\tProcessing incoming backedge from .bb", edge.srcIndex(), " to .bb", block.index());

            biasedset<128> handledVars;

            for (Binding binding : bindings->bindings) {
                i32 var = binding.var;

                // Note that since this is a backedge, the "source" of the move
                // is the binding flowing in from our successor - since it's on
                // an edge targeting our block.
                Operand src = binding.val;
                Operand dest = variableLocations[var];

                if (src != dest) {
                    assert(src.kind != Operand::Invalid);
                    if (dest.kind == Operand::Invalid) {
                        // If a variable isn't live into our block header, we
                        // can safely skip it. It must have been defined within
                        // the loop, and we either recorded it conservatively,
                        // or it arose from a lexically infinite loop.
                        continue;
                    }

                    // This assertion is needed to guarantee we have at least
                    // one register operand, which is necessary for the hack
                    // where we stuff the move's type information in the
                    // register operand so the lowering phase knows what type
                    // of move to generate. This should always be the case,
                    // because the only time a variable should be assigned to a
                    // memory location is if it's in its stack slot, and each
                    // variable should only have one unique stack slot across
                    // the whole function.
                    assert(src.kind != Operand::Memory && dest.kind != Operand::Memory);

                    TypeIndex type = fn.variableList[var].type;
                    if (isFunction(fn, type)) // Needed to avoid any compound types in the move.
                        type = PTR;
                    src.regType = type;
                    if (dest.isReg())
                        dest.regType = type;
                    edge.addMove(src, dest);

                    handledVars.on(var);
                }
            }

            if (!-- bindings->remainingEdges)
                returnBindings(bindings);
        }

        SinglePassRegisterAllocator(TargetSpecificPasses<Target>* passes_in, PassContext& ctx_in, Function& fn_in):
            passes(passes_in), ctx(ctx_in), fn(fn_in), pins(*ctx.pins) {

            validGps = Target::gps(), validFps = Target::fps();
            freeRegs = validGps | validFps;
            for (mreg r : validGps)
                reversedValidGps.add(63 - r);
            for (mreg r : validFps)
                reversedValidFps.add(63 - r);

            for (u32 i = 0; i < 64; i ++)
                regBindings[i] = -1;

            edgeBindings.expandTo(fn.edgeList.size(), nullptr);

            dominatingDefs.expandTo(fn.variableList.size());
            variableLocations.expandTo(fn.variableList.size(), fn.invalid());
            variableSlots.expandTo(fn.variableList.size(), fn.invalid());

            gen.expandTo(fn.blockList.size());
            kill.expandTo(fn.blockList.size());
            liveIn.expandTo(fn.blockList.size());

            magicTag = fn.stringOperand(cstring("magic"));
        }

        ~SinglePassRegisterAllocator() {
            for (auto bindings : bindingsPool)
                delete bindings;
        }

        void allocateNode(Block block, u32 indexInBlock) {
            Node node = block.node(indexInBlock);

            // First, we need to reserve scratches for this instruction. This
            // can be a pretty nasty situation. We start by trying to pick
            // registers from the end of the available register set, rather
            // than the start, to minimize overlap with any live variables. If
            // there are live variables in those registers at this point, then
            // we need to either momentarily spill them, or spill their whole
            // live range. Our heuristic is that if a variable is spilled more
            // than twice, or more times than it's used, then we spill its
            // whole range. Otherwise, we allow it to remain in a register.

            RegSet excludedRegs;
            RegSet availableScratches = reversedValidGps;

            auto& allocations = *ctx.allocations;

            for (u32 k = 0; k < (*ctx.scratchesPerNode)[node.index()]; k ++) {
                bool didAddScratch = false;
                for (mreg r : availableScratches) if (freeRegs[63 - r]) {
                    allocations.scratchesByInstruction[node.index()].push(63 - r);
                    availableScratches.remove(r);
                    excludedRegs.add(63 - r);
                    if UNLIKELY(config::verboseRegalloc)
                        println("[ALLOC]\tPicked free scratch register ", OperandLogger { fn, Target::is_gp(63 - r) ? fn.gp(63 - r) : fn.fp(63 - r) }, " for instruction ", node);
                    didAddScratch = true;
                    break;
                }
                if (didAddScratch)
                    continue;

                // If we reached here, then we didn't find any free scratch.
                // In this case, we evict the first variable we find occupying
                // a scratch register.
                for (mreg r : availableScratches) if (!freeRegs[63 - r]) {
                    allocations.scratchesByInstruction[node.index()].push(63 - r);
                    availableScratches.remove(r);
                    excludedRegs.add(63 - r);
                    assert(regBindings[63 - r] != -1);
                    evict(block, indexInBlock, regBindings[63 - r]);
                    if UNLIKELY(config::verboseRegalloc)
                        println("[ALLOC]\tPicked occupied scratch register ", OperandLogger { fn, Target::is_gp(63 - r) ? fn.gp(63 - r) : fn.fp(63 - r) }, " for instruction ", node);
                    break;
                }
            }

            // Next, we consider any clobbered registers. If this instruction
            // has clobbers, and there are live variables occupying them, those
            // variables unfortunately need to be globally spilled.

            RegSet clobbers = (*ctx.clobberList)[node.index()];
            excludedRegs = excludedRegs | clobbers;

            for (mreg r : ~freeRegs & clobbers) {
                assert(regBindings[(u32)r] != -1);
                if UNLIKELY(config::verboseRegalloc)
                    println("[ALLOC]\tReallocating ", OperandLogger { fn, fn.variableById(regBindings[(u32)r]) }, " due to clobbered register ", OperandLogger { fn, fn.gp(r) }, " in instruction ", node);
                reallocate(block, indexInBlock, regBindings[(u32)r], excludedRegs);
            }

            RegSet preferred;

            // Next, we consider the def, if the node has one. If the def is
            // not currently in a register, we allocate one (TODO: should we?).
            // If this is the first def of this variable along all possible
            // paths we could have taken to reach here, then we can safely free
            // up its register if it has one.

            if (hasDef(node.opcode())) {
                Operand def = node.def(0);
                node.def(0) = allocate(block, indexInBlock, def.var, excludedRegs);
                if (isFirstDef(block.index(), indexInBlock, def.var)) {
                    if (variableLocations[def.var].isReg())
                        preferred.add(variableLocations[def.var].gp);
                    unbind(def.var);
                }
            }

            if (node.opcode() == Opcode::RET && returnOperand.isReg())
                preferred.add(returnOperand.gp);

            CallSite* callSite = nullptr;
            if (node.opcode() == Opcode::CALL || node.opcode() == Opcode::CALL_VOID)
                callSite = &(*ctx.callSites)[(*ctx.callInsns)[node.index()]];

            // Finally, we allocate the uses.

            for (auto [i, use] : enumerate(node.uses())) {
                if (use.kind == Operand::Var) {
                    if (fn.variableList[use.var].parameterIndex != -1) {
                        // If a variable is a parameter, try and hint it to
                        // that register.
                        auto param = fn.variableList[use.var].parameterIndex;
                        if (fn.parameters[param].operand.isReg())
                            preferred.add(fn.parameters[param].operand.gp);
                    }
                    if (callSite) {
                        u32 startOfArguments = node.opcode() == Opcode::CALL ? 2 : 1;
                        // If we're passing this variable into a call, try and
                        // hint it to the parameter register.
                        if (i >= startOfArguments) {
                            Operand arg = callSite->parameters[i - startOfArguments];
                            if (arg.isReg())
                                preferred.add(arg.gp);
                        }
                    }
                    node.use(i) = allocate(block, indexInBlock, use.var, excludedRegs, preferred);
                }
            }

            // If this node was a call, we need to record all caller-saved
            // registers in flight across it.
            if (callSite) {
                for (mreg r : (~freeRegs & Target::caller_saves())) {
                    if (node.opcode() == Opcode::CALL_VOID || r != node.def(0).gp)
                        callSite->liveAcross.add(r);
                }
            }
        }

        void allocate() {
            ctx.require(PINS);
            ctx.require(CLOBBERS);
            ctx.require(DOMINATORS);
            ctx.did(ALLOCATE);

            auto& allocations = *ctx.allocations;
            allocations.initialize(ctx, fn, AllocationResult::AllocateInPlace, AllocationResult::PerInstructionScratchRegisters);

            auto& dominators = *ctx.dominators;

            computeDominatingDefs();
            for (mreg clobber : allClobbers) {
                reversedValidGps.remove(63 - clobber);
                reversedValidFps.remove(63 - clobber);
            }

            assert(reversedValidGps.size() >= 3);
            assert(reversedValidFps.size() >= 3);

            void* parameterState = Target::start_placing_parameters();
            if (fn.returnType != VOID) {
                auto returnPlacement = passes->placeReturnValue(fn, fn.returnType, parameterState);
                returnOperand = operandFromPlacement(fn, returnPlacement);
            }
            for (auto& param : fn.parameters) if (!isCompound(param.type) || isFunction(fn, param.type)) {
                Repr repr = passes->repr(param.type);
                auto placement = Target::place_scalar_parameter(parameterState, repr);
                param.operand = operandFromPlacement(fn, placement);
            }
            Target::finish_placing_parameters(parameterState);

            deque<BlockIndex> frontier;
            for (Block block : fn.blocks()) {
                if (block.successorIndices().size() == 0)
                    frontier.pushl(block.index());

                // In addition to terminal nodes, we need to include nodes that
                // would otherwise be unreachable from their successors - nodes
                // whose only successors are their loop headers. Since the only
                // live-in to these blocks is just whatever is live around the
                // loop, we already have all the information necessary to
                // handle them.

                bool isOnlyBackedge = true;
                for (Edge e : block.successors()) if (!dominators[e.destIndex()].dominates(block.index())) {
                    isOnlyBackedge = false;
                    break;
                }
                if (isOnlyBackedge)
                    frontier.pushl(block.index());
            }

            while (frontier.size()) {
                Block block = fn.block(frontier.popr());
                if (completedBlocks[block.index()])
                    continue;

                bool allSuccessorsDone = true;
                for (Edge succ : block.successors()) {
                    if (succ.destIndex() != block.index()
                        && !completedBlocks[succ.destIndex()]
                        && !dominators[succ.destIndex()].dominates(block.index())) {
                        allSuccessorsDone = false;
                        break;
                    }
                }
                if (!allSuccessorsDone) {
                    frontier.pushl(block.index());
                    continue;
                }

                if UNLIKELY(config::verboseRegalloc)
                    println("\n[ALLOC]\tBeginning register allocation for .bb", block.index());

                initializeAtBlockTail(block);

                for (i32 i = i32(block.nodeIndices().size()) - 1; i >= 0; i --)
                    allocateNode(block, i);

                Bindings* bindings = nullptr;
                if (block.predecessorIndices().size() > 0) {
                    bool isBackedgeTarget = false;
                    for (Edge pred : block.predecessors()) {
                        if (dominators[block.index()].dominates(pred.srcIndex()))
                            handleBackEdge(block, pred), isBackedgeTarget = true;
                        else {
                            if (!bindings)
                                bindings = currentBindings(block, cstring("head"));
                            assert(!edgeBindings[pred.index()]);
                            edgeBindings[pred.index()] = bindings;
                            frontier.pushl(pred.srcIndex());
                        }
                        if (!(freeRegs & validGps).empty())
                            allocations.edgeAllocations[pred.index()] = fn.gp((freeRegs & validGps).next());
                        else
                            allocations.edgeAllocations[pred.index()] = slotForEdge(pred.index());
                    }
                    resetAtBlockHead();
                } else if (block.index() == fn.entrypoint) {
                    for (i32 i = 0; i < 64; i ++) if (regBindings[i] != -1) {
                        i32 var = regBindings[i];
                        if (fn.variableList[var].parameterIndex != -1)
                            fn.parameters[fn.variableList[var].parameterIndex].operand = variableLocations[var];
                    }
                    for (i32 var : liveInSlot) if (fn.variableList[var].parameterIndex != -1)
                        fn.parameters[fn.variableList[var].parameterIndex].operand = variableLocations[var];
                }

                completedBlocks.on(block.index());
            }

            fn.executeInsertions();

            for (u32 i = allocations.scratchesByInstruction.size(); i < fn.nodeList.size(); i ++) {
                // We may have inserted some loads and stores in the process of
                // register allocation. These are guaranteed to not need a
                // scratch reg, but the lowering phase still expects a scratch
                // to potentially be available. So we populate some here.
                allocations.scratchesByInstruction.push({});
                allocations.scratchesByInstruction.back().push(0);
                allocations.scratchesByInstruction.back().push(0);
            }
        }
    };

    template<typename Target>
    void TargetSpecificPasses<Target>::allocateRegisters(PassContext& ctx, Function& fn, RegisterAllocationMode mode) {
        JASMINE_PASS(REGISTER_ALLOCATION);
        switch (mode) {
            case RegisterAllocationMode::SINGLE_PASS:
                SinglePassRegisterAllocator<Target>(this, ctx, fn).allocate();
                break;
            case RegisterAllocationMode::ADVANCED:
                allocateRegistersAdvanced(ctx, *this, fn, mode);
                break;
            default:
                unreachable("Unimplemented register allocation mode.");
        }
    }
}

#endif