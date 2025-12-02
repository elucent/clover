#ifndef JASMINE_PASS_LOWER_H
#define JASMINE_PASS_LOWER_H

#include "jasmine/pass/helpers.h"

namespace jasmine {
    enum MoveStatus : u8 {
        Unmoved, Moving, Moved
    };

    inline maybe<TypeIndex> canMoveWithRegister(TargetSpecificPassInterface* passes, Function& fn, TypeIndex type) {
        if (type < 0) {
            assert(type != VOID && type != UNDEFINED && type != INVALID && type != EXT);
            return some<TypeIndex>(type); // All primitive TypeKinds are either register types or invalid. We assume it's not invalid here.
        }
        auto repr = passes->repr(type);
        if (repr.size() > 8 || !isPowerOfTwo(repr.size())) // Assuming 64-bit register size.
            return none<TypeIndex>();
        return some<TypeIndex>(-5 - intLog2(repr.size()));
    }

    void loadAggregateIntoRegisters(Function& fn, Block& b, Repr repr, Operand first, Operand second, Operand src, Operand scratch) {
        assert(src.kind == Operand::Memory);

        // This function and its partner storeAggregateFromRegisters handle
        // moving an aggregate (array or struct, generally) into and out of
        // either one or two registers. This ends up being pretty complicated,
        // since even if a value fits within a register, there's no guarantee
        // that it's aligned to register size, or that reading a full register
        // from its address (even unaligned) won't read out of bounds. So, we
        // assume the type is padded out to alignment, and then generate a
        // series of loads to load each part, then combine them into a single
        // register payload with shifts and ORs.

        Operand reg;
        if (second.kind != Operand::Invalid) {
            // We always do one full load when loading into a pair, since if we
            // were smaller than a register, we should be passed as a single
            // register.
            b.addNode(Opcode::LOAD, first.kind == Operand::GP ? PTR : F64, first, src);
            src.offset += 8;
            reg = second;
        } else
            reg = first;

        // Next things get a little tricky. We assume that the type is padded
        // out to its alignment and that we can safely load past the end of
        // its data using this information. Then, we potentially do multiple
        // loads and need to OR them together.

        if (reg.kind == Operand::FP) {
            // Of course, for floats, we can only have
            // 32-bit and 64-bit. So we only need a
            // single load in either case.

            b.addNode(Opcode::LOAD, repr.size() > 12 ? F64 : F32, reg, fn.memory(src.base, src.offset));
        } else {
            assert(src.base != reg.gp); // This would be really weird, but it would mess everything up, so we assert it.
            u32 initialRemainder = repr.size() - (second.kind != Operand::Invalid ? 8 : 0), remainder = initialRemainder;
            remainder = roundUpToNearest(remainder, min(8u, repr.alignment()));
            if (!(remainder & remainder - 1)) { // Single power-of-two means we can do a single move.
                TypeIndex moveType = U8;
                if (remainder == 2) moveType = U16;
                if (remainder == 4) moveType = U32;
                if (remainder == 8) moveType = U64;
                b.addNode(Opcode::LOAD, moveType, reg, fn.memory(src.base, src.offset));
            } else while (remainder) {
                TypeIndex moveType = U64;
                u32 moveSize = 8;
                if (remainder & 1) moveSize = 1, moveType = U8;
                else if (remainder & 2) moveSize = 2, moveType = U16;
                else if (remainder & 4) moveSize = 4, moveType = U32;

                bool first = remainder == initialRemainder;
                remainder -= moveSize;
                if (first)
                    b.addNode(Opcode::LOAD, moveType, reg, fn.memory(src.base, src.offset + remainder));
                else {
                    b.addNode(Opcode::LOAD, moveType, scratch, fn.memory(src.base, src.offset + remainder));
                    b.addNode(Opcode::SHL, PTR, reg, reg, fn.intConst(moveSize * 8));
                    b.addNode(Opcode::OR, PTR, reg, reg, scratch);
                }
            }
        }
    }

    void storeAggregateFromRegisters(Function& fn, Block& b, Repr repr, Operand dest, Operand first, Operand second, Operand scratch) {
        assert(dest.kind == Operand::Memory);

        Operand reg;
        if (second.kind != Operand::Invalid) {
            b.addNode(Opcode::STORE, first.kind == Operand::GP ? PTR : F64, dest, first);
            dest.offset += 8;
            reg = second;
        } else
            reg = first;

        if (reg.kind == Operand::FP) {
            // Of course, for floats, we can only have
            // 32-bit and 64-bit. So we only need a
            // single load in either case.

            b.addNode(Opcode::STORE, repr.size() > 12 ? F64 : F32, fn.memory(dest.base, dest.offset), reg);
        } else {
            assert(dest.base != reg.gp); // This would be really weird, but it would mess everything up, so we assert it.
            u32 remainder = repr.size() - (second.kind != Operand::Invalid ? 8 : 0);
            remainder = roundUpToNearest(remainder, min(8u, repr.alignment()));
            if (!(remainder & remainder - 1)) { // Single power-of-two means we can do a single move.
                TypeIndex moveType = U8;
                if (remainder == 2) moveType = U16;
                if (remainder == 4) moveType = U32;
                if (remainder == 8) moveType = U64;
                b.addNode(Opcode::STORE, moveType, fn.memory(dest.base, dest.offset), reg);
            } else {
                b.addNode(Opcode::MOV, PTR, scratch, reg);
                u32 offset = 0;
                while (remainder) {
                    TypeIndex moveType = U8;
                    u32 moveSize = 1;
                    if (remainder >= 8) moveSize = 8, moveType = U64;
                    else if (remainder >= 4) moveSize = 4, moveType = U32;
                    else if (remainder >= 2) moveSize = 2, moveType = U16;

                    remainder -= moveSize;
                    b.addNode(Opcode::STORE, moveType, fn.memory(dest.base, dest.offset + offset), scratch);
                    offset += moveSize;
                    if (remainder) // This wasn't our last iteration.
                        b.addNode(Opcode::SHR, PTR, scratch, scratch, fn.intConst(moveSize * 8));
                }
            }
        }
    }

    template<typename Target>
    void makeMove(TargetSpecificPassInterface* passes, Function& fn, Block& b, TypeIndex type, Operand dest, Operand src, Operand scratch) {
        if (dest == src)
            return;
        if (dest.isReg() && src.isReg()) {
            assert(Target::is_gp(dest.gp) == Target::is_gp(src.gp));
            if (dest.gp == src.gp)
                return;
            b.addNode(Opcode::MOV, type, dest, src);
        } else if (dest.isReg() && src.isConst())
            b.addNode(Opcode::MOV, type, dest, src);
        else if (dest.isReg() && src.isLabel())
            b.addNode(Opcode::ADDR, type, dest, src);
        else if (src.isLabel()) {
            b.addNode(Opcode::ADDR, type, scratch, src);
            b.addNode(Opcode::STORE, type, dest, scratch);
        } else if (src.isConst()) {
            if UNLIKELY(src.kind == Operand::IntConst && !fits<I32>(fn.intValueOf(src))) {
                b.addNode(Opcode::MOV, type, scratch, src);
                b.addNode(Opcode::STORE, type, dest, scratch);
            } else
                b.addNode(Opcode::STORE, type, dest, src);
        } else if (dest.isReg()) {
            if (auto reg = canMoveWithRegister(passes, fn, type))
                b.addNode(Opcode::LOAD, *reg, dest, src);
            else
                loadAggregateIntoRegisters(fn, b, passes->repr(type), dest, fn.invalid(), src, scratch);
        } else if (src.isReg()) {
            if (auto reg = canMoveWithRegister(passes, fn, type))
                b.addNode(Opcode::STORE, *reg, dest, src);
            else
                storeAggregateFromRegisters(fn, b, passes->repr(type), dest, src, fn.invalid(), scratch);
        } else
            b = MemoryMover<Target>(passes, b, passes->repr(type))
                .dst(dest)
                .src(src)
                .generate(scratch);
    }

    template<typename Target>
    void makeMoveDestIndex(TargetSpecificPassInterface* passes, Function& fn, Block& b, TypeIndex type, Operand dest, Operand idx, Operand src, Operand ptrScratch, Operand moveScratch) {
        Repr elementRepr = passes->repr(type);
        if (auto reg = canMoveWithRegister(passes, fn, type)) {
            // Can emit a single indexed move.
            if (src.isConst()) {
                if UNLIKELY(src.kind == Operand::IntConst && !fits<I32>(fn.intValueOf(src))) {
                    b.addNode(Opcode::MOV, *reg, moveScratch, src);
                    b.addNode(Opcode::STORE_INDEX, *reg, dest, idx, moveScratch);
                } else
                    b.addNode(Opcode::STORE_INDEX, *reg, dest, idx, src);
            } else if (src.isReg())
                b.addNode(Opcode::STORE_INDEX, *reg, dest, idx, src);
            else {
                b.addNode(Opcode::ADDR_INDEX, *reg, ptrScratch, dest, idx);
                b = MemoryMover<Target>(passes, b, elementRepr)
                    .dst(fn.memory(ptrScratch.gp, 0))
                    .src(src)
                    .generate(moveScratch);
            }
        } else {
            b.addNode(Opcode::MOV, PTR, ptrScratch, idx);
            b.addNode(Opcode::MUL, PTR, ptrScratch, ptrScratch, fn.intConst(elementRepr.size()));
            b.addNode(Opcode::ADDR_INDEX, I8, ptrScratch, src, ptrScratch);
            makeMove<Target>(passes, fn, b, type, fn.memory(ptrScratch.gp, 0), src, moveScratch);
        }
    }

    template<typename Target>
    void makeMoveSrcIndex(TargetSpecificPassInterface* passes, Function& fn, Block& b, TypeIndex type, Operand dest, Operand src, Operand idx, Operand ptrScratch, Operand moveScratch) {
        Repr elementRepr = passes->repr(type);
        if (auto reg = canMoveWithRegister(passes, fn, type)) {
            // Can emit a single indexed move.
            if (dest.isReg())
                b.addNode(Opcode::LOAD_INDEX, *reg, dest, src, idx);
            else {
                b.addNode(Opcode::ADDR_INDEX, *reg, ptrScratch, src, idx);
                b = MemoryMover<Target>(passes, b, elementRepr)
                    .dst(dest)
                    .src(fn.memory(ptrScratch.gp, 0))
                    .generate(moveScratch);
            }
        } else {
            b.addNode(Opcode::MOV, PTR, ptrScratch, idx);
            b.addNode(Opcode::MUL, PTR, ptrScratch, ptrScratch, fn.intConst(elementRepr.size()));
            b.addNode(Opcode::ADDR_INDEX, I8, ptrScratch, src, ptrScratch);
            makeMove<Target>(passes, fn, b, type, dest, fn.memory(ptrScratch.gp, 0), moveScratch);
        }
    }

    template<typename Target>
    void makeMoveFromLabel(TargetSpecificPassInterface* passes, Function& fn, Block& b, TypeIndex type, Operand dest, Operand src, Operand ptrScratch, Operand scratch) {
        assert(src.isLabel());
        if (auto registerType = canMoveWithRegister(passes, fn, type)) {
            if (dest.isReg())
                b.addNode(Opcode::LOAD, *registerType, dest, src);
            else {
                b.addNode(Opcode::LOAD, *registerType, scratch, src);
                b.addNode(Opcode::STORE, *registerType, dest, scratch);
            }
        } else {
            b.addNode(Opcode::ADDR, PTR, ptrScratch, src);
            b = MemoryMover<Target>(passes, b, passes->repr(type))
                .dst(dest)
                .src(fn.memory(ptrScratch.gp, 0))
                .generate(scratch);
        }
    }

    template<typename Target>
    void makeMoveToLabel(TargetSpecificPassInterface* passes, Function& fn, Block& b, TypeIndex type, Operand dest, Operand src, Operand ptrScratch, Operand scratch) {
        assert(dest.isLabel());
        if (src.isConst()) {
            assert(canMoveWithRegister(passes, fn, type));
            if UNLIKELY(src.kind == Operand::IntConst && !fits<I32>(fn.intValueOf(src))) {
                b.addNode(Opcode::MOV, type, scratch, src);
                b.addNode(Opcode::STORE, type, dest, scratch);
            } else
                b.addNode(Opcode::STORE, type, dest, src);
        } else if (auto registerType = canMoveWithRegister(passes, fn, type)) {
            if (src.isReg())
                b.addNode(Opcode::STORE, *registerType, dest, src);
            else {
                b.addNode(Opcode::LOAD, *registerType, scratch, src);
                b.addNode(Opcode::STORE, *registerType, dest, scratch);
            }
        } else {
            b.addNode(Opcode::ADDR, PTR, ptrScratch, dest);
            b = MemoryMover<Target>(passes, b, passes->repr(type))
                .dst(fn.memory(ptrScratch.gp, 0))
                .src(src)
                .generate(scratch);
        }
    }

    template<typename Target>
    void parallelMove(TargetSpecificPassInterface* passes, Function& fn, Block& b, Operand gpScratch, Operand fpScratch, slice<TypeIndex> moveTypes, slice<Move> moves, vec<MoveStatus>& status, u32 i) {
        Move& move = moves[i];
        auto dest = move.dest;
        auto src = move.src;
        if (dest == src) {
            status[i] = Moved;
            return;
        }
        auto scratchFor = [&](Operand operand) -> Operand { return operand.kind == Operand::FP ? fpScratch : gpScratch; };
        status[i] = Moving;
        for (u32 j = 0; j < moves.size(); j ++) {
            auto tmpSrc = moves[j].src;
            if (tmpSrc == dest) {
                switch (status[j]) {
                    case Unmoved:
                        parallelMove<Target>(passes, fn, b, gpScratch, fpScratch, moveTypes, moves, status, j);
                        break;
                    case Moving: {
                        Operand scratch = scratchFor(move.src);
                        makeMove<Target>(passes, fn, b, moveTypes[j], scratch, tmpSrc, scratch); // This seems possibl wrong...
                        moves[j].src = scratch;
                        break;
                    }
                    case Moved:
                        break;
                }
            }
        }
        makeMove<Target>(passes, fn, b, moveTypes[i], dest, move.src, scratchFor(move.src));
        status[i] = Moved;
    }

    template<typename Target>
    void parallelMove(TargetSpecificPassInterface* passes, Function& fn, Block& b, Operand gpScratch, Operand fpScratch, slice<TypeIndex> moveTypes, slice<Move> moves) {
        vec<MoveStatus> status;
        status.expandTo(moves.size());
        for (u32 i = 0; i < moves.size(); i ++) {
            status[i] = moves[i].src == moves[i].dest ? Moved : Unmoved;
            assert(moves[i].src != gpScratch);
            assert(moves[i].dest != gpScratch);
            assert(moves[i].src != fpScratch);
            assert(moves[i].dest != fpScratch);
        }
        for (u32 i = 0; i < moves.size(); i ++) {
            if (status[i] == Unmoved)
                parallelMove<Target>(passes, fn, b, gpScratch, fpScratch, moveTypes, moves, status, i);
        }
    }

    struct RegPairMove {
        TypeIndex type;
        Operand dest;
        Operand first, second;
    };

    Operand fold(Function& fn, Node node, Operand a, Operand b) {
        // This is kind of an unfortunate function. As a result of only
        // breaking Sizeof operands into integers here, we can run into
        // situations where instructions have two constant operands, meaning
        // we need to be able to fold them since native instructions won't
        // be able to handle two immediates at once.

        i64 av = fn.intValueOf(a), bv = fn.intValueOf(b);
        i64 result = 0;
        switch (node.opcode()) {
            case Opcode::ADD:
                result = av + bv;
                break;
            case Opcode::SUB:
                result = av - bv;
                break;
            case Opcode::MUL:
                result = av * bv;
                break;
            case Opcode::DIV:
                if (isSigned(node.type())) {
                    result = av / bv;
                    break;
                }
                result = u64(av) / u64(bv);
                break;
            case Opcode::REM:
                if (isSigned(node.type())) {
                    result = av / bv;
                    break;
                }
                result = u64(av) / u64(bv);
                break;
            case Opcode::AND:
                result = av & bv;
                break;
            case Opcode::OR:
                result = av | bv;
                break;
            case Opcode::XOR:
                result = av ^ bv;
                break;
            case Opcode::ROL:
                if (node.type() == I8 || node.type() == U8)
                    result = rol<u8>(av, bv);
                else if (node.type() == I16 || node.type() == U16)
                    result = rol<u16>(av, bv);
                else if (node.type() == I32 || node.type() == U32)
                    result = rol<u32>(av, bv);
                else if (node.type() == I64 || node.type() == U64)
                    result = rol<u64>(av, bv);
                else if (node.type() == PTR || node.type() == REF)
                    result = rol<uptr>(av, bv);
                break;
            case Opcode::ROR:
                if (node.type() == I8 || node.type() == U8)
                    result = ror<u8>(av, bv);
                else if (node.type() == I16 || node.type() == U16)
                    result = ror<u16>(av, bv);
                else if (node.type() == I32 || node.type() == U32)
                    result = ror<u32>(av, bv);
                else if (node.type() == I64 || node.type() == U64)
                    result = ror<u64>(av, bv);
                else if (node.type() == PTR || node.type() == REF)
                    result = ror<uptr>(av, bv);
                break;
            case Opcode::SHL:
                if (node.type() == I8 || node.type() == U8)
                    result = u8(av) << u8(bv);
                else if (node.type() == I16 || node.type() == U16)
                    result = u16(av) << u16(bv);
                else if (node.type() == I32 || node.type() == U32)
                    result = u32(av) << u32(bv);
                else if (node.type() == I64 || node.type() == U64)
                    result = u64(av) << u64(bv);
                else if (node.type() == PTR || node.type() == REF)
                    result = uptr(av) << uptr(bv);
                break;
            case Opcode::SHR:
                if (node.type() == I8)
                    result = i8(av) >> i8(bv);
                else if (node.type() == U8)
                    result = u8(av) >> u8(bv);
                else if (node.type() == I16)
                    result = i16(av) >> i16(bv);
                else if (node.type() == U16)
                    result = u16(av) >> u16(bv);
                else if (node.type() == I32)
                    result = i32(av) >> i32(bv);
                else if (node.type() == U32)
                    result = u32(av) >> u32(bv);
                else if (node.type() == I64)
                    result = i64(av) >> i64(bv);
                else if (node.type() == U64)
                    result = u64(av) >> u64(bv);
                else if (node.type() == PTR || node.type() == REF)
                    result = uptr(av) >> uptr(bv);
                break;
            case Opcode::IS_EQ:
                result = av == bv ? 1 : 0;
                break;
            case Opcode::IS_NE:
                result = av != bv ? 1 : 0;
                break;
            case Opcode::IS_LT:
                if (isSigned(node.type())) {
                    result = av < bv ? 1 : 0;
                    break;
                }
                result = u64(av) < u64(bv) ? 1 : 0;
                break;
            case Opcode::IS_LE:
                if (isSigned(node.type())) {
                    result = av <= bv ? 1 : 0;
                    break;
                }
                result = u64(av) <= u64(bv) ? 1 : 0;
                break;
            case Opcode::IS_GT:
                if (isSigned(node.type())) {
                    result = av > bv ? 1 : 0;
                    break;
                }
                result = u64(av) > u64(bv) ? 1 : 0;
                break;
            case Opcode::IS_GE:
                if (isSigned(node.type())) {
                    result = av >= bv ? 1 : 0;
                    break;
                }
                result = u64(av) >= u64(bv) ? 1 : 0;
                break;
            default:
                unreachable("Unfoldable instruction ", node);
        }
        return fn.intConst(result);
    }

    Operand fold(Function& fn, Node node, Operand o) {
        i64 v = fn.intValueOf(o);
        i64 result;
        switch (node.opcode()) {
            case Opcode::NEG:
                result = -v;
                break;
            case Opcode::NOT:
                result = ~v;
                break;
            default:
                // TODO: We should handle bit-counting instructions here too.
                unreachable("Unfoldable instruction ", node);
        }
        return fn.intConst(result);
    }

    maybe<bool> foldBranch(Function& fn, Node node, Operand a) {
        if (a.kind != Operand::IntConst)
            return none<bool>();
        i64 av = fn.intValueOf(a);
        switch (node.opcode()) {
            case Opcode::BR_IF:
                return some<bool>(av != 0);
            case Opcode::BR_IF_NOT:
                return some<bool>(av == 0);
            default:
                return none<bool>();
        }
    }

    maybe<bool> foldBranch(Function& fn, Node node, Operand a, Operand b) {
        if (a.kind != Operand::IntConst || b.kind != Operand::IntConst)
            return none<bool>();
        i64 av = fn.intValueOf(a), bv = fn.intValueOf(b);
        switch (node.opcode()) {
            case Opcode::BR_EQ:
                return some<bool>(av == bv);
            case Opcode::BR_NE:
                return some<bool>(av != bv);
            case Opcode::BR_LT:
                if (isSigned(node.type()))
                    return some<bool>(av < bv);
                return some<bool>(u64(av) < u64(bv));
            case Opcode::BR_LE:
                if (isSigned(node.type()))
                    return some<bool>(av <= bv);
                return some<bool>(u64(av) <= u64(bv));
            case Opcode::BR_GT:
                if (isSigned(node.type()))
                    return some<bool>(av > bv);
                return some<bool>(u64(av) > u64(bv));
            case Opcode::BR_GE:
                if (isSigned(node.type()))
                    return some<bool>(av >= bv);
                return some<bool>(u64(av) >= u64(bv));
            default:
                return none<bool>();
        }
    }

    template<typename Target>
    void TargetSpecificPasses<Target>::lower(PassContext& ctx, Function& fn) {
        JASMINE_PASS(LOWERING);
        ctx.require(ALLOCATE);
        ctx.did(LOWER);

        auto& allocations = *ctx.allocations;
        auto& callInsns = *ctx.callInsns;
        auto& callSites = *ctx.callSites;

        // We allocate new empty blocks to hold the lowered IR, and map the old blocks to the new ones.
        // This is normally 1:1, with the special exception that we generate a few extra blocks to resolve
        // control flow edges if necessary.

        vec<BlockIndex> oldToNewMapping;
        vec<BlockIndex> newToFinalMapping;
        vec<i32> moveBlocks;
        breakCriticalEdges(fn, oldToNewMapping, newToFinalMapping, moveBlocks);

        // Now we get to the stack-allocation specific code. Since all variables are stored on the stack, in
        // order to use them in instructions, we need to load them into registers first. Constants also need to
        // be materialized in most cases.

        // First, we apply the results of our allocation.

        vec<vec<TypeIndex>> originalEdgeTypes;

        for (Edge edge : fn.edges()) {
            originalEdgeTypes.push({});
            for (Move move : edge.moves()) {
                assert(move.dest.kind == Operand::Var);
                originalEdgeTypes.back().push(fn.variableList[move.dest.var].type);
            }
        }

        bool hasPins = ctx.has(PINS) && ctx.pins->count();
        const Pins* pins = hasPins ? &ctx.pins.get() : nullptr;

        if (allocations.allocationMode == AllocationResult::AllocateVariables) {
            for (Block block : fn.blocks()) for (Node node : block.nodes()) {
                for (Operand& operand : node.operands()) if (operand.kind == Operand::Var) {
                    Operand newOperand = hasPins && pins->isPinned(operand.var) ? allocations.allocationForPinned(operand.var) : allocations.allocations[operand.var];
                    if (newOperand.kind == Operand::Invalid)
                        crash();
                    operand = newOperand;
                }
            }
            for (Edge edge : fn.edges()) for (Move& move : edge.moves()) {
                if (move.src.kind == Operand::Var) move.src = allocations.allocations[move.src.var];
                if (move.dest.kind == Operand::Var) move.dest = allocations.allocations[move.dest.var];
            }
        } else if (allocations.allocationMode == AllocationResult::AllocateLiveRanges) {
            auto& liveness = *ctx.liveness;
            for (Block block : fn.blocks()) {
                auto rangeIndices = liveness.indicesInBlock(block.index());
                for (u32 i : rangeIndices) {
                    const LiveRange& range = liveness[i];
                    if (range.start == 0) for (Edge edge : block.predecessors()) for (Move& move : edge.moves()) {
                        if (move.dest.kind == Operand::Var && move.dest.var == range.var)
                            move.dest = allocations.allocations[i];
                    }

                    for (u32 j = max<u16>(1, range.start); j <= range.end; j ++) {
                        if (j - 1 == block.nodeIndices().size())
                            break;
                        Node node = block.node(j - 1);
                        for (Operand& operand : node.operands()) {
                            if (operand.kind == Operand::Var && operand.var == range.var)
                                operand = allocations.allocations[i];
                        }
                    }

                    if (range.end == block.nodeIndices().size() + 1) for (Edge edge : block.successors()) for (Move& move : edge.moves()) {
                        if (move.src.kind == Operand::Var && move.src.var == range.var)
                            move.src = allocations.allocations[i];
                    }
                }
                if (hasPins) for (Node node : block.nodes()) for (Operand& operand : node.operands()) if (operand.kind == Operand::Var && pins->isPinned(operand.var)) {
                    operand = allocations.allocationForPinned(operand.var);
                }
            }
        } else
            unreachable("Unexpected allocation mode.");

        if UNLIKELY(config::verboseRegalloc) {
            println("Function after stack/register allocation:\n");
            println(fn);
        }

        // Next, some helpers to lower the individual instructions.

        auto materialize = [&](Block& b, TypeIndex type, Operand src, Operand scratch) -> Operand {
            if (src.isReg())
                return src;
            else if (src.isConst()) {
                if (src.kind == Operand::IntConst && !fits<I32>(b.function->intValueOf(src))) {
                    b.addNode(Opcode::MOV, type, scratch, src);
                    return scratch;
                }
                return src;
            } else if (src.isLabel()) {
                b.addNode(Opcode::ADDR, PTR, scratch, src);
                return scratch;
            } else if (src.kind == Operand::Memory) {
                auto reg = canMoveWithRegister(this, fn, type);
                assert(reg);
                b.addNode(Opcode::LOAD, *reg, scratch, src);
                return scratch;
            } else
                unreachable("Can't materialize operand ", OperandLogger { fn, src });
        };

        auto writeBack = [&](Block& b, TypeIndex type, Operand dst, Operand tmpDst) {
            if (dst.isReg())
                assert(dst == tmpDst);
            else {
                auto reg = canMoveWithRegister(this, fn, type);
                assert(reg);
                b.addNode(Opcode::STORE, *reg, dst, tmpDst);
            }
        };

        auto makeBinary = [&](Function& fn, Block& b, Node& n, slice<Operand> operands, Opcode opcode) {
            Operand input = materialize(b, n.type(), operands[1], allocations.scratch0(n));
            Operand output;
            if UNLIKELY(input.kind == Operand::IntConst) {
                output = fold(fn, n, input);
                makeMove<Target>(this, fn, b, n.type(), operands[0], output, allocations.scratch0(n));
                return;
            }
            output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
            b.addNode(opcode, n.type(), output, input);
            writeBack(b, n.type(), operands[0], output);
        };

        auto makeTernary = [&](Function& fn, Block& b, Node& n, slice<Operand> operands, Opcode opcode) {
            Operand lhs = materialize(b, n.type(), operands[1], allocations.scratch0(n));
            Operand rhs = materialize(b, n.type(), operands[2], allocations.scratch1(n));
            Operand output;
            if UNLIKELY(lhs.kind == Operand::IntConst && rhs.kind == Operand::IntConst) {
                output = fold(fn, n, lhs, rhs);
                makeMove<Target>(this, fn, b, n.type(), operands[0], output, allocations.scratch0(n));
                return;
            }
            output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
            b.addNode(opcode, n.type(), output, lhs, rhs);
            writeBack(b, n.type(), operands[0], output);
        };

        auto makeCompare = [&](Function& fn, Block& b, Node& n, slice<Operand> operands, Opcode opcode) {
            Operand lhs = materialize(b, n.type(), operands[1], allocations.scratch0(n));
            Operand rhs = materialize(b, n.type(), operands[2], allocations.scratch1(n));
            Operand output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
            b.addNode(opcode, n.type(), output, lhs, rhs);
            writeBack(b, I8, operands[0], output);
        };

        auto makeCompareBranch = [&](Function& fn, Block& b, Node& n, Operand lhs, Operand rhs, Operand dest, Opcode opcode) {
            lhs = materialize(b, n.type(), lhs, allocations.scratch0(n));
            rhs = materialize(b, n.type(), rhs, allocations.scratch1(n));
            b.addNode(opcode, n.type(), lhs, rhs, dest);
        };

        auto* passes = this;
        auto makeEdge = [&](Function& fn, Block& b, EdgeIndex edge) {
            auto moves = fn.edge(edge).moves();
            if (moves.size() == 0)
                return;
            if (moves.size() == 1) {
                auto dest = moves[0].dest;
                auto src = moves[0].src;
                makeMove<Target>(passes, fn, b, originalEdgeTypes[edge][0], dest, src, allocations.edgeAllocations[edge]);
                return;
            }

            parallelMove<Target>(passes, fn, b, allocations.edgeAllocations[edge], fn.fp(Target::fps().next()), originalEdgeTypes[edge], moves);
        };

        vec<mreg, 8> usedCalleeSaves;
        for (mreg calleeSave : allocations.calleeSaves)
            usedCalleeSaves.push(calleeSave);

        u32 maxStackArguments = 0;

        MaybePair<ASMVal> returnPlacement = { {} };
        Operand savedReturnPointer;

        for (BlockIndex bi : indices(oldToNewMapping)) {
            Block old = fn.block(bi);
            Block b = fn.block(oldToNewMapping[bi]);
            if (bi == fn.entrypoint) {
                for (mreg calleeSave : usedCalleeSaves)
                    b.addNode(Opcode::PUSH, Target::is_gp(calleeSave) ? PTR : F64, Target::is_gp(calleeSave) ? fn.gp(calleeSave) : fn.fp(calleeSave));
                void* state = Target::start_placing_parameters();
                if (fn.returnType != VOID) {
                    returnPlacement = placeReturnValue(fn, fn.returnType, state);
                    if (returnPlacement.first.kind == ASMVal::MEM) {
                        // Handle memory return values. This is kind of a pain, and
                        // currently we do a really naive thing. On most ABIs, we
                        // get a pointer to the buffer to write the return value
                        // into from the caller, as a "secret" first argument. We
                        // *should* factor this into register allocation. But this
                        // is probably rare, and slow anyway, so for now we just
                        // stuff it on the stack.
                        allocations.stack = roundUpToNearest(allocations.stack, 8);
                        savedReturnPointer = fn.memory(Target::fp, -(allocations.stack += 8));
                        allocations.stack = roundUpToNearest(allocations.stack, 16);
                        b.addNode(Opcode::STORE, PTR, savedReturnPointer, fn.gp(returnPlacement.first.base));
                    }
                }

                vec<Move> parameterMoves;
                vec<TypeIndex> parameterMoveTypes;
                vec<RegPairMove> parameterPairMoves;
                for (auto p : fn.parameters) {
                    auto arg = placeParameter(fn, p.type, state);
                    Operand parameterOperand;
                    if (allocations.allocationMode == AllocationResult::AllocateVariables)
                        parameterOperand = allocations.allocations[p.operand.var];
                    else if (allocations.allocationMode == AllocationResult::AllocateLiveRanges) {
                        if UNLIKELY(hasPins && pins->isPinned(p.operand.var))
                            parameterOperand = allocations.allocationForPinned(p.operand.var);
                        else {
                            auto& liveness = *ctx.liveness;
                            for (LiveRangeIndex i : liveness.indicesInBlock(bi)) {
                                if (liveness[i].start > 0)
                                    break;
                                if (liveness[i].var == p.operand.var) {
                                    parameterOperand = allocations.allocations[i];
                                    break;
                                }
                            }
                        }
                    } else
                        unreachable("Unknown allocation mode.");
                    if (isCompound(p.type) && !isFunction(fn, p.type) && (arg.isPair() || arg.first.is_reg())) {
                        Operand paramFirst = arg.first.kind == ASMVal::GP ? fn.gp(arg.first.gp) : fn.fp(arg.first.fp);
                        Operand paramSecond = arg.isPair()
                            ? (arg.second.kind == ASMVal::GP ? fn.gp(arg.second.gp) : fn.fp(arg.second.fp))
                            : fn.invalid();
                        parameterPairMoves.push({ p.type, parameterOperand, paramFirst, paramSecond });
                    } else {
                        parameterMoveTypes.push(p.type);
                        if (arg.first.kind == ASMVal::MEM) {
                            // Fix up stack arguments to be relative to our
                            // frame pointer instead of the caller's stack.
                            assert(allocations.stack); // Otherwise we wouldn't push the frame pointer.
                            parameterMoves.push({ fn.memory(Target::fp, arg.first.offset + 16), parameterOperand });
                        } else
                            parameterMoves.push({ operandFromPlacement(fn, arg), parameterOperand });
                    }
                }
                Target::finish_placing_parameters(state);

                Operand gpScratch, fpScratch;
                RegSet available = Target::gps() | Target::fps();
                for (const auto& move : parameterMoves) {
                    if (move.src.isReg()) available.remove(move.src.gp);
                    if (move.dest.isReg()) available.remove(move.dest.gp);
                    if (move.src.kind == Operand::Memory) available.remove(move.src.base);
                    if (move.dest.kind == Operand::Memory) available.remove(move.dest.base);
                }
                for (const auto& pairMove : parameterPairMoves) {
                    if (pairMove.dest.isReg()) available.remove(pairMove.dest.gp);
                    if (pairMove.dest.kind == Operand::Memory) available.remove(pairMove.dest.base);
                    if (pairMove.first.isReg()) available.remove(pairMove.first.gp);
                    if (pairMove.first.kind == Operand::Memory) available.remove(pairMove.first.base);
                    if (pairMove.second.isReg()) available.remove(pairMove.second.gp);
                    if (pairMove.second.kind == Operand::Memory) available.remove(pairMove.second.base);
                }
                gpScratch = fn.gp((available & Target::gps()).next());
                fpScratch = fn.fp((available & Target::fps()).next());

                for (const auto& pairMove : parameterPairMoves)
                    storeAggregateFromRegisters(fn, b, this->repr(pairMove.type), pairMove.dest, pairMove.first, pairMove.second, gpScratch);
                parallelMove<Target>(this, fn, b, gpScratch, fpScratch, parameterMoveTypes, parameterMoves);
            }

            auto pointerRepr = repr(PTR);

            for (Node n : old.nodes()) {
                vec<Operand> operands;
                for (Operand o : n.operands()) {
                    if UNLIKELY(o.kind == Operand::Sizeof)
                        operands.push(fn.intConst(repr(o.type).size()));
                    else
                        operands.push(o);
                }
                switch (n.opcode()) {
                    case Opcode::NOP:
                        break;
                    case Opcode::COMMENT:
                        b.addNode(n);
                        break;
                    case Opcode::VAR:
                        // These only exist to reserve space for a variable, so
                        // now that we've assigned stack slots to variables,
                        // we can drop them.
                        break;
                    case Opcode::MOV:
                        makeMove<Target>(this, fn, b, n.type(), operands[0], operands[1], allocations.scratch0(n));
                        break;
                    case Opcode::GET_FIELD: {
                        auto fieldType = fn.typeContext()[n.type()].fields()[fn.intValueOf(operands[2])];
                        auto fieldRepr = repr(fieldType);
                        assert(operands[1].kind == Operand::Memory); // Compounds should always be allocated to stack slots...for now.
                        i64 offsetStart = offset(fn, n.type(), fn.intValueOf(operands[2])) + operands[1].offset;
                        i64 offsetEnd = offsetStart + fieldRepr.size();
                        if (fitsSigned<20>(offsetStart) && fitsSigned<20>(offsetEnd - 1)) // It's okay to not be inclusive on the end.
                            makeMove<Target>(this, fn, b, fieldType, operands[0], fn.memory(operands[1].base, offsetStart), allocations.scratch0(n));
                        else {
                            // If the dest is a general-purpose register, we
                            // can clobber it, since no other operand can be in
                            // a register.
                            Operand scratch = operands[0].kind == Operand::GP ? operands[0] : allocations.scratch1(n);
                            b.addNode(Opcode::ADDR, PTR, scratch, operands[1]);
                            b.addNode(Opcode::ADD, PTR, scratch, scratch, fn.intConst(offsetStart - operands[1].offset));
                            makeMove<Target>(this, fn, b, fieldType, operands[0], fn.memory(scratch.gp, 0), allocations.scratch0(n));
                        }
                        break;
                    }
                    case Opcode::SET_FIELD: {
                        auto fieldType = fn.typeContext()[n.type()].fields()[fn.intValueOf(operands[1])];
                        auto fieldRepr = repr(fieldType);
                        Operand src = operands[2];
                        assert(operands[0].kind == Operand::Memory); // Compounds should always be allocated to stack slots...for now.
                        i64 offsetStart = offset(fn, n.type(), fn.intValueOf(operands[1])) + operands[0].offset;
                        i64 offsetEnd = offsetStart + fieldRepr.size();
                        if (fitsSigned<20>(offsetStart) && fitsSigned<20>(offsetEnd - 1)) // It's okay to not be inclusive on the end.
                            makeMove<Target>(this, fn, b, fieldType, fn.memory(operands[0].base, offsetStart), src, allocations.scratch0(n));
                        else {
                            Operand scratch = allocations.scratch0(n);
                            b.addNode(Opcode::ADDR, PTR, scratch, operands[0]);
                            b.addNode(Opcode::ADD, PTR, scratch, scratch, fn.intConst(offsetStart - operands[0].offset));
                            makeMove<Target>(this, fn, b, fieldType, fn.memory(scratch.gp, 0), operands[0], allocations.scratch1(n));
                        }
                        break;
                    }
                    case Opcode::ADDR_FIELD: {
                        auto fieldType = fn.typeContext()[n.type()].fields()[fn.intValueOf(operands[2])];
                        auto fieldRepr = repr(fieldType);
                        assert(operands[1].kind == Operand::Memory); // Compounds should always be allocated to stack slots...for now.
                        i64 offsetStart = offset(fn, n.type(), fn.intValueOf(operands[2])) + operands[1].offset;
                        Operand output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
                        if (fitsSigned<20>(offsetStart)) // It's okay to not be inclusive on the end.
                            b.addNode(Opcode::ADDR, PTR, output, fn.memory(operands[1].base, offsetStart));
                        else {
                            Operand scratch = allocations.scratch0(n);
                            b.addNode(Opcode::ADDR, PTR, scratch, operands[1]);
                            b.addNode(Opcode::ADD, PTR, output, scratch, fn.intConst(offsetStart - operands[1].offset));
                        }
                        writeBack(b, PTR, operands[0], output);
                        break;
                    }
                    case Opcode::LOAD_FIELD: {
                        auto fieldType = fn.typeContext()[n.type()].fields()[fn.intValueOf(operands[2])];
                        auto fieldRepr = repr(fieldType);
                        Operand srcPtr = materialize(b, PTR, operands[1], allocations.scratch0(n));
                        i64 off = offset(fn, n.type(), fn.intValueOf(operands[2]));
                        assert(fitsSigned<20>(off + fieldRepr.size() - 1));
                        makeMove<Target>(this, fn, b, fieldType, operands[0], fn.memory(srcPtr.gp, off), allocations.scratch1(n));
                        break;
                    }
                    case Opcode::OFFSET_FIELD: {
                        auto fieldType = fn.typeContext()[n.type()].fields()[fn.intValueOf(operands[2])];
                        auto fieldRepr = repr(fieldType);
                        Operand srcPtr = materialize(b, PTR, operands[1], allocations.scratch0(n));
                        i64 off = offset(fn, n.type(), fn.intValueOf(operands[2]));
                        assert(fitsSigned<20>(off + fieldRepr.size() - 1));
                        Operand output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
                        b.addNode(Opcode::ADDR, PTR, output, fn.memory(srcPtr.gp, off));
                        writeBack(b, PTR, operands[0], output);
                        break;
                    }
                    case Opcode::STORE_FIELD: {
                        auto fieldType = fn.typeContext()[n.type()].fields()[fn.intValueOf(operands[1])];
                        auto fieldRepr = repr(fieldType);
                        Operand dstPtr = materialize(b, PTR, operands[0], allocations.scratch0(n));
                        i64 off = offset(fn, n.type(), fn.intValueOf(operands[1]));
                        assert(fitsSigned<20>(off + fieldRepr.size() - 1));
                        makeMove<Target>(this, fn, b, fieldType, fn.memory(dstPtr.gp, off), operands[2], allocations.scratch1(n));
                        break;
                    }
                    case Opcode::GET_INDEX: {
                        auto elementType = n.type();
                        auto elementRepr = repr(elementType);
                        assert(operands[1].kind == Operand::Memory); // Compounds should always be allocated to stack slots...for now.
                        Operand base = operands[1];
                        Operand index = operands[3];
                        if (index.isConst()) {
                            i64 offsetStart = base.offset + elementRepr.size() * fn.intValueOf(index);
                            i64 offsetEnd = offsetStart + elementRepr.size();
                            if (fitsSigned<20>(offsetStart) && fitsSigned<20>(offsetEnd - 1)) { // It's okay to not be inclusive on the end.
                                makeMove<Target>(this, fn, b, elementType, operands[0], fn.memory(base.base, offsetStart), allocations.scratch0(n));
                                break;
                            } else {
                                b.addNode(Opcode::MOV, PTR, allocations.scratch0(n), fn.intConst(offsetStart - base.offset));
                                makeMoveSrcIndex<Target>(this, fn, b, elementType, operands[0], base, allocations.scratch0(n), allocations.scratch0(n), allocations.scratch1(n));
                                break;
                            }
                        }
                        // Either the index is not a constant, or it would've resulted in too big of a memory offset.
                        index = materialize(b, operands[2].type, index, allocations.scratch0(n));
                        auto indexRepr = repr(operands[2].type);
                        if (indexRepr.size() < pointerRepr.size())
                            b.addNode(Opcode::CONVERT, PTR, index, operands[2], index);

                        // A little bit of a cheat - we reuse the index scratch for the pointer scratch. This is
                        // fine with the current implementation, but hard to assert that we don't break it in the
                        // future. It's probably important to ensure we keep the register pressure a little lower
                        // on this common operation though.
                        makeMoveSrcIndex<Target>(this, fn, b, elementType, operands[0], base, index, allocations.scratch0(n), allocations.scratch1(n));
                        break;
                    }
                    case Opcode::SET_INDEX: {
                        auto elementType = n.type();
                        auto elementRepr = repr(elementType);
                        assert(operands[0].kind == Operand::Memory); // Compounds should always be allocated to stack slots...for now.
                        Operand base = operands[0];
                        Operand index = operands[2];
                        if (index.isConst()) {
                            i64 offsetStart = base.offset + elementRepr.size() * fn.intValueOf(index);
                            i64 offsetEnd = offsetStart + elementRepr.size();
                            if (fitsSigned<20>(offsetStart) && fitsSigned<20>(offsetEnd - 1)) { // It's okay to not be inclusive on the end.
                                makeMove<Target>(this, fn, b, elementType, fn.memory(base.base, offsetStart), operands[3], allocations.scratch0(n));
                                break;
                            } else {
                                b.addNode(Opcode::MOV, PTR, allocations.scratch0(n), fn.intConst(offsetStart - base.offset));
                                makeMoveDestIndex<Target>(this, fn, b, elementType, base, allocations.scratch0(n), operands[3], allocations.scratch0(n), allocations.scratch1(n));
                                break;
                            }
                        }
                        // Either the index is not a constant, or it would've resulted in too big of a memory offset.
                        index = materialize(b, operands[1].type, operands[2], allocations.scratch0(n));
                        auto indexRepr = repr(operands[1].type);
                        if (indexRepr.size() < pointerRepr.size())
                            b.addNode(Opcode::CONVERT, PTR, index, operands[2], index);
                        makeMoveDestIndex<Target>(this, fn, b, elementType, base, index, operands[3], allocations.scratch0(n), allocations.scratch1(n));
                        break;
                    }
                    case Opcode::ADDR_INDEX: {
                        auto elementType = n.type();
                        auto elementRepr = repr(elementType);
                        assert(operands[1].kind == Operand::Memory); // Compounds should always be allocated to stack slots...for now.
                        Operand base = operands[1];
                        Operand index = operands[3];
                        Operand output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
                        if (index.isConst()) {
                            i64 offsetStart = base.offset + elementRepr.size() * fn.intValueOf(index);
                            if (fitsSigned<20>(offsetStart)) { // It's okay to not be inclusive on the end.
                                b.addNode(Opcode::ADDR, PTR, output, fn.memory(base.base, offsetStart));
                            } else {
                                b.addNode(Opcode::MOV, PTR, allocations.scratch1(n), fn.intConst(offsetStart - base.offset));
                                b.addNode(Opcode::ADDR_INDEX, I8, output, base, allocations.scratch1(n));
                            }
                        } else {
                            // Either the index is not a constant, or it would've resulted in too big of a memory offset.
                            index = materialize(b, operands[2].type, index, allocations.scratch0(n));
                            auto indexRepr = repr(operands[2].type);
                            if (indexRepr.size() < pointerRepr.size())
                                b.addNode(Opcode::CONVERT, PTR, index, operands[2], index);

                            if (elementRepr.kind() != Size::MEMORY && elementRepr.kind() != Size::VECTOR)
                                b.addNode(Opcode::ADDR_INDEX, elementType, output, base, index);
                            else {
                                b.addNode(Opcode::MUL, PTR, allocations.scratch0(n), index, fn.intConst(elementRepr.size()));
                                b.addNode(Opcode::ADDR_INDEX, I8, output, base, allocations.scratch0(n));
                            }
                        }
                        writeBack(b, PTR, operands[0], output);
                        break;
                    }
                    case Opcode::LOAD_INDEX: {
                        auto elementType = n.type();
                        auto elementRepr = repr(elementType);

                        Operand srcPtr = materialize(b, PTR, operands[1], allocations.scratch0(n));
                        Operand index = operands[3];
                        if (index.isConst()) {
                            i64 off = fn.intValueOf(index) * elementRepr.size();
                            if (fitsSigned<20>(off + elementRepr.size() - 1))
                                makeMove<Target>(this, fn, b, elementType, operands[0], fn.memory(srcPtr.gp, off), allocations.scratch1(n));
                            else {
                                b.addNode(Opcode::MOV, PTR, allocations.scratch1(n), fn.intConst(off));
                                b.addNode(Opcode::ADD, PTR, allocations.scratch0(n), srcPtr, index);
                                makeMove<Target>(this, fn, b, elementType, operands[0], fn.memory(allocations.scratch0(n).gp, off), allocations.scratch1(n));
                            }
                        } else {
                            index = materialize(b, operands[2].type, index, allocations.scratch1(n));
                            auto indexRepr = repr(operands[2].type);
                            if (indexRepr.size() < pointerRepr.size())
                                b.addNode(Opcode::CONVERT, PTR, index, operands[2], index);

                            // Even more of a hack than the direct indexed accesses, makeMoveSrcIndex currently safely
                            // allows us to reuse specifically the index scratch register as the pointer scratch. So we
                            // do that. Any change to the pointer materialization logic though could potentially throw
                            // this out of whack though, so be careful.
                            makeMoveSrcIndex<Target>(this, fn, b, elementType, operands[0], fn.memory(srcPtr.gp, 0), index, allocations.scratch1(n), allocations.scratch0(n));
                        }
                        break;
                    }
                    case Opcode::OFFSET_INDEX: {
                        auto elementType = n.type();
                        auto elementRepr = repr(elementType);

                        Operand srcPtr = materialize(b, PTR, operands[1], allocations.scratch0(n));
                        Operand index = operands[3];
                        Operand output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
                        if (index.isConst()) {
                            i64 off = fn.intValueOf(index) * elementRepr.size();
                            if (!off)
                                makeMove<Target>(this, fn, b, PTR, output, srcPtr, allocations.scratch0(n));
                            else if (fits<I32>(off))
                                b.addNode(Opcode::ADD, PTR, output, srcPtr, fn.intConst(off));
                            else {
                                b.addNode(Opcode::MOV, PTR, allocations.scratch1(n), fn.intConst(off));
                                b.addNode(Opcode::ADD, PTR, output, srcPtr, allocations.scratch1(n));
                            }
                            writeBack(b, PTR, operands[0], output);
                        } else if (elementRepr.kind() != Size::MEMORY && elementRepr.kind() != Size::VECTOR) {
                            index = materialize(b, operands[2].type, index, allocations.scratch1(n));
                            auto indexRepr = repr(operands[2].type);
                            if (indexRepr.size() < pointerRepr.size())
                                b.addNode(Opcode::CONVERT, PTR, index, operands[2], index);

                            b.addNode(Opcode::ADDR_INDEX, elementType, output, fn.memory(srcPtr.gp, 0), index);
                            writeBack(b, PTR, operands[0], output);
                        } else {
                            index = materialize(b, operands[2].type, index, allocations.scratch1(n));
                            auto indexRepr = repr(operands[2].type);
                            if (indexRepr.size() < pointerRepr.size())
                                b.addNode(Opcode::CONVERT, PTR, index, operands[2], index);

                            b.addNode(Opcode::MUL, PTR, allocations.scratch1(n), index, fn.intConst(elementRepr.size()));
                            b.addNode(Opcode::ADD, PTR, output, srcPtr, allocations.scratch1(n));
                            writeBack(b, PTR, operands[0], output);
                        }
                        break;
                    }
                    case Opcode::STORE_INDEX: {
                        auto elementType = n.type();
                        auto elementRepr = repr(elementType);

                        Operand dstPtr = materialize(b, PTR, operands[0], allocations.scratch0(n));
                        Operand index = operands[2];
                        if (index.isConst()) {
                            i64 off = fn.intValueOf(index) * elementRepr.size();
                            if (fitsSigned<20>(off + elementRepr.size() - 1))
                                makeMove<Target>(this, fn, b, elementType, fn.memory(dstPtr.gp, off), operands[3], allocations.scratch1(n));
                            else {
                                b.addNode(Opcode::MOV, PTR, allocations.scratch1(n), fn.intConst(off));
                                b.addNode(Opcode::ADD, PTR, allocations.scratch0(n), dstPtr, index);
                                makeMove<Target>(this, fn, b, elementType, fn.memory(allocations.scratch0(n).gp, off), operands[3], allocations.scratch1(n));
                            }
                        } else {
                            index = materialize(b, operands[1].type, index, allocations.scratch1(n));
                            auto indexRepr = repr(operands[1].type);
                            if (indexRepr.size() < pointerRepr.size())
                                b.addNode(Opcode::CONVERT, PTR, index, operands[1], index);
                            makeMoveDestIndex<Target>(this, fn, b, elementType, fn.memory(dstPtr.gp, 0), index, operands[3], allocations.scratch1(n), allocations.scratch0(n));
                        }
                        break;
                    }
                    case Opcode::LOAD: {
                        if (operands[1].isLabel()) {
                            makeMoveFromLabel<Target>(this, fn, b, n.type(), operands[0], operands[1], allocations.scratch0(n), allocations.scratch1(n));
                            break;
                        }
                        Operand src = materialize(b, PTR, operands[1], allocations.scratch0(n));
                        src = fn.memory(src.gp, 0); // Load directly from the pointer.
                        makeMove<Target>(this, fn, b, n.type(), operands[0], src, allocations.scratch1(n));
                        break;
                    }
                    case Opcode::STORE: {
                        if (operands[0].isLabel()) {
                            makeMoveToLabel<Target>(this, fn, b, n.type(), operands[0], operands[1], allocations.scratch0(n), allocations.scratch1(n));
                            break;
                        }
                        Operand dest = materialize(b, PTR, operands[0], allocations.scratch0(n));
                        makeMove<Target>(this, fn, b, n.type(), fn.memory(dest.gp, 0), operands[1], allocations.scratch1(n));
                        break;
                    }
                    case Opcode::ADDR: {
                        Operand output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
                        assert(!operands[1].isReg());
                        b.addNode(Opcode::ADDR, n.type(), output, operands[1]);
                        writeBack(b, PTR, operands[0], output);
                        break;
                    }
                    case Opcode::ALLOCA: {
                        Operand size = materialize(b, n.type(), operands[1], allocations.scratch0(n));
                        Operand output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
                        b.addNode(Opcode::ALLOCA, n.type(), output, size);
                        writeBack(b, PTR, operands[0], output);
                        break;
                    }
                    case Opcode::NEG:
                    case Opcode::NOT:
                    case Opcode::SQRT:
                    case Opcode::ABS:
                    case Opcode::ROUND:
                    case Opcode::CEIL:
                    case Opcode::FLOOR:
                        makeBinary(fn, b, n, operands, n.opcode());
                        break;
                    case Opcode::ADD:
                    case Opcode::SUB:
                    case Opcode::MUL:
                    case Opcode::DIV:
                    case Opcode::REM:
                    case Opcode::AND:
                    case Opcode::OR:
                    case Opcode::XOR:
                    case Opcode::SHL:
                    case Opcode::SHR:
                    case Opcode::ROL:
                    case Opcode::ROR:
                    case Opcode::MIN:
                    case Opcode::MAX:
                        makeTernary(fn, b, n, operands, n.opcode());
                        break;
                    case Opcode::IS_LT:
                    case Opcode::IS_LE:
                    case Opcode::IS_GT:
                    case Opcode::IS_GE:
                    case Opcode::IS_EQ:
                    case Opcode::IS_NE:
                        makeCompare(fn, b, n, operands, n.opcode());
                        break;
                    case Opcode::BITCAST:
                    case Opcode::CONVERT: {
                        Repr srcRepr = repr(operands[1].type), destRepr = repr(n.type());
                        if (srcRepr.kind() == destRepr.kind() && srcRepr.kind() != Size::MEMORY) {
                            makeMove<Target>(this, fn, b, n.type(), operands[0], operands[2], allocations.scratch0(n));
                            break;
                        }
                        Operand src = materialize(b, operands[1].type, operands[2], allocations.scratch0(n));
                        if UNLIKELY(src.isConst())
                            makeMove<Target>(this, fn, b, operands[1].type, src = allocations.scratch0(n), operands[2], allocations.scratch0(n));
                        Operand output = operands[0].isReg() ? operands[0] : allocations.scratch0(n);
                        b.addNode(n.opcode(), n.type(), output, operands[1], src);
                        writeBack(b, n.type(), operands[0], output);
                        break;
                    }
                    case Opcode::BR:
                        makeEdge(fn, b, n.operand(0).edge);
                        b.addNode(n);
                        break;
                    case Opcode::BR_IF: {
                        Edge ifTrue = fn.edge(operands[1].edge), ifFalse = fn.edge(operands[2].edge);
                        Operand cond = materialize(b, n.type(), operands[0], allocations.scratch0(n));
                        i32 moveBlock = moveBlocks[bi];
                        if (auto tryFold = foldBranch(fn, n, operands[0])) {
                            makeEdge(fn, b, *tryFold ? ifTrue.index() : ifFalse.index());
                            b.addNode(Opcode::BR, VOID, *tryFold ? operands[1] : operands[2]);
                        } else if (isTrivialEdge(ifTrue)) {
                            assert(moveBlock == -1);
                            b.addNode(Opcode::BR_IF, n.type(), cond, operands[1]);
                            makeEdge(fn, b, ifFalse.index());
                            b.addNode(Opcode::BR, VOID, operands[2]);
                        } else if (isTrivialEdge(ifFalse)) {
                            assert(moveBlock == -1);
                            b.addNode(Opcode::BR_IF_NOT, n.type(), cond, operands[2]);
                            makeEdge(fn, b, ifTrue.index());
                            b.addNode(Opcode::BR, VOID, operands[1]);
                        } else {
                            assert(moveBlock >= 0);
                            Block moves = fn.block(moveBlock);
                            b.addNode(Opcode::BR_IF_NOT, n.type(), cond, fn.branch(fn.addEdge(b, moves)));
                            makeEdge(fn, b, ifTrue.index());
                            b.addNode(Opcode::BR, VOID, operands[1]);
                            makeEdge(fn, moves, ifFalse.index());
                            moves.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(moveBlock, ifFalse.destIndex())));
                            fn.removeEdge(ifFalse.index());
                        }
                        break;
                    }
                    case Opcode::BR_IF_NOT: {
                        Edge ifTrue = fn.edge(operands[1].edge), ifFalse = fn.edge(operands[2].edge);
                        Operand cond = materialize(b, n.type(), operands[0], allocations.scratch0(n));
                        i32 moveBlock = moveBlocks[bi];
                        if (auto tryFold = foldBranch(fn, n, operands[0])) {
                            makeEdge(fn, b, *tryFold ? ifTrue.index() : ifFalse.index());
                            b.addNode(Opcode::BR, VOID, *tryFold ? operands[1] : operands[2]);
                        } else if (isTrivialEdge(ifTrue)) {
                            assert(moveBlock == -1);
                            b.addNode(Opcode::BR_IF_NOT, n.type(), cond, operands[1]);
                            makeEdge(fn, b, ifFalse.index());
                            b.addNode(Opcode::BR, VOID, operands[2]);
                        } else if (isTrivialEdge(ifFalse)) {
                            assert(moveBlock == -1);
                            b.addNode(Opcode::BR_IF, n.type(), cond, operands[2]);
                            makeEdge(fn, b, ifTrue.index());
                            b.addNode(Opcode::BR, VOID, operands[1]);
                        } else {
                            assert(moveBlock >= 0);
                            Block moves = fn.block(moveBlock);
                            b.addNode(Opcode::BR_IF, n.type(), cond, fn.branch(fn.addEdge(b, moves)));
                            makeEdge(fn, b, ifTrue.index());
                            b.addNode(Opcode::BR, VOID, operands[1]);
                            makeEdge(fn, moves, ifFalse.index());
                            moves.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(moveBlock, ifFalse.destIndex())));
                            fn.removeEdge(ifFalse.index());
                        }
                        break;
                    }
                    case Opcode::BR_LT:
                    case Opcode::BR_LE:
                    case Opcode::BR_GT:
                    case Opcode::BR_GE:
                    case Opcode::BR_EQ:
                    case Opcode::BR_NE: {
                        Edge ifTrue = fn.edge(operands[2].edge), ifFalse = fn.edge(operands[3].edge);
                        i32 moveBlock = moveBlocks[bi];
                        if (auto tryFold = foldBranch(fn, n, operands[0], operands[1])) {
                            makeEdge(fn, b, *tryFold ? ifTrue.index() : ifFalse.index());
                            b.addNode(Opcode::BR, VOID, *tryFold ? operands[2] : operands[3]);
                        } else if (isTrivialEdge(ifTrue)) {
                            assert(moveBlock == -1);
                            makeCompareBranch(fn, b, n, operands[0], operands[1], operands[2], n.opcode());
                            makeEdge(fn, b, ifFalse.index());
                            b.addNode(Opcode::BR, VOID, operands[3]);
                        } else if (isTrivialEdge(ifFalse)) {
                            assert(moveBlock == -1);
                            makeCompareBranch(fn, b, n, operands[0], operands[1], operands[3], invert(n.opcode()));
                            makeEdge(fn, b, ifTrue.index());
                            b.addNode(Opcode::BR, VOID, operands[2]);
                        } else {
                            assert(moveBlock >= 0);
                            Block moves = fn.block(moveBlock);
                            makeCompareBranch(fn, b, n, operands[0], operands[1], fn.branch(fn.addEdge(b, moves)), invert(n.opcode()));
                            makeEdge(fn, b, ifTrue.index());
                            b.addNode(Opcode::BR, VOID, operands[2]);
                            makeEdge(fn, moves, ifFalse.index());
                            moves.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(moveBlock, ifFalse.destIndex())));
                            fn.removeEdge(ifFalse.index());
                        }
                        break;
                    }
                    case Opcode::CALL_VOID:
                    case Opcode::CALL: {
                        auto& compound = fn.typeContext()[n.type()];

                        vec<Operand> callerSaves;
                        const CallSite& callSite = callSites[callInsns[n.index()]];
                        for (mreg r : callSite.liveAcross) {
                            callerSaves.push(Target::is_gp(r) ? fn.gp(r) : fn.fp(r));
                            b.addNode(Opcode::PUSH, Target::is_gp(r) ? PTR : F64, callerSaves.back());
                        }

                        RegSet nonArgumentScratches = Target::caller_saved_gps();
                        for (Operand param : callSite.parameters) {
                            if (param.isReg())
                                nonArgumentScratches.remove(param.gp);
                            else if (param.kind == Operand::RegPair)
                                nonArgumentScratches.remove(param.ra), nonArgumentScratches.remove(param.rb);
                        }
                        if (n.opcode() == Opcode::CALL) {
                            auto rv = callSite.returnValue;
                            if (rv.isReg())
                                nonArgumentScratches.remove(rv.gp);
                            else if (rv.kind == Operand::RegPair)
                                nonArgumentScratches.remove(rv.ra), nonArgumentScratches.remove(rv.rb);
                            else
                                nonArgumentScratches.remove(rv.base), nonArgumentScratches.remove(rv.offset);
                        }
                        Operand moveScratch = fn.gp(nonArgumentScratches.next());

                        if (n.opcode() == Opcode::CALL && callSite.returnValue.kind == Operand::Memory) {
                            // Pass the address of our output to the callee.
                            assert(operands[0].kind == Operand::Memory);
                            b.addNode(Opcode::ADDR, PTR, fn.gp(callSite.returnValue.base), operands[0]);
                        }
                        for (auto [i, t] : enumerate(compound.arguments())) {
                            auto arg = callSite.parameters[i];
                            Operand argumentOperand = n.opcode() == Opcode::CALL_VOID ? operands[1 + i] : operands[2 + i];
                            if (isCompound(t) && !isFunction(fn, t) && (arg.kind == Operand::RegPair || arg.isReg())) {
                                Operand paramFirst, paramSecond;
                                if (arg.isReg())
                                    paramFirst = arg, paramSecond = fn.invalid();
                                else {
                                    paramFirst = Target::is_gp(arg.ra) ? fn.gp(arg.ra) : fn.fp(arg.ra);
                                    paramSecond = Target::is_gp(arg.rb) ? fn.gp(arg.rb) : fn.fp(arg.rb);
                                }
                                loadAggregateIntoRegisters(fn, b, this->repr(t), paramFirst, paramSecond, argumentOperand, moveScratch);
                            } else {
                                makeMove<Target>(this, fn, b, t, arg, argumentOperand, moveScratch);
                                if (arg.kind == Operand::Memory) {
                                    assert(arg.base == Target::sp);
                                    maxStackArguments = max(maxStackArguments, u32(arg.offset + repr(t).size()));
                                }
                            }
                        }

                        Operand callee = n.opcode() == Opcode::CALL_VOID ? operands[0] : operands[1];
                        if (callee.kind == Operand::Memory)
                            callee = materialize(b, PTR, callee, moveScratch);
                        b.addNode(Opcode::CALL, n.type(), callee);

                        if (n.opcode() == Opcode::CALL && operands[0].kind != Operand::Var && isCompound(compound.returnType()) && !isFunction(fn, compound.returnType())
                            && (callSite.returnValue.kind == Operand::RegPair || callSite.returnValue.isReg())) {
                            auto rv = callSite.returnValue;
                            Operand paramFirst, paramSecond;
                            if (rv.isReg())
                                paramFirst = rv, paramSecond = fn.invalid();
                            else {
                                paramFirst = Target::is_gp(rv.ra) ? fn.gp(rv.ra) : fn.fp(rv.ra);
                                paramSecond = Target::is_gp(rv.rb) ? fn.gp(rv.rb) : fn.fp(rv.rb);
                            }
                            storeAggregateFromRegisters(fn, b, this->repr(compound.returnType()), operands[0], paramFirst, paramSecond, moveScratch);
                        } else if (compound.returnType() != VOID && operands[0].kind != Operand::Var) {
                            auto rv = callSite.returnValue;
                            assert(rv.kind != Operand::RegPair);
                            if (rv.kind == Operand::Memory) {
                                // Copy out of the pointer we were returned.
                                makeMove<Target>(this, fn, b, compound.returnType(), operands[0], fn.memory(rv.offset, 0), moveScratch);
                            } else
                                makeMove<Target>(this, fn, b, compound.returnType(), operands[0], rv, moveScratch);
                        }
                        for (Operand o : reversed(callerSaves))
                            b.addNode(Opcode::POP, o.kind == Operand::GP ? PTR : F64, o);
                        break;
                    }
                    case Opcode::RET: {
                        if (n.type() != VOID) {
                            if (isCompound(n.type()) && !isFunction(fn, n.type()) && (returnPlacement.isPair() || returnPlacement.first.is_reg())) {
                                Operand paramFirst = returnPlacement.first.kind == ASMVal::GP ? fn.gp(returnPlacement.first.gp) : fn.fp(returnPlacement.first.fp);
                                Operand paramSecond = returnPlacement.isPair()
                                    ? (returnPlacement.second.kind == ASMVal::GP ? fn.gp(returnPlacement.second.gp) : fn.fp(returnPlacement.second.fp))
                                    : fn.invalid();
                                loadAggregateIntoRegisters(fn, b, this->repr(n.type()), paramFirst, paramSecond, operands[0], allocations.scratch0(n));
                            } else {
                                assert(!returnPlacement.isPair());
                                if (returnPlacement.first.kind == ASMVal::MEM) {
                                    assert(returnPlacement.first.memkind == ASMVal::REG_OFFSET);

                                    // We do something weird with memory return
                                    // values, in that the placement is not a
                                    // real memory address. The base is the
                                    // register by which we were given the
                                    // return buffer by the caller, and the
                                    // offset encodes the register by which we
                                    // should return it back to the caller.
                                    b.addNode(Opcode::LOAD, PTR, fn.gp(returnPlacement.first.offset), savedReturnPointer);
                                    makeMove<Target>(this, fn, b, n.type(), fn.memory(returnPlacement.first.offset, 0), operands[0], allocations.scratch0(n));
                                } else
                                    makeMove<Target>(this, fn, b, n.type(), operandFromPlacement(fn, returnPlacement), operands[0], allocations.scratch0(n));
                            }
                        }
                        for (mreg calleeSave : reversed(usedCalleeSaves))
                            b.addNode(Opcode::POP, Target::is_gp(calleeSave) ? PTR : F64, Target::is_gp(calleeSave) ? fn.gp(calleeSave) : fn.fp(calleeSave));
                        b.addNode(Opcode::RET, n.type());
                        break;
                    }
                    case Opcode::TRAP:
                        b.addNode(Opcode::TRAP, VOID);
                        break;
                    default:
                        unreachable("Unimplemented lowering for opcode ", OPCODE_NAMES[(u32)n.opcode()], ".");
                }
            }
        }
        allocations.stack += maxStackArguments;
        allocations.stack = roundUpToNearest(allocations.stack, 16);

        removeOldBlocks(fn, newToFinalMapping);
    }
}

#endif