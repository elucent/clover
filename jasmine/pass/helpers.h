#ifndef JASMINE_PASS_REPR_H
#define JASMINE_PASS_REPR_H

#include "jasmine/pass.h"
#include "jasmine/type.h"

namespace jasmine {
    template<typename Target>
    i32 TargetSpecificPasses<Target>::offset(Function& fn, TypeIndex aggregate, i32 field) const {
        assert(isStruct(fn.typeContext(), aggregate));
        assert(aggregate >= 0 && (u32)aggregate < structFieldIndices.size());
        return fieldOffsets[structFieldIndices[aggregate] + field];
    }

    template<typename Target>
    MaybePair<ASMVal> TargetSpecificPasses<Target>::placeParameter(Function& fn, TypeIndex type, void* state) const {
        if (isFunction(fn.typeContext(), type))
            type = PTR; // Functions are passed as pointers.
        if (isCompound(type)) {
            vec<Repr> reprs;
            const auto& compound = fn.typeContext()[type];
            if (compound.typeKind == CompoundType::ARRAY) {
                Repr repr = this->repr(compound.elementType());
                if (max(repr.size(), repr.alignment()) * compound.length() > 64)
                    reprs.push(Repr::Memory(repr.alignment(), compound.length() * repr.size()));
                else for (u32 i = 0; i < compound.length(); i ++)
                    reprs.push(repr);
            } else if (compound.typeKind == CompoundType::STRUCT) {
                for (TypeIndex field : compound.fields())
                    reprs.push(this->repr(field));
            } else if (compound.typeKind == CompoundType::UNION) {
                Repr r = this->repr(type);
                u32 size = r.size();
                if (size > 64)
                    reprs.push(r);
                else {
                    u32 align = r.alignment();
                    for (u32 i = 0; i < size; i += align) {
                        switch (align) {
                            case 1: reprs.push(Repr::Scalar(Size::BITS8)); break;
                            case 2: reprs.push(Repr::Scalar(Size::BITS16)); break;
                            case 4: reprs.push(Repr::Scalar(Size::BITS32)); break;
                            case 8: reprs.push(Repr::Scalar(Size::BITS64)); break;
                            default:
                                unreachable("Unexpected alignment for union type: ", align);
                        }
                    }
                }
            } else if (compound.typeKind == CompoundType::VECTOR)
                unreachable("TODO: Handle passing vector types.");
            else
                unreachable("Unexpected compound type kind in parameter placement.");
            return Target::place_aggregate_parameter(state, reprs);
        } else
            return Target::place_scalar_parameter(state, this->repr(type));
    }

    template<typename Target>
    MaybePair<ASMVal> TargetSpecificPasses<Target>::placeReturnValue(Function& fn, TypeIndex type, void* state) const {
        assert(type != VOID);
        if (isFunction(fn.typeContext(), type))
            type = PTR; // Functions are passed as pointers.
        if (isCompound(type)) {
            vec<Repr> reprs;
            const auto& compound = fn.typeContext()[type];
            if (compound.typeKind == CompoundType::ARRAY) {
                Repr repr = this->repr(compound.elementType());
                for (u32 i = 0; i < compound.length(); i ++)
                    reprs.push(repr);
            } else if (compound.typeKind == CompoundType::STRUCT) {
                for (TypeIndex field : compound.fields())
                    reprs.push(this->repr(field));
            } else if (compound.typeKind == CompoundType::UNION) {
                Repr r = this->repr(type);
                u32 size = r.size();
                if (size > 64)
                    reprs.push(r);
                else {
                    u32 align = r.alignment();
                    for (u32 i = 0; i < size; i += align) {
                        switch (align) {
                            case 1: reprs.push(Repr::Scalar(Size::BITS8)); break;
                            case 2: reprs.push(Repr::Scalar(Size::BITS16)); break;
                            case 4: reprs.push(Repr::Scalar(Size::BITS32)); break;
                            case 8: reprs.push(Repr::Scalar(Size::BITS64)); break;
                            default:
                                unreachable("Unexpected alignment for union type: ", align);
                        }
                    }
                }
            } else if (compound.typeKind == CompoundType::VECTOR)
                unreachable("TODO: Handle passing vector types.");
            else
                unreachable("Unexpected compound type kind in parameter placement.");
            return Target::place_aggregate_return_value(state, reprs);
        } else {
            Repr repr = this->repr(type);
            return Target::place_scalar_return_value(state, this->repr(type));
        }
    }

    template<typename Target>
    Repr TargetSpecificPasses<Target>::repr(TypeIndex type) const {
        if (type < 0) {
            static const Repr reprs[16] = {
                Repr::Scalar(Size::BITS8), // I8
                Repr::Scalar(Size::BITS16), // I16
                Repr::Scalar(Size::BITS32), // I32
                Repr::Scalar(Size::BITS64), // I64
                Repr::Scalar(Size::BITS8), // U8
                Repr::Scalar(Size::BITS16), // U16
                Repr::Scalar(Size::BITS32), // U32
                Repr::Scalar(Size::BITS64), // U64
                Repr::Scalar(Target::ptr_size()), // PTR
                Repr::Scalar(Target::ptr_size()), // REF
                Repr::Scalar(Size::FLOAT32), // F32
                Repr::Scalar(Size::FLOAT64), // F64
                Repr::Scalar(Target::ptr_size()), // VOID
                Repr::Scalar(Target::ptr_size()), // UNDEFINED
                Repr::Scalar(Size::BITS8), // BOOL
                Repr::Scalar(Target::ptr_size()), // INVALID
            };
            assert(~type < 16);
            return reprs[~type];
        }

        assert(type >= 0 && type < reprs.size());
        return reprs[type];
    }

    template<typename Target>
    struct MemoryMover {
        TargetSpecificPassInterface* passes;
        Block block;
        Repr repr;
        Operand dstOperand, srcOperand;
        u32 dstOffset, srcOffset;
        u32 dstAlign, srcAlign;
        bool hasDst, hasSrc;

        inline MemoryMover(TargetSpecificPassInterface* passes_in, Block block_in, Repr repr_in):
            passes(passes_in), block(block_in), repr(repr_in), dstOffset(0), srcOffset(0),
            dstAlign(0), srcAlign(0), hasDst(false), hasSrc(false) {}

        inline MemoryMover& dst(Operand operand) {
            if (operand.kind == Operand::Memory && (operand.base == Target::fp || operand.base == Target::sp))
                dstAlign = 1 << ctz32(operand.offset);
            dstOperand = operand;
            hasDst = true;
            return *this;
        }

        inline MemoryMover& src(Operand operand) {
            if (operand.kind == Operand::Memory && (operand.base == Target::fp || operand.base == Target::sp))
                srcAlign = 1u << ctz32(operand.offset);
            srcOperand = operand;
            hasSrc = true;
            return *this;
        }

        inline void moveWithOffset(TypeIndex type, Operand dst, Operand src, Operand scratch, u32 dstOffset, u32 srcOffset) {
            assert(src.kind == Operand::Memory && dst.kind == Operand::Memory);
            block.addNode(Opcode::LOAD, type, scratch, block.function->memory(src.base, src.offset + srcOffset));
            block.addNode(Opcode::STORE, type, block.function->memory(dst.base, dst.offset + dstOffset), scratch);
        }

        inline Block generate(Operand scratch) {
            assert(hasDst && hasSrc);
            assert(scratch.isReg());
            assert(scratch.gp != dstOperand.base);
            assert(scratch.gp != srcOperand.base);
            u32 size = repr.size();
            u32 i = 0;
            u32 align = max(repr.alignment(), min(dstAlign, srcAlign));
            if (align >= 8) for (; i < size / 8; i ++)
                moveWithOffset(I64, dstOperand, srcOperand, scratch, dstOffset + i * 8, srcOffset + i * 8);
            i *= 2;
            if (align >= 4) for (; i < size / 4; i ++)
                moveWithOffset(I32, dstOperand, srcOperand, scratch, dstOffset + i * 4, srcOffset + i * 4);
            i *= 2;
            if (align >= 2) for (; i < size / 2; i ++)
                moveWithOffset(I16, dstOperand, srcOperand, scratch, dstOffset + i * 2, srcOffset + i * 2);
            i *= 2;
            for (; i < size; i ++)
                moveWithOffset(I8, dstOperand, srcOperand, scratch, dstOffset + i, srcOffset + i);
            return block;
        }
    };

    template<typename Target>
    void TargetSpecificPasses<Target>::lowerTypes(Module* module) {
        TypeContext& typeCtx = module->typeContext();
        for (u32 i = reprs.size(); i < typeCtx.types.size(); i ++) {
            const CompoundType& compound = typeCtx[i];
            Repr repr;
            switch (compound.kind()) {
                case CompoundType::VECTOR:
                    repr = Repr::Vector(this->repr(compound.elementType()).scalar.size, compound.length());
                    break;
                case CompoundType::FUNCTION:
                    repr = Repr::Scalar(Target::ptr_size());
                    break;
                case CompoundType::ARRAY:
                    repr = Repr::Memory(this->repr(compound.elementType()).alignment(), this->repr(compound.elementType()).size() * compound.length());
                    break;
                case CompoundType::UNION: {
                    u8 align = 1;
                    u32 size = 0;
                    for (auto t : compound.cases()) {
                        Repr r = this->repr(t);
                        align = max<u8>(r.alignment(), align);
                        size = max<u32>(r.size(), size);
                    }
                    while (size % align) ++ size;
                    repr = Repr::Memory(align, size);
                    break;
                }
                case CompoundType::STRUCT: {
                    u8 align = 1;
                    u32 size = 0;
                    structFieldIndices.push(fieldOffsets.size()); // Not a struct, has no struct field indices.
                    for (auto t : compound.fields()) {
                        Repr r = this->repr(t);
                        align = max<u8>(r.alignment(), align);
                        while (size % r.alignment()) ++ size;
                        fieldOffsets.push(size);
                        size += r.size();
                    }
                    while (size % align) ++ size;
                    repr = Repr::Memory(align, size);
                    break;
                }
            }
            assert(i == reprs.size());
            reprs.push(repr);
            if (compound.kind() != CompoundType::STRUCT)
                structFieldIndices.push(-1); // Not a struct, has no struct field indices.
        }
    }

    template<typename T>
    inline u8 scratchAndClobberNode(PassContext& ctx, Function& fn, Node node, RegSet& clobbers);

    inline u8 defaultScratchesForNode(PassContext& ctx, Function& fn, Node node) {
        auto constCount = [&]() -> u32 {
            u32 sum = 0;
            for (Operand operand : node.uses()) {
                if (operand.isConst())
                    sum ++;
            }
            return sum;
        };
        switch (node.opcode()) {
            case Opcode::ADD:
            case Opcode::SUB:
            case Opcode::MUL:
            case Opcode::DIV:
            case Opcode::REM:
            case Opcode::AND:
            case Opcode::OR:
            case Opcode::XOR:
            case Opcode::MIN:
            case Opcode::MAX:
            case Opcode::IS_LT:
            case Opcode::IS_LE:
            case Opcode::IS_GT:
            case Opcode::IS_GE:
            case Opcode::IS_EQ:
            case Opcode::IS_NE:
            case Opcode::SHL:
            case Opcode::SHR:
            case Opcode::ROL:
            case Opcode::ROR: // Binary operators.
                return max<u8>(1, 2 - constCount());
            case Opcode::BR_LT:
            case Opcode::BR_LE:
            case Opcode::BR_GT:
            case Opcode::BR_GE:
            case Opcode::BR_EQ:
            case Opcode::BR_NE: // Branches have no output, so they can get away with none.
                return 2 - constCount();
            case Opcode::BR_INB:
            case Opcode::BR_OOB:
                return 3 - constCount();
            case Opcode::ABS:
            case Opcode::NEG:
            case Opcode::NOT:
            case Opcode::FLOOR:
            case Opcode::CEIL:
            case Opcode::ROUND:
            case Opcode::LZC:
            case Opcode::TZC:
            case Opcode::POPC:
            case Opcode::MOV: // Unary ops.
            case Opcode::CONVERT:
            case Opcode::BITCAST:
            case Opcode::ALLOCA:
                return 1;
            case Opcode::BR_IF:
            case Opcode::BR_IF_NOT:
                return 1 - constCount();
            case Opcode::ADDR:
            case Opcode::ADDR_FIELD:
            case Opcode::ADDR_INDEX:
            case Opcode::OFFSET_FIELD:
                return 1;
            case Opcode::OFFSET_INDEX:
                return 2;
            case Opcode::GET_FIELD:
                return isGPType(fn, node.type()) ? 1 : 2; // One to execute the memory/memory move, one to materialize the address in rare cases if we can't clobber the dest.
            case Opcode::SET_FIELD:
                return 2; // We can't clobber the dest like in GET_FIELD, so we always need 2.
            case Opcode::GET_INDEX:
            case Opcode::SET_INDEX:
                return 2; // One for the index/address, one to execute memory/memory moves.
            case Opcode::LOAD:
            case Opcode::STORE:
                return 2;
            case Opcode::LOAD_FIELD:
            case Opcode::STORE_FIELD:
                return 2; // One for the pointer, one for memory/memory moves.
            case Opcode::LOAD_INDEX:
            case Opcode::STORE_INDEX:
                return 3; // One for the index, one for the pointer, one for memory/memory moves.
            case Opcode::CALL:
            case Opcode::CALL_VOID:
                return 0; // We dynamically select a scratch during lowering based on the arguments.
            case Opcode::RET:
                return 1; // If we need to do a memory/memory return.
            case Opcode::NOP:
            case Opcode::BR:
            case Opcode::PUSH:
            case Opcode::POP:
            case Opcode::VAR:
            case Opcode::TRAP:
                return 0;
            default:
                unreachable("Unspecified clobbers for node with opcode ", OPCODE_NAMES[(u32)node.opcode()]);
        }
    }

    template<>
    inline u8 scratchAndClobberNode<AMD64Assembler>(PassContext& ctx, Function& fn, Node node, RegSet& clobbers) {
        u8 scratches = defaultScratchesForNode(ctx, fn, node);
        switch (node.opcode()) {
            case Opcode::DIV:
            case Opcode::REM:
                if (isInt(node.type())) {
                    clobbers.add(AMD64Assembler::RAX);
                    clobbers.add(AMD64Assembler::RDX);
                    if (node.type() != U8 && node.type() != I8)
                        clobbers.add(AMD64Assembler::RCX);
                } else if (isFloat(node.type()))
                    clobbers.add(AMD64Assembler::XMM0);
                break;
            case Opcode::SHL:
            case Opcode::SHR:
            case Opcode::ROL:
            case Opcode::ROR:
                clobbers.add(AMD64Assembler::RCX);
                break;
            default:
                break;
        }
        return scratches;
    }

    template<>
    inline u8 scratchAndClobberNode<AMD64LinuxAssembler>(PassContext& ctx, Function& fn, Node node, RegSet& clobbers) {
        return scratchAndClobberNode<AMD64Assembler>(ctx, fn, node, clobbers);
    }

    template<typename T>
    inline u8 scratchAndClobberNode(PassContext& ctx, Function& fn, Node node, RegSet& clobbers) {
        return scratchAndClobberNode<typename T::Underlying>(ctx, fn, node, clobbers);
    }

    inline Operand operandFromPlacement(Function& fn, MaybePair<ASMVal> placement) {
        if (placement.isPair()) {
            mreg first = placement.first.kind == ASMVal::GP ? placement.first.gp : placement.first.fp;
            mreg second = placement.second.kind == ASMVal::GP ? placement.second.gp : placement.second.fp;
            return fn.regPair(first, second);
        } else {
            ASMVal singular = placement.first;
            if (singular.kind == ASMVal::GP) return fn.gp(singular.gp);
            else if (singular.kind == ASMVal::FP) return fn.fp(singular.fp);
            else if (singular.kind == ASMVal::MEM) {
                assert(singular.memkind == ASMVal::REG_OFFSET);
                return fn.memory(singular.base, singular.offset);
            }
        }
        unreachable("Can't convert placement to operand");
    }

    template<typename Target>
    inline void checkForCalls(PassContext& ctx, TargetSpecificPasses<Target>& targetCtx, Function& fn, Node node, PassContext::CallList& calls, PassContext::CallSites& sites) {
        if UNLIKELY(isCall(node.opcode())) {
            assert(calls.size() == node.index());
            calls.push(sites.size());
            sites.push(CallSite());
            sites.back().node = node.index();
            auto& compound = fn.typeContext()[node.type()];

            void* state = Target::start_placing_parameters();
            if (node.opcode() == Opcode::CALL) {
                auto placement = targetCtx.placeReturnValue(fn, compound.returnType(), state);
                sites.back().returnValue = operandFromPlacement(fn, placement);
            } else
                sites.back().returnValue = fn.invalid();

            for (auto [i, t] : enumerate(compound.arguments())) {
                auto placement = targetCtx.placeParameter(fn, t, state);
                Operand result = operandFromPlacement(fn, placement);
                sites.back().parameters.push(result);
            }
            Target::finish_placing_parameters(state);
        } else
            calls.push(-1);
    }

    template<typename Target>
    void TargetSpecificPasses<Target>::computeClobbers(PassContext& ctx, Function& fn) {
        ctx.did(CLOBBERS);
        auto& callList = *ctx.callInsns;
        auto& callSites = *ctx.callSites;
        auto& clobberList = *ctx.clobberList;
        auto& scratchesPerNode = *ctx.scratchesPerNode;
        for (u32 i = 0; i < fn.nodeList.size(); i ++) {
            checkForCalls(ctx, *this, fn, fn.node(i), callList, callSites);
            RegSet clobbers;
            u8 scratchesNeeded = scratchAndClobberNode<Target>(ctx, fn, fn.node(i), clobbers);
            clobberList.push(clobbers);
            scratchesPerNode.push(scratchesNeeded);
        }
    }

    inline bool isTrivialEdge(Edge edge) {
        return edge.moves().size() == 0;
    };

    inline void breakCriticalEdges(Function& fn, vec<BlockIndex>& oldToNewMapping, vec<BlockIndex>& newToFinalMapping, vec<i32>& moveBlocks) {
        for (BlockIndex b : indices(fn.blockList))
            moveBlocks.push(-1), newToFinalMapping.push(-1), oldToNewMapping.push(-1);

        u32 initialBlockCount = fn.blockList.size();
        i32 finalIndex = 0;
        for (BlockIndex b : indices(fn.blockList)) {
            Block block = fn.addBlock();
            oldToNewMapping[b] = block.index();
            newToFinalMapping.push(finalIndex ++);
            for (const auto& e : fn.block(b).predecessors())
                block.addPredecessor(e);

            assert(fn.block(b).successorIndices().size() <= 2); // We could fix this eventually, but it's true for now.
            int nonTrivialEdges = 0;
            for (const auto& e : fn.block(b).successors()) {
                block.addSuccessor(e);
                if (!isTrivialEdge(e))
                    nonTrivialEdges ++;
                if (nonTrivialEdges > 1) {
                    moveBlocks[b] = fn.addBlock().index();
                    newToFinalMapping.push(finalIndex ++);
                }
            }
        }

        for (Edge e : fn.edges()) {
            e.header().src = oldToNewMapping[e.header().src];
            e.header().dest = oldToNewMapping[e.header().dest];
        }
    }

    inline void removeOldBlocks(Function& fn, const vec<BlockIndex>& newToFinalMapping) {
        i32 finalBlockCount = 0;
        for (auto [i, b] : enumerate(newToFinalMapping)) {
            if (b == -1) continue;
            fn.blockList[b] = fn.blockList[i];
            finalBlockCount ++;
        }
        for (auto e : fn.edges()) {
            i32 fin = newToFinalMapping[e.srcIndex()];
            if (fin == -1) continue;
            e.header().src = newToFinalMapping[e.header().src];
            e.header().dest = newToFinalMapping[e.header().dest];
            e.clearMoves();
        }
        while (fn.blockList.size() > finalBlockCount)
            fn.blockList.pop();
    }
}

#endif