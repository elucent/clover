#include "jasmine/pass.h"

namespace jasmine {
    struct Decomposition {
        i32 first : 28;
        u32 n : 4;
    };

    constexpr u32 MaxScalarsPerType = 4;
    constexpr bool AllowArrayDecomposition = false;

    bool tryDecompose(Function& fn, const CompoundType& compound, vec<TypeIndex, 16>& decomposition) {
        // We currently only decompose structs and arrays that only contain
        // scalar fields. We could extend this in the future to nested structs,
        // but we'd need to detect multi-level access or recursively SROA the
        // members somehow.

        if (compound.kind() == CompoundType::STRUCT) {
            for (TypeIndex t : compound.fields()) {
                if (isCompound(t) && !isFunction(fn, t))
                    return false;
                if (decomposition.size() >= MaxScalarsPerType)
                    return false;
                decomposition.push(t);
            }
        } else if (compound.kind() == CompoundType::ARRAY && AllowArrayDecomposition) {
            if (isCompound(compound.elementType()) && !isFunction(fn, compound.elementType()))
                return false;
            if (compound.length() > MaxScalarsPerType)
                return false;
            for (u32 i = 0; i < compound.length(); i ++)
                decomposition.push(compound.elementType());
        }
        return true;
    }

    void scalarReplacement(PassContext& ctx, Function& fn) {
        JASMINE_PASS(SCALAR_REPLACEMENT);
        ctx.require(IRTrait::PINS);

        auto& pins = *ctx.pins;
        vec<Operand> decompositions;
        vec<Decomposition> state;

        bool foundSROAOpportunity = false;
        for (const auto& [i, var] : enumerate(fn.variableList)) {
            if (isCompound(var.type) && !pins.isPinned(i)) {
                const auto& compound = fn.typeContext()[var.type];
                vec<TypeIndex, 16> decomposition;
                if (tryDecompose(fn, compound, decomposition)) {
                    Decomposition element;
                    element.first = decompositions.size();
                    element.n = decomposition.size();
                    for (TypeIndex t : decomposition)
                        decompositions.push(fn.variable());
                    state.push(element);
                    foundSROAOpportunity = true;
                    continue;
                }
            }

            // If we didn't successfully decompose, everything falls through to
            // here.
            state.push(Decomposition { .first = -1, .n = 0 });
        }

        if (!foundSROAOpportunity)
            return;

        for (Block b : fn.blocks()) for (const auto& [i, ni] : enumerate(b.nodeIndices())) {
            Node n = fn.node(ni);

            auto isDecomposed = [&](Operand o) -> bool {
                return o.kind == Operand::Var && state[o.var].first != -1;
            };

            switch (n.opcode()) {
                case Opcode::VAR:
                    if (isDecomposed(n.operand(0)))
                        n.nopify(); // Scalar variables that are known not to be pinned shouldn't have to be declared like this.
                    break;
                case Opcode::MOV:
                    if (isDecomposed(n.operand(0)) && isDecomposed(n.operand(1))) {
                        // Do fieldwise move.
                        const auto& compound = fn.typeContext()[n.type()];
                        auto firstLeft = state[n.operand(0).var].first, firstRight = state[n.operand(1).var].first;
                        fn.addInsertion(b, i);
                        for (u32 i = 0; i < compound.fields().size(); i ++)
                            fn.addNodeToInsertion(fn.addNode(Opcode::MOV, compound.fields()[i], decompositions[firstLeft + i], decompositions[firstRight + i]));
                        n.nopify();
                    } else if (isDecomposed(n.operand(0))) {
                        // Get fieldwise from source.
                        const auto& compound = fn.typeContext()[n.type()];
                        auto first = state[n.operand(0).var].first;
                        fn.addInsertion(b, i);
                        for (u32 i = 0; i < compound.fields().size(); i ++)
                            fn.addNodeToInsertion(fn.addNode(Opcode::GET_FIELD, n.type(), decompositions[first + i], n.operand(1), fn.intConst(i)));
                        n.nopify();
                    } else if (isDecomposed(n.operand(1))) {
                        // Write fieldwise to destination.
                        const auto& compound = fn.typeContext()[n.type()];
                        auto first = state[n.operand(1).var].first;
                        fn.addInsertion(b, i);
                        for (u32 i = 0; i < compound.fields().size(); i ++)
                            fn.addNodeToInsertion(fn.addNode(Opcode::SET_FIELD, n.type(), n.operand(0), fn.intConst(i), decompositions[first + i]));
                        n.nopify();
                    }
                    break;
                case Opcode::LOAD:
                    if (isDecomposed(n.operand(0))) {
                        // Load each field independently.
                        const auto& compound = fn.typeContext()[n.type()];
                        auto firstLeft = state[n.operand(0).var].first, firstRight = state[n.operand(1).var].first;
                        fn.addInsertion(b, i);
                        for (u32 i = 0; i < compound.fields().size(); i ++)
                            fn.addNodeToInsertion(fn.addNode(Opcode::LOAD_FIELD, n.type(), decompositions[firstLeft + i], n.operand(1), fn.intConst(i)));
                        n.nopify();
                    }
                    break;
                case Opcode::STORE:
                    if (isDecomposed(n.operand(1))) {
                        // Write fieldwise to destination.
                        const auto& compound = fn.typeContext()[n.type()];
                        auto first = state[n.operand(1).var].first;
                        fn.addInsertion(b, i);
                        for (u32 i = 0; i < compound.fields().size(); i ++)
                            fn.addNodeToInsertion(fn.addNode(Opcode::STORE_FIELD, n.type(), n.operand(0), fn.intConst(i), decompositions[first + i]));
                        n.nopify();
                    }
                    break;
                case Opcode::GET_FIELD:
                    if (n.operand(1).kind == Operand::Var && state[n.operand(1).var].first != -1) {
                        auto fieldId = fn.intValueOf(n.operand(2));
                        const auto& compound = fn.typeContext()[n.type()];
                        fn.addInsertion(b, i);
                        auto newVar = decompositions[state[n.operand(1).var].first + fieldId];
                        fn.addNodeToInsertion(fn.addNode(Opcode::MOV, compound.fields()[fieldId], n.operand(0), newVar));
                        n.nopify();
                    }
                    break;
                case Opcode::SET_FIELD:
                    if (n.operand(0).kind == Operand::Var && state[n.operand(0).var].first != -1) {
                        auto fieldId = fn.intValueOf(n.operand(1));
                        const auto& compound = fn.typeContext()[n.type()];
                        fn.addInsertion(b, i);
                        auto newVar = decompositions[state[n.operand(0).var].first + fieldId];
                        fn.addNodeToInsertion(fn.addNode(Opcode::MOV, compound.fields()[fieldId], newVar, n.operand(2)));
                        n.nopify();
                    }
                    break;
                case Opcode::CALL:
                    // This is a hard one...
                    unreachable("TODO: Implement SROA for call parameters.");
                    break;
                case Opcode::RET:
                default:
                    break;
            }
        }
        fn.executeInsertions();

        ctx.invalidate(IRTrait::SSA);
    }
}