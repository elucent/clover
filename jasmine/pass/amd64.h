#ifndef JASMINE_PASS_AMD64_H
#define JASMINE_PASS_AMD64_H

#include "jasmine/pass.h"

namespace jasmine::amd64 {

    extern RegSet
        NULLARY_OP[1],
        UNARY_INT_OP[1], BINARY_INT_OP[2], TERNARY_INT_OP[3], DIV_INT_OP[3], SHIFT_INT_OP[3],
        INT_BRANCH_OP[4], INT_ISBOUNDS_OP[4], INT_BRBOUNDS_OP[5],
        UNARY_FLOAT_OP[1], BINARY_FLOAT_OP[2], TERNARY_FLOAT_OP[3],
        FLOAT_COMPARE_OP[3], FLOAT_BRANCH_OP[4], FLOAT_ISBOUNDS_OP[4], FLOAT_BRBOUNDS_OP[5],
        BOOL_BRANCH_OP[3],
        CONVERT_INT_OP[3], CONVERT_FLOAT_OP[3], CONVERT_INT_TO_FLOAT_OP[3], CONVERT_FLOAT_TO_INT_OP[3],
        LOAD_FLOAT_OP[2], STORE_FLOAT_OP[2],
        INT_LOAD_FIELD_OP[3], INT_STORE_FIELD_OP[3],
        INT_LOAD_INDEX_OP[4], INT_STORE_INDEX_OP[4],
        INT_GET_FIELD_OP[3], INT_SET_FIELD_OP[3],
        INT_GET_INDEX_OP[4], INT_SET_INDEX_OP[4],
        FLOAT_LOAD_FIELD_OP[3], FLOAT_STORE_FIELD_OP[3],
        FLOAT_LOAD_INDEX_OP[4], FLOAT_STORE_INDEX_OP[4],
        FLOAT_GET_FIELD_OP[3], FLOAT_SET_FIELD_OP[3],
        FLOAT_GET_INDEX_OP[4], FLOAT_SET_INDEX_OP[4];

    inline RegSet* validRegsFor(Node node) {
        Function& fn = *node.function;
        switch (node.opcode()) {
            case Opcode::ADD:
            case Opcode::SUB:
            case Opcode::MUL:
                if (isFloat(node.type()))
                    return TERNARY_FLOAT_OP;
                return TERNARY_INT_OP;
            case Opcode::DIV:
            case Opcode::REM:
                return DIV_INT_OP;
            case Opcode::MIN:
            case Opcode::MAX:
                return TERNARY_FLOAT_OP;
            case Opcode::IS_LT:
            case Opcode::IS_LE:
            case Opcode::IS_GT:
            case Opcode::IS_GE:
            case Opcode::IS_EQ:
            case Opcode::IS_NE:
                if (isFloat(node.type()))
                    return FLOAT_COMPARE_OP;
                return TERNARY_INT_OP;
            case Opcode::IS_INB:
            case Opcode::IS_OOB:
                if (isFloat(node.type()))
                    return FLOAT_ISBOUNDS_OP;
                return INT_ISBOUNDS_OP;
            case Opcode::AND:
            case Opcode::OR:
            case Opcode::XOR:
                return TERNARY_INT_OP;
            case Opcode::SHL:
            case Opcode::SHR:
            case Opcode::ROL:
            case Opcode::ROR: // Binary operators.
                return SHIFT_INT_OP;
            case Opcode::BR_LT:
            case Opcode::BR_LE:
            case Opcode::BR_GT:
            case Opcode::BR_GE:
            case Opcode::BR_EQ:
            case Opcode::BR_NE: // Branches have no output, so they can get away with none.
                if (isFloat(node.type()))
                    return FLOAT_BRANCH_OP;
                return INT_BRANCH_OP;
            case Opcode::BR_INB:
            case Opcode::BR_OOB:
            case Opcode::BR_ADD_O:
            case Opcode::BR_SUB_O:
            case Opcode::BR_MUL_O:
                if (isFloat(node.type()))
                    return FLOAT_BRBOUNDS_OP;
                return INT_BRBOUNDS_OP;
            case Opcode::NEG:
            case Opcode::MOV:
                if (isFloat(node.type()))
                    return BINARY_FLOAT_OP;
                return BINARY_INT_OP;
            case Opcode::ABS:
            case Opcode::FLOOR:
            case Opcode::CEIL:
            case Opcode::ROUND:
            case Opcode::SQRT:
                return BINARY_FLOAT_OP;
            case Opcode::NOT:
            case Opcode::LZCNT:
            case Opcode::TZCNT:
            case Opcode::POPCNT:
            case Opcode::ALLOCA:
                return BINARY_INT_OP;
            case Opcode::BR_IF:
            case Opcode::BR_IF_NOT:
                return BOOL_BRANCH_OP;
            case Opcode::CONVERT:
            case Opcode::BITCAST: {
                bool srcFloat = isFloat(node.operand(1).type), dstFloat = isFloat(node.type());
                if (dstFloat && srcFloat)
                    return CONVERT_FLOAT_OP;
                else if (dstFloat)
                    return CONVERT_INT_TO_FLOAT_OP;
                else if (srcFloat)
                    return CONVERT_FLOAT_TO_INT_OP;
                else
                    return CONVERT_INT_OP;
            }
            case Opcode::ADDR:
                return BINARY_INT_OP;
            case Opcode::ADDR_FIELD:
            case Opcode::OFFSET_FIELD:
                return INT_LOAD_FIELD_OP;
            case Opcode::ADDR_INDEX:
            case Opcode::OFFSET_INDEX:
                return INT_LOAD_INDEX_OP;
            case Opcode::LOAD:
                if (isFloat(node.type()))
                    return LOAD_FLOAT_OP;
                return BINARY_INT_OP;
            case Opcode::STORE:
                if (isFloat(node.type()))
                    return STORE_FLOAT_OP;
                return BINARY_INT_OP;
            case Opcode::GET_FIELD: {
                TypeIndex fieldType = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(node.opcode() == Opcode::SET_FIELD || node.opcode() == Opcode::STORE_FIELD ? 1 : 2))];
                if (isFloat(fieldType))
                    return FLOAT_GET_FIELD_OP;
                return INT_GET_FIELD_OP;
            }
            case Opcode::SET_FIELD: {
                TypeIndex fieldType = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(node.opcode() == Opcode::SET_FIELD || node.opcode() == Opcode::STORE_FIELD ? 1 : 2))];
                if (isFloat(fieldType))
                    return FLOAT_SET_FIELD_OP;
                return INT_SET_FIELD_OP;
            }
            case Opcode::LOAD_FIELD: {
                TypeIndex fieldType = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(node.opcode() == Opcode::SET_FIELD || node.opcode() == Opcode::STORE_FIELD ? 1 : 2))];
                if (isFloat(fieldType))
                    return FLOAT_LOAD_FIELD_OP;
                return INT_LOAD_FIELD_OP;
            }
            case Opcode::STORE_FIELD: {
                TypeIndex fieldType = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(node.opcode() == Opcode::SET_FIELD || node.opcode() == Opcode::STORE_FIELD ? 1 : 2))];
                if (isFloat(fieldType))
                    return FLOAT_STORE_FIELD_OP;
                return INT_STORE_FIELD_OP;
            }
            case Opcode::GET_INDEX:
                if (isFloat(node.type()))
                    return FLOAT_GET_INDEX_OP;
                return INT_GET_INDEX_OP;
            case Opcode::SET_INDEX:
                if (isFloat(node.type()))
                    return FLOAT_SET_INDEX_OP;
                return INT_SET_INDEX_OP;
            case Opcode::LOAD_INDEX:
                if (isFloat(node.type()))
                    return FLOAT_LOAD_INDEX_OP;
                return INT_LOAD_INDEX_OP;
            case Opcode::STORE_INDEX:
                    return FLOAT_STORE_INDEX_OP;
                return INT_STORE_INDEX_OP;
            case Opcode::CALL:
            case Opcode::CALL_VOID:
                return NULLARY_OP; // We dynamically figure this out based on ABI.
            case Opcode::RET:
                return NULLARY_OP; // Ditto.
            case Opcode::NOP:
            case Opcode::BR:
            case Opcode::PUSH:
            case Opcode::POP:
            case Opcode::VAR:
            case Opcode::TRAP:
            case Opcode::COMMENT:
                return NULLARY_OP;
        }
    }

    inline RegSet validRegsFor(Node node, u32 operand) {
        return validRegsFor(node)[operand];
    }

    inline void lower(PassContext& ctx, Function& fn, Block& block, u32 indexInBlock, Node& node) {
        // We want to enforce a number of properties:
        //
        // 1. Non-integer immediates, and immediates wider than 32 bits, should
        //    be moved out to separate mov ops.
        //
        // 2. Index instructions may only multiply by integer and float types
        //    between 1 and 8 bytes. Otherwise we need to explicitly multiply.
        //
        // 3. Label references outside of direct load/store must be separately
        //    materialized.
        //
        // 4. Address addends that exceed 22 bits (the max encodable in an
        //    Operand) must be computed via new add nodes.
        //
        // 5. All memory moves must be register-size, or lowered into multiple
        //    loads/stores.

        bool madeEarlyInsertion = false, madeLateInsertion = false;
        auto addToInsertion = [&](Node node, Function::InsertionPoint point = Function::Early) {
            if (!(point == Function::Early ? madeEarlyInsertion : madeLateInsertion)) {
                fn.addInsertion(block, indexInBlock, point);
                (point == Function::Early ? madeEarlyInsertion : madeLateInsertion) = true;
            }
            fn.addNodeToInsertion(node);
        };

        constexpr u64 AllowImm64Mask = 1ull << (u32)Opcode::MOV;
        constexpr u64 HasComplexTypeMask = 1ull << (u32)Opcode::LOAD_FIELD
            | 1ull << (u32)Opcode::STORE_FIELD
            | 1ull << (u32)Opcode::GET_FIELD
            | 1ull << (u32)Opcode::SET_FIELD
            | 1ull << (u32)Opcode::OFFSET_FIELD;

        auto fixImmediates = [&]() {
            for (u32 i : indices(node.operands())) {
                TypeIndex operandType = node.type();
                if ((1ull << (u32)node.opcode()) & HasComplexTypeMask) {
                    if (node.opcode() == Opcode::STORE_FIELD || node.opcode() == Opcode::SET_FIELD)
                        operandType = fn.typeContext()[operandType].fields()[fn.intValueOf(node.operand(1))];
                    else
                        operandType = fn.typeContext()[operandType].fields()[fn.intValueOf(node.operand(2))];
                }
                Operand operand = node.operand(i);
                if (operand.kind == Operand::IntConst && !(AllowImm64Mask & 1ull << (u32)node.opcode()) && !fitsSigned<32>(fn.intValueOf(operand))) {
                    Operand dst = fn.variableWithType(operandType);
                    addToInsertion(fn.addNode(Opcode::MOV, operandType, dst, operand));
                    node.operands()[i] = dst;
                }
                if ((operand.kind == Operand::F32Const || operand.kind == Operand::F64Const) && node.opcode() != Opcode::MOV) {
                    Operand dst = fn.variableWithType(operandType);
                    addToInsertion(fn.addNode(Opcode::MOV, operandType, dst, operand));
                    node.operands()[i] = dst;
                }
            }
        };

        auto fixLabel = [&](u32 i) {
            if (node.operand(i).isLabel()) {
                Operand dst = fn.variableWithType(PTR);
                addToInsertion(fn.addNode(Opcode::MOV, PTR, dst, node.operand(i)));
                node.operands()[i] = dst;
            }
        };

        auto canMoveWithRegister = [&](TypeIndex type) -> maybe<TypeIndex> {
            if (type < 0) {
                assert(type != VOID && type != UNDEFINED && type != INVALID && type != EXT);
                return some<TypeIndex>(type); // All primitive TypeKinds are either register types or invalid. We assume it's not invalid here.
            }
            auto repr = ctx.targetSpecificPasses->repr(type);
            if (repr.size() > 8 || !isPowerOfTwo(repr.size())) // Assuming 64-bit register size.
                return none<TypeIndex>();
            return some<TypeIndex>(-5 - intLog2(repr.size()));
        };

        auto addMemcpy = [&](Operand dst, Operand src, Repr repr) {
            auto size = repr.size();
            auto tmp = fn.variableWithType(U64);
            u32 numQwords = size / 8;
            for (u32 i = 0; i < numQwords; i ++) {
                addToInsertion(fn.addNode(Opcode::LOAD_INDEX, U64, tmp, src, fn.typeOperand(U64), fn.intConst(i)));
                addToInsertion(fn.addNode(Opcode::STORE_INDEX, U64, dst, fn.typeOperand(U64), fn.intConst(i), tmp));
            }
            if (size & 4) {
                addToInsertion(fn.addNode(Opcode::LOAD_INDEX, U32, tmp, src, fn.typeOperand(U64), fn.intConst(size / 4 - 1)));
                addToInsertion(fn.addNode(Opcode::LOAD_INDEX, U32, dst, fn.typeOperand(U64), fn.intConst(size / 4 - 1), tmp));
            }
            if (size & 2) {
                addToInsertion(fn.addNode(Opcode::LOAD_INDEX, U16, tmp, src, fn.typeOperand(U64), fn.intConst(size / 2 - 1)));
                addToInsertion(fn.addNode(Opcode::LOAD_INDEX, U16, dst, fn.typeOperand(U64), fn.intConst(size / 2 - 1), tmp));
            }
            if (size & 1) {
                addToInsertion(fn.addNode(Opcode::LOAD_INDEX, U8, tmp, src, fn.typeOperand(U64), fn.intConst(size - 1)));
                addToInsertion(fn.addNode(Opcode::LOAD_INDEX, U8, dst, fn.typeOperand(U64), fn.intConst(size - 1), tmp));
            }
        };

        switch (node.opcode()) {
            case Opcode::ADD:
            case Opcode::SUB:
            case Opcode::MUL:
            case Opcode::DIV:
            case Opcode::REM:
            case Opcode::MIN:
            case Opcode::MAX:
            case Opcode::IS_LT:
            case Opcode::IS_LE:
            case Opcode::IS_GT:
            case Opcode::IS_GE:
            case Opcode::IS_EQ:
            case Opcode::IS_NE:
            case Opcode::IS_INB:
            case Opcode::IS_OOB:
            case Opcode::AND:
            case Opcode::OR:
            case Opcode::XOR:
            case Opcode::SHL:
            case Opcode::SHR:
            case Opcode::ROL:
            case Opcode::ROR:
            case Opcode::NEG:
            case Opcode::ABS:
            case Opcode::FLOOR:
            case Opcode::CEIL:
            case Opcode::ROUND:
            case Opcode::SQRT:
            case Opcode::NOT:
            case Opcode::LZCNT:
            case Opcode::TZCNT:
            case Opcode::POPCNT:
            case Opcode::ALLOCA:
                fixImmediates();
                break;
            case Opcode::BR_LT:
            case Opcode::BR_LE:
            case Opcode::BR_GT:
            case Opcode::BR_GE:
            case Opcode::BR_EQ:
            case Opcode::BR_NE:
                fixImmediates();
                break;
            case Opcode::BR_INB:
            case Opcode::BR_OOB:
            case Opcode::BR_ADD_O:
            case Opcode::BR_SUB_O:
            case Opcode::BR_MUL_O:
                fixImmediates();
                break;
            case Opcode::MOV:
                if (auto t = canMoveWithRegister(node.type()))
                    node.setType(*t);
                else {
                    assert(isStruct(fn, t) || isArray(fn, t));
                    auto src = fn.variableWithType(PTR), dst = fn.variableWithType(PTR);
                    auto repr = ctx.targetSpecificPasses->repr(node.type());
                    addToInsertion(fn.addNode(Opcode::ADDR, PTR, src, node.operand(1)));
                    addToInsertion(fn.addNode(Opcode::ADDR, PTR, dst, node.operand(0)));
                    addMemcpy(dst, src, repr);
                    node.nopify();
                }
                break;
            case Opcode::BR_IF:
            case Opcode::BR_IF_NOT:
                fixImmediates();
                break;
            case Opcode::CONVERT:
            case Opcode::BITCAST:
                fixImmediates();
                break;
            case Opcode::ADDR:
            case Opcode::ADDR_FIELD:
                // TODO: Should we maybe handle the case that we overflow the range of a memory operand?
                break;
            case Opcode::OFFSET_FIELD:
                fixLabel(1);
                break;
            case Opcode::ADDR_INDEX:
                if (auto t = canMoveWithRegister(node.type()))
                    node.setType(*t);
                else {
                    auto repr = ctx.targetSpecificPasses->repr(node.type());
                    auto idx = node.operand(3);
                    auto idxType = node.operand(2).type;
                    if (idxType != U64 && idxType != I64 && idxType != PTR) {
                        idx = fn.variableWithType(U64);
                        addToInsertion(fn.addNode(Opcode::CONVERT, U64, idx, fn.typeOperand(idxType), node.operand(3)));
                    }
                    auto addend = fn.variableWithType(U64);
                    addToInsertion(fn.addNode(Opcode::MUL, U64, addend, idx, fn.intConst(roundUpToNearest(repr.size(), repr.alignment()))));
                    fn.replaceNode(node, Opcode::ADDR_INDEX, U8, node.operand(0), node.operand(1), addend);
                }
                break;
            case Opcode::OFFSET_INDEX:
                fixLabel(1);
                if (auto t = canMoveWithRegister(node.type()))
                    node.setType(*t);
                else {
                    auto repr = ctx.targetSpecificPasses->repr(node.type());
                    auto idx = node.operand(3);
                    auto idxType = node.operand(2).type;
                    if (idxType != U64 && idxType != I64 && idxType != PTR) {
                        idx = fn.variableWithType(U64);
                        addToInsertion(fn.addNode(Opcode::CONVERT, U64, idx, fn.typeOperand(idxType), node.operand(3)));
                    }
                    auto addend = fn.variableWithType(U64);
                    addToInsertion(fn.addNode(Opcode::MUL, U64, addend, idx, fn.intConst(roundUpToNearest(repr.size(), repr.alignment()))));
                    fn.replaceNode(node, Opcode::ADD, PTR, node.operand(0), node.operand(1), idx);
                }
                break;
            case Opcode::LOAD:
                if (auto t = canMoveWithRegister(node.type()))
                    node.setType(*t);
                else {
                    auto dst = fn.variableWithType(PTR);
                    addToInsertion(fn.addNode(Opcode::ADDR, PTR, dst, node.operand(0)));
                    auto src = node.operand(1);
                    if (node.operand(1).isLabel()) {
                        src = fn.variableWithType(PTR);
                        addToInsertion(fn.addNode(Opcode::MOV, PTR, src, node.operand(1)));
                    }
                    auto repr = ctx.targetSpecificPasses->repr(node.type());
                    addMemcpy(dst, src, repr);
                    node.nopify();
                }
                break;
            case Opcode::STORE:
                if (auto t = canMoveWithRegister(node.type())) {
                    fixImmediates();
                    node.setType(*t);
                } else {
                    auto src = fn.variableWithType(PTR);
                    addToInsertion(fn.addNode(Opcode::ADDR, PTR, src, node.operand(1)));
                    auto dst = node.operand(0);
                    if (node.operand(0).isLabel()) {
                        dst = fn.variableWithType(PTR);
                        addToInsertion(fn.addNode(Opcode::MOV, PTR, src, node.operand(0)));
                    }
                    auto repr = ctx.targetSpecificPasses->repr(node.type());
                    addMemcpy(dst, src, repr);
                    node.nopify();
                }
                break;
            case Opcode::GET_FIELD: {
                auto field = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(2))];
                if (auto t = canMoveWithRegister(field))
                    node.setType(*t);
                else {
                    auto dst = fn.variableWithType(PTR), src = fn.variableWithType(PTR);
                    addToInsertion(fn.addNode(Opcode::ADDR, PTR, dst, node.operand(0)));
                    addToInsertion(fn.addNode(Opcode::ADDR_FIELD, node.type(), src, node.operand(1), node.operand(2)));
                    addMemcpy(dst, src, ctx.targetSpecificPasses->repr(field));
                    node.nopify();
                }
                break;
            }
            case Opcode::SET_FIELD: {
                auto field = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(1))];
                if (auto t = canMoveWithRegister(field)) {
                    fixImmediates();
                    node.setType(*t);
                } else {
                    auto dst = fn.variableWithType(PTR), src = fn.variableWithType(PTR);
                    addToInsertion(fn.addNode(Opcode::ADDR, PTR, src, node.operand(2)));
                    addToInsertion(fn.addNode(Opcode::ADDR_FIELD, node.type(), dst, node.operand(0), node.operand(1)));
                    addMemcpy(dst, src, ctx.targetSpecificPasses->repr(field));
                    node.nopify();
                }
                break;
            }
            case Opcode::LOAD_FIELD: {
                fixLabel(1);
                auto field = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(2))];
                if (auto t = canMoveWithRegister(field))
                    node.setType(*t);
                else {
                    auto dst = fn.variableWithType(PTR);
                    addToInsertion(fn.addNode(Opcode::ADDR, PTR, dst, node.operand(0)));
                    auto src = node.operand(1);
                    if (src.isLabel()) {
                        src = fn.variableWithType(PTR);
                        addToInsertion(fn.addNode(Opcode::MOV, PTR, src, node.operand(1)));
                    }
                    addToInsertion(fn.addNode(Opcode::ADDR_FIELD, node.type(), src, src, node.operand(2)));
                    addMemcpy(dst, src, ctx.targetSpecificPasses->repr(field));
                    node.nopify();
                }
                break;
            }
            case Opcode::STORE_FIELD: {
                fixLabel(0);
                auto field = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(1))];
                if (auto t = canMoveWithRegister(field)) {
                    fixImmediates();
                    node.setType(*t);
                } else {
                    auto src = fn.variableWithType(PTR);
                    addToInsertion(fn.addNode(Opcode::ADDR, PTR, src, node.operand(2)));
                    auto dst = node.operand(0);
                    if (dst.isLabel()) {
                        dst = fn.variableWithType(PTR);
                        addToInsertion(fn.addNode(Opcode::MOV, PTR, dst, node.operand(0)));
                    }
                    addToInsertion(fn.addNode(Opcode::ADDR_FIELD, node.type(), dst, dst, node.operand(1)));
                    addMemcpy(dst, src, ctx.targetSpecificPasses->repr(field));
                    node.nopify();
                }
                break;
            }
            case Opcode::GET_INDEX:
                if (auto t = canMoveWithRegister(node.type())) {

                }
                break;
            case Opcode::SET_INDEX:
                break;
            case Opcode::LOAD_INDEX:
                break;
            case Opcode::STORE_INDEX:
                break;
            case Opcode::CALL:
            case Opcode::CALL_VOID:
            case Opcode::RET:
                fixImmediates();
                break;
            case Opcode::NOP:
            case Opcode::BR:
            case Opcode::PUSH:
            case Opcode::POP:
            case Opcode::VAR:
            case Opcode::TRAP:
            case Opcode::COMMENT:
                break;
        }
    }
}

#endif