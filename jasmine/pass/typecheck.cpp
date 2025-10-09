#include "jasmine/mod.h"
#include "jasmine/pass.h"
#include "util/config.h"

namespace jasmine {
    void assignTypesUnchecked(PassContext& ctx, Function& fn) {
        if UNLIKELY(config::printJasmineDOTBeforeOpts || config::printJasmineDOTBeforeEachPass || config::printJasmineDOTAfterOpts)
            beginDOT(ctx, fn);

        if UNLIKELY(config::printJasmineBeforeOpts)
            println("=== IR for function ", fn.name(), " before opts ===\n", fn);

        if UNLIKELY(config::printJasmineCompileStats) {
            CompileStats::functionsCompiled ++;
            CompileStats::blocksCompiled += fn.blockList.size();
            CompileStats::nodesCompiled += fn.nodeList.size();
        }

        for (const auto& p : fn.parameters) {
            if (p.operand.kind == Operand::Var)
                fn.variableList[p.operand.var].type = p.type;
        }

        auto unify = [&](Operand operand, TypeIndex type) {
            if (operand.kind == Operand::Var)
                fn.variableList[operand.var].type = type;
        };

        for (Block block : fn.blocks()) {
            for (Node node : block.nodes()) switch (node.opcode()) {
                case Opcode::NOP:
                case Opcode::COMMENT:
                    break;
                case Opcode::VAR:
                case Opcode::PACK:
                case Opcode::UNPACK:
                case Opcode::MOV:
                case Opcode::ABS:
                case Opcode::SQRT:
                case Opcode::ROUND:
                case Opcode::FLOOR:
                case Opcode::CEIL:
                case Opcode::NEG:
                case Opcode::NOT:
                case Opcode::LZCNT:
                case Opcode::TZCNT:
                case Opcode::POPCNT:
                case Opcode::MIN:
                case Opcode::MAX:
                case Opcode::AND:
                case Opcode::OR:
                case Opcode::XOR:
                case Opcode::SHL:
                case Opcode::SHR:
                case Opcode::ROL:
                case Opcode::ROR:
                case Opcode::ADD:
                case Opcode::SUB:
                case Opcode::MUL:
                case Opcode::DIV:
                case Opcode::REM:
                case Opcode::BR_ADD_O:
                case Opcode::BR_SUB_O:
                case Opcode::BR_MUL_O:
                case Opcode::LOAD:
                case Opcode::BITCAST:
                case Opcode::CONVERT:
                    unify(node.operand(0), node.type());
                    break;
                case Opcode::IS_LT:
                case Opcode::IS_LE:
                case Opcode::IS_GT:
                case Opcode::IS_GE:
                case Opcode::IS_EQ:
                case Opcode::IS_NE:
                case Opcode::IS_INB:
                case Opcode::IS_OOB:
                case Opcode::BR_IF:
                case Opcode::BR_IF_NOT:
                    unify(node.operand(0), TypeKind::BOOL);
                    break;
                case Opcode::BR:
                case Opcode::BR_LT:
                case Opcode::BR_LE:
                case Opcode::BR_GT:
                case Opcode::BR_GE:
                case Opcode::BR_EQ:
                case Opcode::BR_NE:
                case Opcode::BR_INB:
                case Opcode::BR_OOB:
                case Opcode::STORE:
                case Opcode::STORE_FIELD:
                case Opcode::STORE_INDEX:
                case Opcode::SET_FIELD:
                case Opcode::SET_INDEX:
                case Opcode::RET:
                    break;
                case Opcode::ADDR:
                case Opcode::ADDR_FIELD:
                case Opcode::ADDR_INDEX:
                case Opcode::OFFSET_FIELD:
                case Opcode::OFFSET_INDEX:
                    unify(node.operand(0), PTR);
                    break;
                case Opcode::GET_FIELD:
                case Opcode::LOAD_FIELD: {
                    auto& type = fn.typeContext()[node.type()];
                    i64 id = fn.intValueOf(node.operand(2));
                    unify(node.operand(0), type.fields()[id]);
                    break;
                }
                case Opcode::GET_INDEX:
                case Opcode::LOAD_INDEX: {
                    unify(node.operand(0), node.type());
                    break;
                }
                case Opcode::TRAP:
                    break;
                case Opcode::CALL_VOID:
                    fn.makesCalls = true;
                    break;
                case Opcode::CALL: {
                    auto& type = fn.typeContext()[node.type()];
                    unify(node.operand(0), type.returnType());
                    fn.makesCalls = true;
                    break;
                }
                case Opcode::ALLOCA:
                    fn.hasAlloca = true;
                    unify(node.operand(0), PTR);
                    break;
                case Opcode::PUSH:
                case Opcode::POP:
                    unreachable("Illegal in this phase.");
                default:
                    unreachable("Invalid opcode.");
            }
        }

        ctx.did(TYPECHECK);
    }

    void typecheck(PassContext& ctx, Function& fn) {
        if UNLIKELY(config::printJasmineDOTBeforeOpts || config::printJasmineDOTBeforeEachPass || config::printJasmineDOTAfterOpts)
            beginDOT(ctx, fn);

        JASMINE_PASS(TYPECHECKING);
        ctx.did(TYPECHECK);

        if UNLIKELY(config::printJasmineBeforeOpts)
            println("=== IR for function ", fn.name(), " before opts ===\n", fn);

        if UNLIKELY(config::printJasmineCompileStats) {
            CompileStats::functionsCompiled ++;
            CompileStats::blocksCompiled += fn.blockList.size();
            CompileStats::nodesCompiled += fn.nodeList.size();
        }

        for (const auto& p : fn.parameters) {
            if (p.operand.kind == Operand::Var)
                fn.variableList[p.operand.var].type = p.type;
        }

        auto unify = [&](Block block, Node node, Operand operand, TypeIndex type) {
            switch (operand.kind) {
                case Operand::Var: {
                    TypeIndex& currentType = fn.variableList[operand.var].type;
                    if (currentType == UNDEFINED)
                        currentType = type;
                    else if (currentType != type)
                        ctx.error(block, node, "Can't unify variable ", OperandLogger { fn, operand }, " of type ", TypeLogger { fn, currentType }, " with ", TypeLogger { fn, type }, '.');
                    break;
                }
                case Operand::Sizeof:
                    if (!isInt(type))
                        ctx.error(block, node, "Sizeof operand ", OperandLogger { fn, operand }, " cannot unify with non-integral type ", TypeLogger { fn, type }, '.');
                    break;
                case Operand::IntConst:
                    if (!isInt(type) && type != BOOL)
                        ctx.error(block, node, "Integer constant ", OperandLogger { fn, operand }, " cannot unify with non-integral type ", TypeLogger { fn, type }, '.');
                    else if (!fits(fn.intValueOf(operand), type)) {
                        if (type == BOOL)
                            ctx.error(block, node, "Integer constant used as type ", TypeLogger { fn, type }, " must be 0 or 1, but given ", OperandLogger { fn, operand }, '.');
                        else
                            ctx.error(block, node, "Integer constant ", OperandLogger { fn, operand }, " does not fit in type ", TypeLogger { fn, type }, '.');
                    }
                    break;
                case Operand::F32Const:
                    if (type != TypeKind::F32)
                        ctx.error(block, node, "32-bit float constant ", OperandLogger { fn, operand }, " must have type F32.");
                    break;
                case Operand::F64Const:
                    if (type != TypeKind::F64)
                        ctx.error(block, node, "64-bit float constant ", OperandLogger { fn, operand }, " must have type F64.");
                    break;
                case Operand::GP:
                    if (!isInt(type))
                        ctx.error(block, node, "General-purpose register operand ", OperandLogger { fn, operand }, " cannot be assigned non-integral type ", TypeLogger { fn, type }, '.');
                    break;
                case Operand::FP:
                    if (!isFloat(type))
                        ctx.error(block, node, "Floating-point register operand ", OperandLogger { fn, operand }, " cannot be assigned non-float type ", TypeLogger { fn, type }, '.');
                    break;
                case Operand::Memory:
                case Operand::Func:
                case Operand::Data:
                case Operand::Static:
                case Operand::Branch:
                    break;
                default:
                    unreachable("Invalid operand kind.");
            }
        };

        auto unifyBinary = [&](Block block, Node node) {
            unify(block, node, node.operand(0), node.type());
        };

        auto unifyTernary = [&](Block block, Node node) {
            unify(block, node, node.operand(0), node.type());
        };

        auto unifyCompare = [&](Block block, Node node) {
            unify(block, node, node.operand(0), TypeKind::BOOL);
            unify(block, node, node.operand(1), node.type());
            unify(block, node, node.operand(2), node.type());
        };

        auto checkIsInt = [&](Block block, Node node) {
            if (!isInt(node.type()) && node.type() != PTR)
                ctx.error(block, node, "Expected integral type for node with opcode ", node.opcode(), ", found non-integral type ", TypeLogger { fn, node.type() }, '.');
        };

        auto checkIsFloat = [&](Block block, Node node) {
            if (!isFloat(node.type()))
                ctx.error(block, node, "Expected floating-point type for node with opcode ", node.opcode(), ", found non-floating-point type ", TypeLogger { fn, node.type() }, '.');
        };

        auto checkIsNumeric = [&](Block block, Node node) {
            if (!isInt(node.type()) && !isFloat(node.type()) && node.type() != PTR)
                ctx.error(block, node, "Expected numeric type for node with opcode ", node.opcode(), ", found non-numeric type ", TypeLogger { fn, node.type() }, '.');
        };

        auto checkBranch = [&](Block block, Node node, Operand operand) {
            if (operand.kind != Operand::Branch)
                ctx.error(block, node, "Expected branch operand in node with opcode ", node.opcode(), ", found ", OperandLogger { *node.function, operand }, '.');
        };

        auto checkMemory = [&](Block block, Node node, Operand operand) {
            if (operand.isConst() || operand.isReg() || operand.kind == Operand::Branch)
                ctx.error(block, node, "Expected memory operand in node with opcode ", node.opcode(), ", found ", OperandLogger { *node.function, operand }, '.');
        };

        auto checkVar = [&](Block block, Node node, Operand operand) {
            if (operand.kind != Operand::Var)
                ctx.error(block, node, "Expected variable operand in node with opcode ", node.opcode(), ", found ", OperandLogger { *node.function, operand }, '.');
        };

        auto validateArity = [&](Block block, Node node, i32 arity) -> bool {
            if (node.operands().size() != arity) {
                ctx.error(block, node, "Incorrect number of operands in node with opcode ", node.opcode(), ": expected ", arity, ", got ", node.operands().size());
                return false;
            }
            return true;
        };

        auto validateArityAndDest = [&](Block block, Node node, i32 arity) -> bool {
            if (!validateArity(block, node, arity))
                return false;
            Operand operand = node.operand(0);
            if (operand.isConst()) {
                ctx.error(block, node, "Destination parameter of node with opcode ", node.opcode(), " may not be constant, but found ", OperandLogger { fn, operand }, '.');
                return false;
            }
            return true;
        };

        for (Block block : fn.blocks()) {
            for (Node node : block.nodes()) switch (node.opcode()) {
                case Opcode::NOP:
                    validateArity(block, node, 0);
                    break;
                case Opcode::COMMENT:
                    validateArity(block, node, 1);
                    break;
                case Opcode::VAR:
                    if (!validateArityAndDest(block, node, 1))
                        break;
                    unify(block, node, node.operand(0), node.type());
                    break;
                case Opcode::PACK:
                    if (!isStruct(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected struct type in pack node, found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& structType = fn.typeContext()[node.type()];
                        if (structType.fieldCount != node.operands().size() - 1) {
                            ctx.error(block, node, "Incorrect number of inputs in make.struct node: expected ", structType.fieldCount, ", got ", node.operands().size() - 1);
                            break;
                        }
                        for (auto [t, o] : zip(structType.fields(), node.operands().drop(1)))
                            unify(block, node, o, t);
                        unify(block, node, node.operand(0), node.type());
                    }
                    break;
                case Opcode::UNPACK:
                    if (!isStruct(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected struct type in unpack node, found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& structType = fn.typeContext()[node.type()];
                        if (structType.fieldCount != node.operands().size() - 1) {
                            ctx.error(block, node, "Incorrect number of outputs in unpack node: expected ", structType.fieldCount, ", got ", node.operands().size() - 1);
                            break;
                        }
                        for (auto [t, o] : zip(structType.fields(), node.operands().take(structType.fieldCount)))
                            unify(block, node, o, t);
                        unify(block, node, node.operand(structType.fieldCount), node.type());
                    }
                    break;
                case Opcode::MOV:
                    if (!validateArityAndDest(block, node, 2))
                        break;
                    unifyBinary(block, node);
                    break;
                case Opcode::NEG:
                    if (!validateArityAndDest(block, node, 2))
                        break;
                    checkIsNumeric(block, node);
                    if (isUnsigned(node.type()))
                        ctx.error(block, node, "Expected signed integer or float type in node with opcode ", node.opcode(), ", found unsigned type ", TypeLogger { fn, node.type() }, '.');
                    unifyBinary(block, node);
                    break;
                case Opcode::ABS:
                case Opcode::SQRT:
                case Opcode::ROUND:
                case Opcode::FLOOR:
                case Opcode::CEIL:
                    if (!validateArityAndDest(block, node, 2))
                        break;
                    checkIsFloat(block, node);
                    unifyBinary(block, node);
                    break;
                case Opcode::NOT:
                case Opcode::LZCNT:
                case Opcode::TZCNT:
                case Opcode::POPCNT:
                    if (!validateArityAndDest(block, node, 2))
                        break;
                    checkIsInt(block, node);
                    unifyBinary(block, node);
                    break;
                case Opcode::MIN:
                case Opcode::MAX:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    checkIsFloat(block, node);
                    unifyTernary(block, node);
                    break;
                case Opcode::AND:
                case Opcode::OR:
                case Opcode::XOR:
                case Opcode::SHL:
                case Opcode::SHR:
                case Opcode::ROL:
                case Opcode::ROR:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    checkIsInt(block, node);
                    unifyTernary(block, node);
                    break;
                case Opcode::ADD:
                case Opcode::SUB:
                case Opcode::MUL:
                case Opcode::DIV:
                case Opcode::REM:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    checkIsNumeric(block, node);
                    unifyTernary(block, node);
                    break;
                case Opcode::IS_LT:
                case Opcode::IS_LE:
                case Opcode::IS_GT:
                case Opcode::IS_GE:
                case Opcode::IS_EQ:
                case Opcode::IS_NE:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    checkIsNumeric(block, node);
                    unifyCompare(block, node);
                    break;
                case Opcode::IS_INB:
                case Opcode::IS_OOB:
                    if (!validateArityAndDest(block, node, 4))
                        break;
                    checkIsNumeric(block, node);
                    unify(block, node, node.operand(0), TypeKind::BOOL);
                    unify(block, node, node.operand(1), node.type());
                    unify(block, node, node.operand(2), node.type());
                    unify(block, node, node.operand(3), node.type());
                    break;
                case Opcode::BR:
                    if (!validateArity(block, node, 1))
                        break;
                    if (node.operand(0).kind != Operand::Branch)
                        ctx.error(block, node, "Expected singular branch operand in unconditional branch, found ", node.operands().size(), '.');
                    break;
                case Opcode::BR_IF:
                case Opcode::BR_IF_NOT:
                    if (!validateArity(block, node, 3))
                        break;
                    if (!isInt(node.type()) && node.type() != TypeKind::BOOL)
                        ctx.error(block, node, "Expected integral or boolean type for node with opcode ", node.opcode(), ", found type ", TypeLogger { fn, node.type() }, '.');
                    checkBranch(block, node, node.operand(1));
                    checkBranch(block, node, node.operand(2));
                    unify(block, node, node.operand(0), node.type());
                    break;
                case Opcode::BR_LT:
                case Opcode::BR_LE:
                case Opcode::BR_GT:
                case Opcode::BR_GE:
                case Opcode::BR_EQ:
                case Opcode::BR_NE:
                    if (!validateArity(block, node, 4))
                        break;
                    checkIsNumeric(block, node);
                    checkBranch(block, node, node.operand(2));
                    checkBranch(block, node, node.operand(3));
                    unifyBinary(block, node);
                    break;
                case Opcode::BR_INB:
                case Opcode::BR_OOB:
                    if (!validateArity(block, node, 5))
                        break;
                    checkIsNumeric(block, node);
                    checkBranch(block, node, node.operand(3));
                    checkBranch(block, node, node.operand(4));
                    unify(block, node, node.operand(0), node.type());
                    unify(block, node, node.operand(1), node.type());
                    unify(block, node, node.operand(2), node.type());
                    break;
                case Opcode::BR_ADD_O:
                case Opcode::BR_SUB_O:
                case Opcode::BR_MUL_O:
                    if (!validateArityAndDest(block, node, 5))
                        break;
                    checkIsNumeric(block, node);
                    checkBranch(block, node, node.operand(3));
                    checkBranch(block, node, node.operand(4));
                    unifyTernary(block, node);
                    break;
                case Opcode::LOAD:
                    if (!validateArityAndDest(block, node, 2))
                        break;
                    checkMemory(block, node, node.operand(1));
                    unify(block, node, node.operand(0), node.type());
                    unify(block, node, node.operand(1), PTR);
                    break;
                case Opcode::ADDR:
                    if (!validateArityAndDest(block, node, 2))
                        break;
                    checkVar(block, node, node.operand(1));
                    unify(block, node, node.operand(0), PTR);
                    break;
                case Opcode::STORE:
                    if (!validateArityAndDest(block, node, 2))
                        break;
                    checkMemory(block, node, node.operand(0));
                    unify(block, node, node.operand(0), PTR);
                    unify(block, node, node.operand(1), node.type());
                    break;
                case Opcode::GET_FIELD:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    if (node.operand(2).kind != Operand::IntConst)
                        ctx.error(block, node, "Expected constant field index in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    if (!isStruct(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected struct type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& type = fn.typeContext()[node.type()];
                        i64 id = fn.intValueOf(node.operand(2));
                        unify(block, node, node.operand(1), node.type());
                        if (id >= 0 && id < type.fieldCount)
                            unify(block, node, node.operand(0), type.fields()[id]);
                        else
                            ctx.error(block, node, "Field index out of bounds: tried to access field ", id, " of type with ", type.fieldCount, " fields.");
                    }
                    break;
                case Opcode::SET_FIELD:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    if (node.operand(1).kind != Operand::IntConst)
                        ctx.error(block, node, "Expected constant field index in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(1) }, '.');
                    if (!isStruct(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected struct type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& type = fn.typeContext()[node.type()];
                        i64 id = fn.intValueOf(node.operand(1));
                        unify(block, node, node.operand(0), node.type());
                        if (id >= 0 && id < type.fieldCount)
                            unify(block, node, node.operand(2), type.fields()[id]);
                        else
                            ctx.error(block, node, "Field index out of bounds: tried to access field ", id, " of type with ", type.fieldCount, " fields.");
                    }
                    break;
                case Opcode::LOAD_FIELD:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    if (node.operand(2).kind != Operand::IntConst)
                        ctx.error(block, node, "Expected constant field index in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    if (!isStruct(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected struct type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& type = fn.typeContext()[node.type()];
                        i64 id = fn.intValueOf(node.operand(2));
                        if (id >= 0 && id < type.fieldCount)
                            unify(block, node, node.operand(0), type.fields()[id]);
                        else
                            ctx.error(block, node, "Field index out of bounds: tried to access field ", id, " of type with ", type.fieldCount, " fields.");
                        unify(block, node, node.operand(1), PTR);
                    }
                    break;
                case Opcode::ADDR_FIELD:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    if (node.operand(2).kind != Operand::IntConst)
                        ctx.error(block, node, "Expected constant field index in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    if (!isStruct(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected struct type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& type = fn.typeContext()[node.type()];
                        i64 id = fn.intValueOf(node.operand(2));
                        if (id < 0 || id >= type.fieldCount)
                            ctx.error(block, node, "Field index out of bounds: tried to access field ", id, " of type with ", type.fieldCount, " fields.");
                        unify(block, node, node.operand(1), node.type());
                        unify(block, node, node.operand(0), PTR);
                    }
                    break;
                case Opcode::OFFSET_FIELD:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    if (node.operand(2).kind != Operand::IntConst)
                        ctx.error(block, node, "Expected constant field index in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    if (!isStruct(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected struct type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& type = fn.typeContext()[node.type()];
                        i64 id = fn.intValueOf(node.operand(2));
                        if (id < 0 || id >= type.fieldCount)
                            ctx.error(block, node, "Field index out of bounds: tried to access field ", id, " of type with ", type.fieldCount, " fields.");
                        unify(block, node, node.operand(1), PTR);
                        unify(block, node, node.operand(0), PTR);
                    }
                    break;
                case Opcode::STORE_FIELD:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    if (node.operand(1).kind != Operand::IntConst)
                        ctx.error(block, node, "Expected constant field index in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(1) }, '.');
                    if (!isStruct(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected struct type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& type = fn.typeContext()[node.type()];
                        i64 id = fn.intValueOf(node.operand(1));
                        unify(block, node, node.operand(0), PTR);
                        if (id >= 0 && id < type.fieldCount)
                            unify(block, node, node.operand(2), type.fields()[id]);
                        else
                            ctx.error(block, node, "Field index out of bounds: tried to access field ", id, " of type with ", type.fieldCount, " fields.");
                    }
                    break;
                case Opcode::GET_INDEX:
                    if (!validateArityAndDest(block, node, 4))
                        break;
                    unify(block, node, node.operand(0), node.type());
                    if (node.operand(2).kind != Operand::Type)
                        ctx.error(block, node, "Expected index type operand in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    else if (!isInt(node.operand(2).type))
                        ctx.error(block, node, "Expected integral index type in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    else
                        unify(block, node, node.operand(3), node.operand(2).type);
                    break;
                case Opcode::SET_INDEX:
                    if (!validateArityAndDest(block, node, 4))
                        break;
                    unify(block, node, node.operand(3), node.type());
                    if (node.operand(1).kind != Operand::Type)
                        ctx.error(block, node, "Expected index type operand in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(1) }, '.');
                    else if (!isInt(node.operand(1).type))
                        ctx.error(block, node, "Expected integral index type in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(1) }, '.');
                    else
                        unify(block, node, node.operand(2), node.operand(1).type);
                    break;
                case Opcode::LOAD_INDEX:
                    if (!validateArityAndDest(block, node, 4))
                        break;
                    unify(block, node, node.operand(0), node.type());
                    unify(block, node, node.operand(1), PTR);
                    if (node.operand(2).kind != Operand::Type)
                        ctx.error(block, node, "Expected index type operand in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    else if (!isInt(node.operand(2).type))
                        ctx.error(block, node, "Expected integral index type in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    else
                        unify(block, node, node.operand(3), node.operand(2).type);
                    break;
                case Opcode::ADDR_INDEX:
                    if (!validateArityAndDest(block, node, 4))
                        break;
                    unify(block, node, node.operand(0), PTR);
                    if (node.operand(2).kind != Operand::Type)
                        ctx.error(block, node, "Expected index type operand in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    else if (!isInt(node.operand(2).type))
                        ctx.error(block, node, "Expected integral index type in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    else
                        unify(block, node, node.operand(3), node.operand(2).type);
                    break;
                case Opcode::OFFSET_INDEX:
                    if (!validateArityAndDest(block, node, 4))
                        break;
                    unify(block, node, node.operand(0), PTR);
                    unify(block, node, node.operand(1), PTR);
                    if (node.operand(2).kind != Operand::Type)
                        ctx.error(block, node, "Expected index type operand in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    else if (!isInt(node.operand(2).type))
                        ctx.error(block, node, "Expected integral index type in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(2) }, '.');
                    else
                        unify(block, node, node.operand(3), node.operand(2).type);
                    break;
                case Opcode::STORE_INDEX:
                    if (!validateArityAndDest(block, node, 4))
                        break;
                    unify(block, node, node.operand(3), node.type());
                    unify(block, node, node.operand(0), PTR);
                    if (node.operand(1).kind != Operand::Type)
                        ctx.error(block, node, "Expected index type operand in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(1) }, '.');
                    else if (!isInt(node.operand(1).type))
                        ctx.error(block, node, "Expected integral index type in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(1) }, '.');
                    else
                        unify(block, node, node.operand(2), node.operand(1).type);
                    break;
                case Opcode::RET:
                    if (!validateArity(block, node, node.type() == VOID ? 0 : 1))
                        break;
                    if (node.type() != VOID)
                        unify(block, node, node.operand(0), node.type());
                    break;
                case Opcode::CALL:
                    if (!isFunction(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected function type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& type = fn.typeContext()[node.type()];
                        if (type.returnType() == VOID)
                            ctx.error(block, node, "Expected non-void return type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, type.returnType() }, '.');
                        else {
                            unify(block, node, node.operand(0), type.returnType());
                            unify(block, node, node.operand(1), node.type());
                            if (node.operands().size() - 2 != type.argumentCount)
                                ctx.error(block, node, "Incorrect number of arguments in call node: expected ", type.argumentCount, ", found ", node.operands().size() - 2, '.');
                            else for (auto [t, o] : zip(type.arguments(), node.operands().drop(2)))
                                unify(block, node, o, t);
                        }
                    }
                    fn.makesCalls = true;
                    break;
                case Opcode::CALL_VOID:
                    if (!isFunction(fn.typeContext(), node.type()))
                        ctx.error(block, node, "Expected function type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, node.type() }, '.');
                    else {
                        auto& type = fn.typeContext()[node.type()];
                        if (type.returnType() != VOID)
                            ctx.error(block, node, "Expected void return type for node with opcode ", node.opcode(), ", found ", TypeLogger { fn, type.returnType() });
                        else {
                            unify(block, node, node.operand(0), node.type());
                            if (node.operands().size() - 1 != type.argumentCount)
                                ctx.error(block, node, "Incorrect number of arguments in call node: expected ", type.argumentCount, ", found ", node.operands().size() - 2, '.');
                            else for (auto [t, o] : zip(type.arguments(), node.operands().drop(1)))
                                unify(block, node, o, t);
                        }
                    }
                    fn.makesCalls = true;
                    break;
                case Opcode::BITCAST:
                case Opcode::CONVERT:
                    if (!validateArityAndDest(block, node, 3))
                        break;
                    if (node.operand(1).kind != Operand::Type)
                        ctx.error(block, node, "Expected type operand in node with opcode ", node.opcode(), ", found ", OperandLogger { fn, node.operand(1) }, ".");
                    unify(block, node, node.operand(2), node.operand(1).type);
                    unify(block, node, node.operand(0), node.type());
                    break;
                case Opcode::TRAP:
                    validateArity(block, node, 0);
                    break;
                case Opcode::ALLOCA:
                    fn.hasAlloca = true;
                    if (!validateArityAndDest(block, node, 2))
                        break;
                    unify(block, node, node.operand(1), node.type());
                    unify(block, node, node.operand(0), PTR);
                    break;
                case Opcode::PUSH:
                case Opcode::POP:
                    unreachable("Illegal in this phase.");
                default:
                    break;
                    // unreachable("Invalid opcode.");
            }

            if (block.nodeIndices().size() == 0) {
                if (block.predecessorIndices().size() > 0)
                    ctx.error(block, -1, "Expected last node of block to be a branch or return, but block ", block.index(), " is empty.");
                // Otherwise, this is an unreachable block, it's not really causing any problems.
            } else {
                Opcode tail = block.last().opcode();
                if (!isBranch(tail) && tail != Opcode::RET && tail != Opcode::TRAP)
                    ctx.error(block, block.last(), "Expected last node of block to be a branch or return or trap, found ", block.last(), '.');
            }
        }

        if (ctx.errored())
            ctx.printErrors(file::stderr);
    }
}