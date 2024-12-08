#include "jasmine/mod.h"
#include "jasmine/pass.h"
#include "util/config.h"

namespace jasmine {
    void validateAfterLowering(PassContext& ctx, Function& fn) {
        JASMINE_PASS(VALIDATION);
        ctx.require(LOWER);

        if UNLIKELY(config::printJasmineAfterOpts)
            println("=== IR for function ", fn.name(), " after opts ===\n", ScheduleLogger { fn, *ctx.schedule });

        auto gpOrImm = [&](Node node) {
            bool seenImm = false;
            for (auto [i, operand] : enumerate(node.operands())) {
                assert(operand.kind == Operand::GP || (operand.kind == Operand::IntConst && !seenImm && i));
                seenImm = seenImm || operand.kind == Operand::IntConst;
            }
        };

        auto fpOrF32 = [&](Node node) {
            bool seenImm = false;
            for (auto [i, operand] : enumerate(node.operands())) {
                assert(operand.kind == Operand::FP || (operand.kind == Operand::F32Const && !seenImm && i));
                seenImm = seenImm || operand.kind == Operand::F32Const;
            }
        };

        auto fpOrF64 = [&](Node node) {
            bool seenImm = false;
            for (auto [i, operand] : enumerate(node.operands())) {
                assert(operand.kind == Operand::FP || (operand.kind == Operand::F64Const && !seenImm && i));
                seenImm = seenImm || operand.kind == Operand::F64Const;
            }
        };

        auto gpOnly = [&](Node node) {
            for (auto operand : node.operands())
                assert(operand.kind == Operand::GP);
        };

        auto fpOnly = [&](Node node) {
            for (auto operand : node.operands())
                assert(operand.kind == Operand::FP);
        };

        auto fpOrImm = [&](Node node) {
            if (node.type() == F32)
                fpOrF32(node);
            else if (node.type() == F64)
                fpOrF64(node);
            else
                unreachable("Invalid type for instruction ", node);
        };

        auto regOrImm = [&](Node node) {
            if (isGPType(fn, node.type()))
                gpOrImm(node);
            else if (node.type() == F32)
                fpOrF32(node);
            else if (node.type() == F64)
                fpOrF64(node);
            else
                unreachable("Invalid type for instruction ", node);
        };

        auto regOnly = [&](Node node) {
            if (isGPType(fn, node.type()))
                gpOnly(node);
            else if (isFloat(node.type()))
                fpOnly(node);
            else
                unreachable("Invalid type for instruction ", node);
        };

        auto gpOrImmBranch = [&](Node node) {
            bool seenImm = false;
            for (auto [i, operand] : enumerate(node.operands())) {
                if (i >= 2)
                    assert(operand.kind == Operand::Branch);
                else {
                    assert(operand.kind == Operand::GP || (operand.kind == Operand::IntConst && !seenImm));
                    seenImm = seenImm || operand.kind == Operand::IntConst;
                }
            }
        };

        auto fpOrF32Branch = [&](Node node) {
            bool seenImm = false;
            for (auto [i, operand] : enumerate(node.operands())) {
                if (i >= 2)
                    assert(operand.kind == Operand::Branch);
                else {
                    assert(operand.kind == Operand::FP || (operand.kind == Operand::F32Const && !seenImm));
                    seenImm = seenImm || operand.kind == Operand::F32Const;
                }
            }
        };

        auto fpOrF64Branch = [&](Node node) {
            bool seenImm = false;
            for (auto [i, operand] : enumerate(node.operands())) {
                if (i >= 2)
                    assert(operand.kind == Operand::Branch);
                else {
                    assert(operand.kind == Operand::FP || (operand.kind == Operand::F64Const && !seenImm));
                    seenImm = seenImm || operand.kind == Operand::F64Const;
                }
            }
        };

        auto regOrImmBranch = [&](Node node) {
            if (isGPType(fn, node.type()))
                gpOrImmBranch(node);
            else if (node.type() == F32)
                fpOrF32Branch(node);
            else if (node.type() == F64)
                fpOrF64Branch(node);
            else
                unreachable("Invalid type for instruction.");
        };

        for (Block block : fn.blocks()) {
            for (Node node : block.nodes()) switch (node.opcode()) {
                case Opcode::NOP:
                    break;
                case Opcode::VAR:
                case Opcode::NEW:
                case Opcode::NEW_STRUCT:
                case Opcode::NEW_ARRAY:
                    assert(node.operands().size() == 1);
                    assert(node.operand(0).isReg());
                    break;
                case Opcode::MOV:
                    assert(node.operands().size() == 2);
                    regOrImm(node);
                    break;
                case Opcode::NEG:
                    assert(node.operands().size() == 2);
                    regOnly(node);
                    break;
                case Opcode::ABS:
                case Opcode::SQRT:
                case Opcode::ROUND:
                case Opcode::FLOOR:
                case Opcode::CEIL:
                    assert(node.operands().size() == 2);
                    fpOnly(node);
                    break;
                case Opcode::MIN:
                case Opcode::MAX:
                    assert(node.operands().size() == 3);
                    fpOrImm(node);
                    break;
                case Opcode::AND:
                case Opcode::OR:
                case Opcode::XOR:
                case Opcode::SHL:
                case Opcode::SHR:
                case Opcode::ROL:
                case Opcode::ROR:
                    assert(node.operands().size() == 3);
                    gpOrImm(node);
                    break;
                case Opcode::NOT:
                case Opcode::LZC:
                case Opcode::TZC:
                case Opcode::POPC:
                    assert(node.operands().size() == 2);
                    gpOnly(node);
                    break;
                case Opcode::ADD:
                case Opcode::SUB:
                case Opcode::MUL:
                case Opcode::DIV:
                case Opcode::REM:
                case Opcode::IS_LT:
                case Opcode::IS_LE:
                case Opcode::IS_GT:
                case Opcode::IS_GE:
                case Opcode::IS_EQ:
                case Opcode::IS_NE:
                    assert(node.operands().size() == 3);
                    regOrImm(node);
                    break;
                case Opcode::IS_INB:
                case Opcode::IS_OOB:
                    assert(node.operands().size() == 4);
                    assert(node.operand(0).kind == Operand::GP);
                    assert(node.operand(1).isReg() || node.operand(1).isConst());
                    assert(node.operand(2).isReg() || node.operand(2).isConst());
                    assert(node.operand(3).isReg() || node.operand(3).isConst());
                    break;
                case Opcode::BR:
                    assert(node.operands().size() == 1);
                    assert(node.operand(0).kind == Operand::Branch);
                    break;
                case Opcode::BR_IF:
                case Opcode::BR_IF_NOT:
                    assert(node.operands().size() == 2);
                    assert(node.operand(0).kind == Operand::GP || node.operand(0).kind == Operand::IntConst);
                    assert(node.operand(1).kind == Operand::Branch);
                    break;
                case Opcode::BR_LT:
                case Opcode::BR_LE:
                case Opcode::BR_GT:
                case Opcode::BR_GE:
                case Opcode::BR_EQ:
                case Opcode::BR_NE:
                    assert(node.operands().size() == 3);
                    regOrImmBranch(node);
                    break;
                case Opcode::BR_INB:
                case Opcode::BR_OOB:
                    assert(node.operands().size() == 4);
                    assert(node.operand(0).isReg() || node.operand(0).isConst());
                    assert(node.operand(1).isReg() || node.operand(1).isConst());
                    assert(node.operand(2).isReg() || node.operand(2).isConst());
                    assert(node.operand(3).isLabel());
                    break;
                case Opcode::BR_ADD_O:
                case Opcode::BR_SUB_O:
                case Opcode::BR_MUL_O:
                    assert(node.operands().size() == 4);
                    assert(node.operand(0).isReg());
                    assert(node.operand(1).isReg() || node.operand(1).isConst());
                    assert(node.operand(2).isReg() || node.operand(2).isConst());
                    assert(node.operand(3).isLabel());
                    break;
                case Opcode::LOAD:
                case Opcode::ADDR:
                    assert(node.operands().size() == 2);
                    assert(node.operand(0).isReg());
                    assert(node.operand(1).kind == Operand::Memory || node.operand(1).isLabel());
                    break;
                case Opcode::STORE:
                    assert(node.operands().size() == 2);
                    assert(node.operand(0).kind == Operand::Memory || node.operand(0).isLabel());
                    assert(node.operand(1).isReg() || node.operand(1).isConst());
                    break;
                case Opcode::LOAD_INDEX:
                case Opcode::ADDR_INDEX:
                    assert(node.operands().size() == 3);
                    assert(node.operand(0).isReg());
                    assert(node.operand(1).kind == Operand::Memory);
                    assert(node.operand(2).kind == Operand::GP); // Constant index is illegal in the assembler, so it's illegal here too.
                    break;
                case Opcode::STORE_INDEX:
                    assert(node.operands().size() == 3);
                    assert(node.operand(0).kind == Operand::Memory);
                    assert(node.operand(1).kind == Operand::GP); // Constant index is illegal in the assembler, so it's illegal here too.
                    assert(node.operand(2).isReg() || node.operand(2).isConst());
                    break;
                case Opcode::RET:
                case Opcode::TRAP:
                    assert(node.operands().size() == 0);
                    break;
                case Opcode::CALL:
                    break;
                case Opcode::CALL_VOID:
                    unreachable("Should have been lowered to normal call.");
                case Opcode::BITCAST:
                case Opcode::CONVERT:
                    assert(node.operands().size() == 3);
                    assert(node.operand(1).kind == Operand::Type);
                    assert(node.operand(0).kind == (isInt(node.type()) ? Operand::GP : Operand::FP));
                    assert(node.operand(2).kind == (isInt(node.operand(1).type) ? Operand::GP : Operand::FP));
                    break;
                case Opcode::GET_FIELD:
                case Opcode::SET_FIELD:
                case Opcode::GET_INDEX:
                case Opcode::SET_INDEX:
                    unreachable("Field and index get and set should be lowered into loads and stores by this point.");
                case Opcode::LOAD_FIELD:
                case Opcode::ADDR_FIELD:
                case Opcode::STORE_FIELD:
                    unreachable("Field memory operations should have been lowered into normal memory operations by this point.");
                case Opcode::OFFSET_FIELD:
                case Opcode::OFFSET_INDEX:
                    unreachable("Address-offset instructions should have been lowered into other addressing or arithmetic instructions by this point.");
                case Opcode::PACK:
                case Opcode::UNPACK:
                    unreachable("Pack and unpack instructions should have been lowered into individual moves by this point.");
                case Opcode::PUSH:
                case Opcode::POP:
                    assert(node.operands().size() == 1);
                    assert(node.operand(0).isReg());
                    break;
                case Opcode::ALLOCA:
                    assert(node.operands().size() == 2);
                    assert(node.operand(1).isReg() || node.operand(1).isConst());
                    assert(node.operand(0).kind == Operand::GP);
                    break;
            }
        }
    }
}