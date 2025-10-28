#ifndef JASMINE_PASS_GENERATE_H
#define JASMINE_PASS_GENERATE_H

#include "util/config.h"
#include "jasmine/pass/helpers.h"

namespace jasmine {
    template<typename Target>
    struct Instructions {
        #define UNARY_INT(name) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## 8(as, dst); \
                case Size::BITS16: return Target:: name ## 16(as, dst); \
                case Size::BITS32: return Target:: name ## 32(as, dst); \
                case Size::BITS64: return Target:: name ## 64(as, dst); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define UNARY_FLOAT(name) { \
            switch (type.kind()) { \
                case Size::FLOAT32: return Target:: f ## name ## 32(as, dst); \
                case Size::FLOAT64: return Target:: f ## name ## 64(as, dst); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define UNARY_INT_FLOAT(name) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## 8(as, dst); \
                case Size::BITS16: return Target:: name ## 16(as, dst); \
                case Size::BITS32: return Target:: name ## 32(as, dst); \
                case Size::BITS64: return Target:: name ## 64(as, dst); \
                case Size::FLOAT32: return Target:: f ## name ## 32(as, dst); \
                case Size::FLOAT64: return Target:: f ## name ## 64(as, dst); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define BINARY_INT(name) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## 8(as, dst, src); \
                case Size::BITS16: return Target:: name ## 16(as, dst, src); \
                case Size::BITS32: return Target:: name ## 32(as, dst, src); \
                case Size::BITS64: return Target:: name ## 64(as, dst, src); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define EXTEND_INT(name) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## 8(as, dst, src); \
                case Size::BITS16: return Target:: name ## 16(as, dst, src); \
                case Size::BITS32: return Target:: name ## 32(as, dst, src); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define BINARY_INT_TO_FLOAT(prefix, suffix) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: prefix ## 8 ## suffix(as, dst, src); \
                case Size::BITS16: return Target:: prefix ## 16 ## suffix(as, dst, src); \
                case Size::BITS32: return Target:: prefix ## 32 ## suffix(as, dst, src); \
                case Size::BITS64: return Target:: prefix ## 64 ## suffix(as, dst, src); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define LOAD_INT(name, suffix) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## suffix ## 8(as, dst, src); \
                case Size::BITS16: return Target:: name ## suffix ## 16(as, dst, src); \
                case Size::BITS32: return Target:: name ## suffix ## 32(as, dst, src); \
                case Size::BITS64: return Target:: name ## 64(as, dst, src); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define LOAD_INDEXED_INT(name, suffix) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## suffix ## 8(as, dst, a, b); \
                case Size::BITS16: return Target:: name ## suffix ## 16(as, dst, a, b); \
                case Size::BITS32: return Target:: name ## suffix ## 32(as, dst, a, b); \
                case Size::BITS64: return Target:: name ## 64(as, dst, a, b); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define BINARY_FLOAT(name) { \
            switch (type.kind()) { \
                case Size::FLOAT32: return Target:: f ## name ## 32(as, dst, src); \
                case Size::FLOAT64: return Target:: f ## name ## 64(as, dst, src); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define BINARY_INT_FLOAT(name) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## 8(as, dst, src); \
                case Size::BITS16: return Target:: name ## 16(as, dst, src); \
                case Size::BITS32: return Target:: name ## 32(as, dst, src); \
                case Size::BITS64: return Target:: name ## 64(as, dst, src); \
                case Size::FLOAT32: return Target:: f ## name ## 32(as, dst, src); \
                case Size::FLOAT64: return Target:: f ## name ## 64(as, dst, src); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define TERNARY_INT(name) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## 8(as, dst, a, b); \
                case Size::BITS16: return Target:: name ## 16(as, dst, a, b); \
                case Size::BITS32: return Target:: name ## 32(as, dst, a, b); \
                case Size::BITS64: return Target:: name ## 64(as, dst, a, b); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define TERNARY_FLOAT(name) { \
            switch (type.kind()) { \
                case Size::FLOAT32: return Target:: f ## name ## 32(as, dst, a, b); \
                case Size::FLOAT64: return Target:: f ## name ## 64(as, dst, a, b); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define TERNARY_INT_COND(name) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## 8(as, cond, dst, a, b); \
                case Size::BITS16: return Target:: name ## 16(as, cond, dst, a, b); \
                case Size::BITS32: return Target:: name ## 32(as, cond, dst, a, b); \
                case Size::BITS64: return Target:: name ## 64(as, cond, dst, a, b); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define TERNARY_FLOAT_COND(name) { \
            switch (type.kind()) { \
                case Size::FLOAT32: return Target:: f ## name ## 32(as, cond, dst, a, b); \
                case Size::FLOAT64: return Target:: f ## name ## 64(as, cond, dst, a, b); \
                default: unreachable("Unsupported size."); \
            } \
        }

        #define TERNARY_INT_FLOAT(name) { \
            switch (type.kind()) { \
                case Size::BITS8: return Target:: name ## 8(as, dst, a, b); \
                case Size::BITS16: return Target:: name ## 16(as, dst, a, b); \
                case Size::BITS32: return Target:: name ## 32(as, dst, a, b); \
                case Size::BITS64: return Target:: name ## 64(as, dst, a, b); \
                case Size::FLOAT32: return Target:: f ## name ## 32(as, dst, a, b); \
                case Size::FLOAT64: return Target:: f ## name ## 64(as, dst, a, b); \
                default: unreachable("Unsupported size."); \
            } \
        }

        static void mov(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT_FLOAT(mov)
        static void ld(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_FLOAT(ld)
        static void lds(Assembly& as, Repr type, ASMVal dst, ASMVal src) LOAD_INT(ld, s)
        static void ldz(Assembly& as, Repr type, ASMVal dst, ASMVal src) LOAD_INT(ld, z)
        static void st(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT_FLOAT(st)
        static void ldi(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_FLOAT(ldi)
        static void ldis(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) LOAD_INDEXED_INT(ldi, s)
        static void ldiz(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) LOAD_INDEXED_INT(ldi, z)
        static void sti(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT_FLOAT(sti)
        static void push(Assembly& as, Repr type, ASMVal dst) UNARY_INT_FLOAT(push)
        static void pop(Assembly& as, Repr type, ASMVal dst) UNARY_INT_FLOAT(pop)

        static void move(Assembly& as, Repr type, ASMVal dst, ASMVal src) {
            assert(src.kind != ASMVal::IMM64);
            if (dst.is_reg() && (src.is_reg() || src.is_const()))
                mov(as, type, dst, src);
            else if (dst.is_reg() && src.kind == ASMVal::MEM)
                lds(as, type, dst, src);
            else if (dst.kind == ASMVal::MEM && (src.is_reg() || src.is_const()))
                st(as, type, dst, src);
            else {
                push(as, type, src);
                pop(as, type, dst);
            }
        }

        static void add(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT_FLOAT(add)
        static void sub(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT_FLOAT(sub)
        static void mul(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT_FLOAT(mul)
        static void sdiv(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(sdiv)
        static void udiv(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(udiv)
        static void div(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_FLOAT(div)
        static void srem(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(srem)
        static void urem(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(urem)
        static void rem(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_FLOAT(rem)
        static void neg(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT_FLOAT(neg)

        static void band(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(and)
        static void bxor(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(xor)
        static void bor(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(or)
        static void bnot(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT(not)
        static void shl(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(shl)
        static void shr(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(shr)
        static void sar(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(sar)
        static void rol(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(rol)
        static void ror(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT(ror)
        static void tzcnt(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT(tzcnt)
        static void lzcnt(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT(lzcnt)
        static void popcnt(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT(popcnt)

        static void abs(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_FLOAT(abs);
        static void min(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_FLOAT(min);
        static void max(Assembly& as, Repr type, ASMVal dst, ASMVal a, ASMVal b) TERNARY_FLOAT(max);
        static void sqrt(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_FLOAT(sqrt);
        static void round(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_FLOAT(round);
        static void floor(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_FLOAT(floor);
        static void ceil(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_FLOAT(ceil);

        static void cmp(Assembly& as, Repr type, Condition cond, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT_COND(cmpcc)
        static void fcmp(Assembly& as, Repr type, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b) TERNARY_FLOAT_COND(cmpcc)
        static void brcmp(Assembly& as, Repr type, Condition cond, ASMVal dst, ASMVal a, ASMVal b) TERNARY_INT_COND(brcc)
        static void fbrcmp(Assembly& as, Repr type, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b) TERNARY_FLOAT_COND(brcc)

        static void sxt(Assembly& as, Repr type, ASMVal dst, ASMVal src) EXTEND_INT(sxt)
        static void zxt(Assembly& as, Repr type, ASMVal dst, ASMVal src) EXTEND_INT(zxt)
        static void f32toi(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT(f32toi);
        static void f64toi(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT(f64toi);
        static void itof32(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT_TO_FLOAT(i, tof32);
        static void itof64(Assembly& as, Repr type, ASMVal dst, ASMVal src) BINARY_INT_TO_FLOAT(i, tof64);
    };

    template<typename Target>
    ASMVal lowerOperand(PassContext& ctx, Function& fn, Assembly& as, const vec<ASMVal, 16>& blockLabels, Operand operand) {
        switch (operand.kind) {
            case Operand::IntConst:
                if (fn.intValueOf(operand) >= -0x80000000ll && fn.intValueOf(operand) <= 0x7fffffffll)
                    return Imm(fn.intValueOf(operand));
                else
                    return Imm64(fn.intValueOf(operand));
            case Operand::F32Const:
                return ::F32(fn.f32ValueOf(operand));
            case Operand::F64Const:
                return ::F64(fn.f64ValueOf(operand));
            case Operand::GP:
                return GP(operand.gp);
            case Operand::FP:
                return FP(operand.fp);
            case Operand::Memory:
                return Mem(operand.base, operand.offset);
            case Operand::Func:
                return Func(as.symtab[fn.syms()[operand.sym]]);
            case Operand::Data:
                return Data(as.symtab[fn.syms()[operand.sym]]);
            case Operand::Static:
                return Static(as.symtab[fn.syms()[operand.sym]]);
            case Operand::Branch:
                return blockLabels[fn.edge(operand.edge).destIndex()];
            case Operand::RegPair:
                unreachable("Can't directly lower register pair.");
            case Operand::Var:
                unreachable("Attempted to lower non-allocated variable operand.");
            case Operand::Invalid:
                unreachable("Invalid operand.");
            case Operand::Type:
                return ASMVal();
            case Operand::Sizeof:
                unreachable("Should have already lowered to integer constant.");
            case Operand::String:
                unreachable("Shouldn't try to lower string operand in comment.");
        }
    }

    template<typename Target>
    void TargetSpecificPasses<Target>::generateAssembly(PassContext& ctx, Function& fn, Assembly& as) {
        JASMINE_PASS(CODE_GENERATION);
        ctx.require(LOWER);
        ctx.require(FINALIZE);

        if UNLIKELY(config::printJasmineDOTAfterOpts || config::printJasmineDOTBeforeOpts || config::printJasmineDOTBeforeEachPass)
            finishDOT(ctx, fn);

        #define LOWER(operand) lowerOperand<Target>(ctx, fn, as, blockLabels, operand)

        if (config::printJasmineAssembly)
            println("=== Assembly for function ", fn.name(), " ===");

        auto& allocations = *ctx.allocations;

        vec<Symbol, 16> blockNames;
        vec<ASMVal, 16> blockLabels;
        for (Block b : fn.blocks()) {
            blockNames.push(as.symtab.anon());
            blockLabels.push(LocalLabel(Label::fromSym(blockNames.back())));
        }

        u32 totalStackWithCalleeSaves = allocations.stack + (allocations.stack ? 8 : 0) + allocations.calleeSaves.size() * 8;

        Target::global(as, Label::fromSym(as.symtab[fn.name()]));

        if (totalStackWithCalleeSaves % 16 == 0 && fn.makesCalls) {
            // We need to make sure our callees have 16-byte aligned stack. So
            // if our stack before we call something is 16-byte aligned, then
            // we need an extra 8 bytes of padding.
            if (allocations.stack)
                allocations.stack += 8;
            else
                Target::stack(as, Imm(8));
        }
        if (allocations.stack || fn.hasAlloca) {
            Target::enter(as);
            Target::stack(as, Imm(allocations.stack));
        }

        using Insns = Instructions<Target>;

        // We use these in really rare scenarios where we don't want to always reserve a clobber.
        // For these, we push the old value somewhere, do the operation, then pop the value to
        // restore it.
        mreg emergencyGPScratch = (Target::caller_saves() & Target::gps()).next();
        mreg backupEmergencyGPScratch = (Target::caller_saves() & Target::gps()).without(emergencyGPScratch).next();
        mreg emergencyFPScratch =(Target::caller_saves() & Target::fps()).next();
        mreg backupEmergencyFPScratch = (Target::caller_saves() & Target::fps()).without(emergencyFPScratch).next();

        Repr pointerRepr = this->repr(PTR);

        auto getEmergencyGP = [&](mreg overlap) -> ASMVal {
            mreg r = overlap == emergencyGPScratch ? backupEmergencyGPScratch : emergencyGPScratch;
            Insns::push(as, pointerRepr, GP(r));
            return GP(r);
        };
        auto restoreEmergencyGP = [&](ASMVal r) {
            Insns::pop(as, pointerRepr, r);
        };
        auto getEmergencyFP = [&](mreg overlap) -> ASMVal {
            mreg r = overlap == emergencyFPScratch ? backupEmergencyFPScratch : emergencyFPScratch;
            Insns::push(as, Repr::Scalar(Size::FLOAT64), FP(r));
            return FP(r);
        };
        auto restoreEmergencyFP = [&](ASMVal r) {
            Insns::pop(as, Repr::Scalar(Size::FLOAT64), r);
        };

        vec<ASMVal, 8> loweredOperands;
        auto branchCompare = [&](Condition signedCondition, Condition unsignedCondition, FloatCondition floatCondition, Node n, Repr repr) {
            if (isSigned(n.type()))
                Insns::brcmp(as, repr, signedCondition, loweredOperands[2], loweredOperands[0], loweredOperands[1]);
            else if (isGPType(fn, n.type()))
                Insns::brcmp(as, repr, unsignedCondition, loweredOperands[2], loweredOperands[0], loweredOperands[1]);
            else if (isFloat(n.type()))
                Insns::fbrcmp(as, repr, floatCondition, loweredOperands[2], loweredOperands[0], loweredOperands[1]);
        };

        for (u32 i : *ctx.schedule) {
            Block b = fn.block(i);
            Target::local(as, Label::fromSym(blockNames[b.index()]));
            for (Node n : b.nodes()) {
                Repr repr = this->repr(n.type());
                loweredOperands.clear();
                for (Operand o : n.operands())
                    loweredOperands.push(lowerOperand<Target>(ctx, fn, as, blockLabels, o));
                switch (n.opcode()) {
                    case Opcode::ADDR:
                        Target::la(as, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::LOAD:
                        if (isSigned(n.type()))
                            Insns::lds(as, repr, loweredOperands[0], loweredOperands[1]);
                        else if (isGPType(fn, n.type()))
                            Insns::ldz(as, repr, loweredOperands[0], loweredOperands[1]);
                        else if (isFloat(n.type()))
                            Insns::ld(as, repr, loweredOperands[0], loweredOperands[1]);
                        else
                            unreachable("Unexpected load type.");
                        break;
                    case Opcode::STORE:
                        Insns::st(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::LOAD_INDEX:
                        if (isSigned(n.type()))
                            Insns::ldis(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isGPType(fn, n.type()))
                            Insns::ldiz(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::ldi(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected load type.");
                        break;
                    case Opcode::STORE_INDEX:
                        Insns::sti(as, repr, loweredOperands[0], loweredOperands[2], loweredOperands[1]);
                        break;
                    case Opcode::ADDR_INDEX:
                        switch (repr.kind()) {
                            case Size::BITS8:
                                Target::lai8(as, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                                break;
                            case Size::BITS16:
                                Target::lai16(as, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                                break;
                            case Size::BITS32:
                            case Size::FLOAT32:
                                Target::lai32(as, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                                break;
                            case Size::BITS64:
                            case Size::FLOAT64:
                                Target::lai64(as, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                                break;
                            default:
                                unreachable("Unexpected type in addr_index.");
                        }
                        break;
                    case Opcode::PUSH:
                        Insns::push(as, repr, loweredOperands[0]);
                        break;
                    case Opcode::POP:
                        Insns::pop(as, repr, loweredOperands[0]);
                        break;
                    case Opcode::MOV:
                        if (n.operand(1).kind == Operand::IntConst && !fits<TypeKind::I32>(fn.intValueOf(n.operand(1)))) {
                            assert(isGPType(fn, n.type()));
                            Target::lc(as, loweredOperands[0], loweredOperands[1]);
                        } else
                            Insns::move(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::ADD:
                        Insns::add(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::SUB:
                        Insns::sub(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::MUL:
                        Insns::mul(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::DIV:
                        if (isUnsigned(n.type()))
                            Insns::udiv(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isSigned(n.type()))
                            Insns::sdiv(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::div(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected div type.");
                        break;
                    case Opcode::REM:
                        if (isUnsigned(n.type()))
                            Insns::urem(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isSigned(n.type()))
                            Insns::srem(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::rem(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected rem type.");
                        break;
                    case Opcode::NEG:
                        Insns::neg(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::AND:
                        Insns::band(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::XOR:
                        Insns::bxor(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::OR:
                        Insns::bor(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::NOT:
                        Insns::bnot(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::SHL:
                        Insns::shl(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::SHR:
                        if (isUnsigned(n.type()))
                            Insns::shr(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isSigned(n.type()))
                            Insns::sar(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected shr type.");
                        break;
                    case Opcode::ROL:
                        Insns::rol(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::ROR:
                        Insns::ror(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::TZCNT:
                        Insns::tzcnt(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::LZCNT:
                        Insns::lzcnt(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::POPCNT:
                        Insns::popcnt(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::ABS:
                        Insns::abs(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::MIN:
                        Insns::min(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::MAX:
                        Insns::max(as, repr, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        break;
                    case Opcode::SQRT:
                        Insns::sqrt(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::ROUND:
                        Insns::round(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::FLOOR:
                        Insns::floor(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::CEIL:
                        Insns::ceil(as, repr, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::IS_LT:
                        if (isSigned(n.type()))
                            Insns::cmp(as, repr, COND_LT, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isUnsigned(n.type()))
                            Insns::cmp(as, repr, COND_BELOW, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::fcmp(as, repr, FCOND_LT, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected cmp type.");
                        break;
                    case Opcode::IS_LE:
                        if (isSigned(n.type()))
                            Insns::cmp(as, repr, COND_LE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isUnsigned(n.type()))
                            Insns::cmp(as, repr, COND_BE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::fcmp(as, repr, FCOND_LE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected cmp type.");
                        break;
                    case Opcode::IS_GT:
                        if (isSigned(n.type()))
                            Insns::cmp(as, repr, COND_GT, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isUnsigned(n.type()))
                            Insns::cmp(as, repr, COND_ABOVE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::fcmp(as, repr, FCOND_GT, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected cmp type.");
                        break;
                    case Opcode::IS_GE:
                        if (isSigned(n.type()))
                            Insns::cmp(as, repr, COND_GE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isUnsigned(n.type()))
                            Insns::cmp(as, repr, COND_AE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::fcmp(as, repr, FCOND_GE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected cmp type.");
                        break;
                    case Opcode::IS_EQ:
                        if (isGPType(fn, n.type()))
                            Insns::cmp(as, repr, COND_EQ, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::fcmp(as, repr, FCOND_EQ, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected cmp type.");
                        break;
                    case Opcode::IS_NE:
                        if (isGPType(fn, n.type()))
                            Insns::cmp(as, repr, COND_NE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else if (isFloat(n.type()))
                            Insns::fcmp(as, repr, FCOND_NE, loweredOperands[0], loweredOperands[1], loweredOperands[2]);
                        else
                            unreachable("Unexpected cmp type.");
                        break;
                    case Opcode::BR:
                        Target::br(as, loweredOperands[0]);
                        break;
                    case Opcode::BR_IF:
                        if (n.operand(0).kind == Operand::IntConst && fn.intValueOf(n.operand(0)))
                            Target::br(as, loweredOperands[1]);
                        else
                            Target::brcc8(as, COND_TEST_NONZERO, loweredOperands[1], loweredOperands[0], loweredOperands[0]);
                        break;
                    case Opcode::BR_IF_NOT:
                        if (n.operand(0).kind == Operand::IntConst && !fn.intValueOf(n.operand(0)))
                            Target::br(as, loweredOperands[1]);
                        else
                            Target::brcc8(as, COND_TEST_ZERO, loweredOperands[1], loweredOperands[0], loweredOperands[0]);
                        break;
                    case Opcode::BR_LT:
                        branchCompare(COND_LT, COND_BELOW, FCOND_LT, n, repr);
                        break;
                    case Opcode::BR_LE:
                        branchCompare(COND_LE, COND_BE, FCOND_LE, n, repr);
                        break;
                    case Opcode::BR_GT:
                        branchCompare(COND_GT, COND_ABOVE, FCOND_GT, n, repr);
                        break;
                    case Opcode::BR_GE:
                        branchCompare(COND_GE, COND_AE, FCOND_GE, n, repr);
                        break;
                    case Opcode::BR_EQ:
                        branchCompare(COND_EQ, COND_EQ, FCOND_EQ, n, repr);
                        break;
                    case Opcode::BR_NE:
                        branchCompare(COND_NE, COND_NE, FCOND_NE, n, repr);
                        break;
                    case Opcode::CALL: {
                        Target::call(as, loweredOperands[0]);
                        break;
                    }
                    case Opcode::RET: {
                        if (totalStackWithCalleeSaves % 16 == 0 && !allocations.stack && fn.makesCalls)
                            Target::unstack(as, Imm(8));
                        if (allocations.stack || fn.hasAlloca)
                            Target::leave(as);
                        Target::ret(as);
                        break;
                    }
                    case Opcode::TRAP:
                        Target::trap(as);
                        break;
                    case Opcode::ALLOCA:
                        Target::alloca(as, loweredOperands[0], loweredOperands[1]);
                        break;
                    case Opcode::BITCAST: {
                        TypeIndex srcType = n.operand(1).type, destType = n.type();
                        Repr srcRepr = this->repr(srcType), destRepr = this->repr(destType);
                        if (isFloat(srcType) && isInt(destType)) {
                            if (srcType == F32)
                                Target::f32tobits(as, loweredOperands[0], loweredOperands[2]);
                            else
                                Target::f64tobits(as, loweredOperands[0], loweredOperands[2]);
                        } else if (isInt(srcType) && isFloat(destType)) {
                            if (srcType == F32)
                                Target::f32frombits(as, loweredOperands[0], loweredOperands[2]);
                            else
                                Target::f64frombits(as, loweredOperands[0], loweredOperands[2]);
                        } else
                            Insns::mov(as, destRepr, loweredOperands[0], loweredOperands[2]);
                        break;
                    }
                    case Opcode::CONVERT: {
                        TypeIndex srcType = n.operand(1).type, destType = n.type();
                        Repr srcRepr = this->repr(srcType), destRepr = this->repr(destType);
                        if (srcType == destType)
                            Insns::mov(as, destRepr, loweredOperands[0], loweredOperands[2]);
                        if (isInt(destType) && isInt(srcType)) {
                            if (destRepr.size() <= srcRepr.size()) // Truncation or just changing signedness - in either case, it's just a move.
                                Insns::mov(as, destRepr, loweredOperands[0], loweredOperands[2]);
                            else if (isSigned(destType)) // Otherwise, we're doing some kind of extension.
                                Insns::sxt(as, srcRepr, loweredOperands[0], loweredOperands[2]);
                            else
                                Insns::zxt(as, srcRepr, loweredOperands[0], loweredOperands[2]);
                        } else if (isInt(destType) && isFloat(srcType)) {
                            if (srcType == F32)
                                Insns::f32toi(as, destRepr, loweredOperands[0], loweredOperands[2]);
                            else
                                Insns::f64toi(as, destRepr, loweredOperands[0], loweredOperands[2]);
                        } else if (isFloat(destType) && isInt(srcType)) {
                            if (destType == F32)
                                Insns::itof32(as, srcRepr, loweredOperands[0], loweredOperands[2]);
                            else
                                Insns::itof64(as, srcRepr, loweredOperands[0], loweredOperands[2]);
                        } else if (isFloat(destType) && isFloat(srcType)) {
                            if (destType == F32)
                                Target::f64tof32(as, loweredOperands[0], loweredOperands[2]);
                            else
                                Target::f32tof64(as, loweredOperands[0], loweredOperands[2]);
                        } else
                            unreachable("Can't convert from ", TypeLogger { fn, srcType }, " to ", TypeLogger { fn, destType }, ".");
                        break;
                    }
                    case Opcode::NOP:
                    case Opcode::COMMENT:
                        break;
                    default:
                        unreachable("Unimplemented assembly lowering for opcode ", OPCODE_NAMES[(u32)n.opcode()], ".");
                }
            }
        }

        if (config::printJasmineAssembly)
            println();
    }

    template<typename Target>
    void TargetSpecificPasses<Target>::generateDataValue(Module* mod, Assembly& as, bytebuf& buf, Value value, maybe<DefInfo> def) {
        auto alignTo = [&](u32 alignment) {
            while (buf.size() % alignment)
                buf.write<i8>(0);
        };
        ConstantTable& constants = mod->constants;
        const auto& words = mod->values.words;
        switch (value.kind) {
            case Value::I8:
                if (def) def->def(as);
                buf.write<i8>(i8(value.payload));
                break;
            case Value::I16:
                alignTo(2);
                if (def) def->def(as);
                buf.write<i16>(with_endian<Target::endianness, i16>(value.payload));
                break;
            case Value::I32: {
                alignTo(4);
                if (def) def->def(as);
                i32 i = value.isInline ? value.payload : constants[value.payload].i;
                buf.write<i32>(with_endian<Target::endianness, i32>(i));
                break;
            }
            case Value::I64: {
                alignTo(8);
                if (def) def->def(as);
                i64 i = value.isInline ? value.payload : constants[value.payload].i;
                buf.write<i64>(with_endian<Target::endianness, i64>(i));
                break;
            }
            case Value::U8:
                if (def) def->def(as);
                buf.write<u8>(u8(value.payload));
                break;
            case Value::U16:
                alignTo(2);
                if (def) def->def(as);
                buf.write<u16>(with_endian<Target::endianness, u16>(value.payload));
                break;
            case Value::U32: {
                alignTo(4);
                if (def) def->def(as);
                u32 u = value.isInline ? value.payload : constants[value.payload].i;
                buf.write<u32>(with_endian<Target::endianness, u32>(u));
                break;
            }
            case Value::U64: {
                alignTo(8);
                if (def) def->def(as);
                u64 i = value.isInline ? value.payload : constants[value.payload].i;
                buf.write<u64>(with_endian<Target::endianness, u64>(i));
                break;
            }
            case Value::F32: {
                alignTo(4);
                if (def) def->def(as);
                f32 f = value.isInline ? bitcast<f32>(u32(value.payload) << 5) : constants[value.payload].f;
                buf.write<f32>(with_endian<Target::endianness, f32>(f));
                break;
            }
            case Value::F64: {
                alignTo(8);
                if (def) def->def(as);
                f64 f = value.isInline ? bitcast<f64>(u64(value.payload) << 37) : constants[value.payload].d;
                buf.write<f64>(with_endian<Target::endianness, f64>(f));
                break;
            }
            case Value::Ptr: {
                // Currently we're assuming pointers are 64-bit.
                alignTo(8);
                if (def) def->def(as);
                u64 i = value.isInline ? value.payload : constants[value.payload].i;
                buf.write<u64>(with_endian<Target::endianness, u64>(i));
                break;
            }
            case Value::Func: {
                alignTo(8);
                if (def) def->def(as);
                buf.write<u64>(0);
                Reloc::Kind relKind;
                if (Target::endianness == EndianOrder::LITTLE)
                    relKind = Reloc::ABS64_LE;
                else
                    relKind = Reloc::ABS64_BE;
                as.ref(CODE_SECTION, DEF_GLOBAL, relKind, Label::fromSym(value.sym));
                break;
            }
            case Value::Data: {
                alignTo(8);
                if (def) def->def(as);
                buf.write<u64>(0);
                Reloc::Kind relKind;
                if (Target::endianness == EndianOrder::LITTLE)
                    relKind = Reloc::ABS64_LE;
                else
                    relKind = Reloc::ABS64_BE;
                as.ref(DATA_SECTION, DEF_GLOBAL, relKind, Label::fromSym(value.sym));
                break;
            }
            case Value::Static: {
                alignTo(8);
                if (def) def->def(as);
                buf.write<u64>(0);
                Reloc::Kind relKind;
                if (Target::endianness == EndianOrder::LITTLE)
                    relKind = Reloc::ABS64_LE;
                else
                    relKind = Reloc::ABS64_BE;
                as.ref(STATIC_SECTION, DEF_GLOBAL, relKind, Label::fromSym(value.sym));
                break;
            }
            case Value::Array: {
                const Value* data = &words[value.ref] + 1;
                const auto& arrayType = mod->typeContext()[data[-1].type];
                alignTo(repr(arrayType.elementType()).alignment());
                if (def) def->def(as);
                if (arrayType.elementType() < 0 && data[-1].isSpecialized) switch (arrayType.elementType()) {
                    case I8:
                    case U8:
                        for (u32 i = 0; i < roundUpToNearest<u32>(arrayType.length(), 4); i ++)
                            buf.write<i8>(((i8*)data)[i]);
                        return;
                    case I16:
                    case U16:
                        for (u32 i = 0; i < roundUpToNearest<u32>(arrayType.length(), 2); i ++)
                            buf.write<i16>(with_endian<Target::endianness, i16>(((i16*)data)[i]));
                        return;
                    case I32:
                    case U32:
                    case F32:
                        for (u32 i = 0; i < arrayType.length(); i ++)
                            buf.write<i32>(with_endian<Target::endianness, i32>(((i32*)data)[i]));
                        return;
                    case I64:
                    case U64:
                    case F64:
                        for (u32 i = 0; i < arrayType.length(); i ++)
                            buf.write<i64>(with_endian<Target::endianness, i64>(load<i64>((i64*)data + i)));
                        return;
                    default:
                        unreachable("Unexpected element type ", TypeLogger { *mod, arrayType.elementType() }, " in specialized array data.");
                }
                for (u32 i = 0; i < arrayType.length(); i ++)
                    generateDataValue(mod, as, buf, data[i], none<DefInfo>());
                break;
            }
            case Value::Struct: {
                const Value* data = &words[value.ref] + 1;
                alignTo(repr(data[-1].type).alignment());
                if (def) def->def(as);
                const auto& structType = mod->typeContext()[data[-1].type];
                for (u32 i = 0; i < structType.fieldCount; i ++)
                    generateDataValue(mod, as, buf, data[i], none<DefInfo>());
                break;
            }
        }
    }

    template<typename Target>
    void TargetSpecificPasses<Target>::generateData(Module* mod, Assembly& as) {
        bytebuf& stat = as.stat;
        bytebuf& data = as.data;
        for (const auto& [k, v] : mod->values.staticDefs)
            generateDataValue(mod, as, stat, v, some<DefInfo>(STATIC_SECTION, k));
        for (const auto& [k, v] : mod->values.staticUninitDefs) {
            auto r = repr(v);
            while (stat.size() % r.alignment())
                stat.write<i8>(0);
            as.def(STATIC_SECTION, DEF_GLOBAL, Label::fromSym(k));
            for (u32 i = 0; i < r.size(); i ++)
                stat.write<i8>(0);
        }
        for (const auto& [k, v] : mod->values.dataDefs)
            generateDataValue(mod, as, data, v, some<DefInfo>(DATA_SECTION, k));
    }
}

#endif