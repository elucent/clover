#include "jasmine/mod.h"
#include "util/bits.h"
#include "util/config.h"
#include "util/heap.h"

namespace jasmine {
    Operand tryFold(PassContext& ctx, Function& fn, Block block, Node n) {
        auto& pins = *ctx.pins;

        #define CHECK_FOLDABLE_UNARY assert(!hasEffects(n.opcode())); if (pins.isPinned(n.operand(0).var) || !n.operand(1).isConst()) return fn.invalid()
        #define CHECK_FOLDABLE_BINARY assert(!hasEffects(n.opcode())); if (pins.isPinned(n.operand(0).var) || !n.operand(1).isConst() || !n.operand(2).isConst()) return fn.invalid()
        #define CHECK_FOLDABLE_BRANCH_COMPARE assert(!hasEffects(n.opcode())); if (pins.isPinned(n.operand(0).var) || !n.operand(0).isConst() || !n.operand(1).isConst()) return fn.invalid()
        #define PREFIX(op, lhs) op lhs
        #define POSTFIX(op, lhs) lhs op
        #define INFIX(op, lhs, rhs) lhs op rhs
        #define CALL_BINARY(op, lhs, rhs) op(lhs, rhs)
        #define CALL_UNARY(op, lhs) op(lhs)
        #define REPLACE_UNARY(type, action, op) return fn.constant<type>(action(op, fn.valueOf<type>(n.operand(1))));
        #define REPLACE_BINARY(type, action, op) return fn.constant<type>(action(op, fn.valueOf<type>(n.operand(1)), fn.valueOf<type>(n.operand(2))));
        #define REPLACE_COMPARE(type, action, op) return fn.constant<i64>(action(op, fn.valueOf<type>(n.operand(1)), fn.valueOf<type>(n.operand(2))));
        #define REPLACE_BRANCH_COMPARE(ty, action, op) { \
            bool cond = action(op, fn.valueOf<ty>(n.operand(0)), fn.valueOf<ty>(n.operand(1))); \
            n.setOpcode(Opcode::BR); \
            Operand taken = cond ? n.operand(2) : n.operand(3); \
            Operand notTaken = cond ? n.operand(3) : n.operand(2); \
            n.operand(0) = taken; \
            n.header().type = VOID; \
            fn.removeEdge(notTaken.edge); \
            n.setArity(1); \
            return taken; \
        }

        #define FOR_EACH_SINT(macro, ...) \
            case I8: macro(i8, __VA_ARGS__) break; \
            case I16: macro(i16, __VA_ARGS__) break; \
            case I32: macro(i32, __VA_ARGS__) break; \
            case I64: macro(i64, __VA_ARGS__) break;
        #define FOR_EACH_UINT(macro, ...) \
            case U8: macro(u8, __VA_ARGS__) break; \
            case U16: macro(u16, __VA_ARGS__) break; \
            case U32: macro(u32, __VA_ARGS__) break; \
            case U64: macro(u64, __VA_ARGS__) break; \
            case PTR: macro(uptr, __VA_ARGS__) break; \
            case REF: macro(uptr, __VA_ARGS__) break;
        #define FOR_EACH_INT(macro, ...) \
            FOR_EACH_SINT(macro, __VA_ARGS__) \
            FOR_EACH_UINT(macro, __VA_ARGS__)
        #define FOR_EACH_FLOAT(macro, ...) \
            case F32: macro(f32, __VA_ARGS__) break; \
            case F64: macro(f64, __VA_ARGS__) break;

        switch (n.opcode()) {
            case Opcode::MOV:
                CHECK_FOLDABLE_UNARY;
                return n.operand(1);
            case Opcode::ADD:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, +);
                    FOR_EACH_FLOAT(REPLACE_BINARY, INFIX, +);
                    default: break;
                }
                break;
            case Opcode::SUB:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, -);
                    FOR_EACH_FLOAT(REPLACE_BINARY, INFIX, -);
                    default: break;
                }
                break;
            case Opcode::MUL:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, *);
                    FOR_EACH_FLOAT(REPLACE_BINARY, INFIX, *);
                    default: break;
                }
                break;
            case Opcode::DIV:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, /);
                    FOR_EACH_FLOAT(REPLACE_BINARY, INFIX, /);
                    default: break;
                }
                break;
            case Opcode::REM:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, %);
                    FOR_EACH_FLOAT(REPLACE_BINARY, CALL_BINARY, frem);
                    default: break;
                }
                break;
            case Opcode::NEG:
                CHECK_FOLDABLE_UNARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_UNARY, PREFIX, -);
                    FOR_EACH_FLOAT(REPLACE_UNARY, PREFIX, -);
                    default: break;
                }
                break;
            case Opcode::AND:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, &);
                    default: break;
                }
                break;
            case Opcode::XOR:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, ^);
                    default: break;
                }
                break;
            case Opcode::OR:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, |);
                    default: break;
                }
                break;
            case Opcode::NOT:
                CHECK_FOLDABLE_UNARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_UNARY, PREFIX, ~);
                    default: break;
                }
                break;
            case Opcode::SHL:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, <<);
                    default: break;
                }
                break;
            case Opcode::SHR:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, INFIX, >>);
                    default: break;
                }
                break;
            case Opcode::ROL:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, CALL_BINARY, rol);
                    default: break;
                }
                break;
            case Opcode::ROR:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BINARY, CALL_BINARY, ror);
                    default: break;
                }
                break;
            case Opcode::MIN:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_FLOAT(REPLACE_BINARY, CALL_BINARY, min);
                    default: break;
                }
                break;
            case Opcode::MAX:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_FLOAT(REPLACE_BINARY, CALL_BINARY, max);
                    default: break;
                }
                break;
            case Opcode::ABS:
                CHECK_FOLDABLE_UNARY;
                switch (n.type()) {
                    FOR_EACH_FLOAT(REPLACE_UNARY, CALL_UNARY, abs);
                    default: break;
                }
                break;
            case Opcode::ROUND:
                CHECK_FOLDABLE_UNARY;
                switch (n.type()) {
                    FOR_EACH_FLOAT(REPLACE_UNARY, CALL_UNARY, round);
                    default: break;
                }
                break;
            case Opcode::FLOOR:
                CHECK_FOLDABLE_UNARY;
                switch (n.type()) {
                    FOR_EACH_FLOAT(REPLACE_UNARY, CALL_UNARY, floor);
                    default: break;
                }
                break;
            case Opcode::CEIL:
                CHECK_FOLDABLE_UNARY;
                switch (n.type()) {
                    FOR_EACH_FLOAT(REPLACE_UNARY, CALL_UNARY, ceil);
                    default: break;
                }
                break;
            case Opcode::IS_LT:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_COMPARE, INFIX, <);
                    FOR_EACH_FLOAT(REPLACE_COMPARE, INFIX, <);
                    default: break;
                }
                break;
            case Opcode::IS_LE:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_COMPARE, INFIX, <=);
                    FOR_EACH_FLOAT(REPLACE_COMPARE, INFIX, <=);
                    default: break;
                }
                break;
            case Opcode::IS_GT:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_COMPARE, INFIX, >);
                    FOR_EACH_FLOAT(REPLACE_COMPARE, INFIX, >);
                    default: break;
                }
                break;
            case Opcode::IS_GE:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_COMPARE, INFIX, >=);
                    FOR_EACH_FLOAT(REPLACE_COMPARE, INFIX, >=);
                    default: break;
                }
                break;
            case Opcode::IS_EQ:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_COMPARE, INFIX, ==);
                    FOR_EACH_FLOAT(REPLACE_COMPARE, INFIX, ==);
                    default: break;
                }
                break;
            case Opcode::IS_NE:
                CHECK_FOLDABLE_BINARY;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_COMPARE, INFIX, !=);
                    FOR_EACH_FLOAT(REPLACE_COMPARE, INFIX, !=);
                    default: break;
                }
                break;
            case Opcode::BR_IF:
                if (n.operand(0).isConst()) {
                    n.setOpcode(Opcode::BR);
                    Operand taken = fn.intValueOf(n.operand(0)) ? n.operand(1) : n.operand(2);
                    Operand notTaken = fn.intValueOf(n.operand(0)) ? n.operand(2) : n.operand(1);
                    n.operand(0) = taken;
                    n.header().type = VOID;
                    fn.removeEdge(notTaken.edge);
                    n.setArity(1);
                    return taken;
                }
                break;
            case Opcode::BR_IF_NOT:
                if (n.operand(0).isConst()) {
                    n.setOpcode(Opcode::BR);
                    Operand taken = !fn.intValueOf(n.operand(0)) ? n.operand(1) : n.operand(2);
                    Operand notTaken = !fn.intValueOf(n.operand(0)) ? n.operand(2) : n.operand(1);
                    n.operand(0) = taken;
                    n.header().type = VOID;
                    fn.removeEdge(notTaken.edge);
                    n.setArity(1);
                    return taken;
                }
                break;
            case Opcode::BR_LT:
                CHECK_FOLDABLE_BRANCH_COMPARE;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BRANCH_COMPARE, INFIX, <);
                    FOR_EACH_FLOAT(REPLACE_BRANCH_COMPARE, INFIX, <);
                    default: break;
                }
                break;
            case Opcode::BR_LE:
                CHECK_FOLDABLE_BRANCH_COMPARE;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BRANCH_COMPARE, INFIX, <=);
                    FOR_EACH_FLOAT(REPLACE_BRANCH_COMPARE, INFIX, <=);
                    default: break;
                }
                break;
            case Opcode::BR_GT:
                CHECK_FOLDABLE_BRANCH_COMPARE;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BRANCH_COMPARE, INFIX, >);
                    FOR_EACH_FLOAT(REPLACE_BRANCH_COMPARE, INFIX, >);
                    default: break;
                }
                break;
            case Opcode::BR_GE:
                CHECK_FOLDABLE_BRANCH_COMPARE;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BRANCH_COMPARE, INFIX, >=);
                    FOR_EACH_FLOAT(REPLACE_BRANCH_COMPARE, INFIX, >=);
                    default: break;
                }
                break;
            case Opcode::BR_EQ:
                CHECK_FOLDABLE_BRANCH_COMPARE;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BRANCH_COMPARE, INFIX, ==);
                    FOR_EACH_FLOAT(REPLACE_BRANCH_COMPARE, INFIX, ==);
                    default: break;
                }
                break;
            case Opcode::BR_NE:
                CHECK_FOLDABLE_BRANCH_COMPARE;
                switch (n.type()) {
                    FOR_EACH_INT(REPLACE_BRANCH_COMPARE, INFIX, !=);
                    FOR_EACH_FLOAT(REPLACE_BRANCH_COMPARE, INFIX, !=);
                    default: break;
                }
                break;
            default:
                break;
        }
        return fn.invalid();
    }

    void foldConstants(PassContext& ctx, Function& fn) {
        JASMINE_PASS(CONSTANT_FOLDING);
        ctx.require(SSA);

        vec<biasedset<128>> userNodes, userEdges;
        userNodes.expandTo(fn.variableList.size());
        userEdges.expandTo(fn.variableList.size());

        for (Node n : fn.nodes()) for (Operand operand : n.uses()) if (operand.kind == Operand::Var)
            userNodes[operand.var].on(n.index());

        for (Edge e : fn.edges()) for (Move& move : e.moves()) if (move.src.kind == Operand::Var)
            userEdges[move.src.var].on(e.index());

        bool fixpoint = false;
        bitset<256> wasFoldedOut;
        auto replace = [&](Operand var, Operand val) {
            for (i32 i : userNodes[var.var]) {
                Node node = fn.node(i);
                for (Operand& use : node.uses()) if (use == var)
                    use = val;
            }
            for (i32 i : userEdges[var.var]) {
                Edge edge = fn.edge(i);
                for (Move& move : edge.moves()) if (move.src == var)
                    move.src = val;
            }
            fixpoint = false;
            wasFoldedOut.on(var.var);
        };

        const auto& pins = ctx.pins.get();

        while (!fixpoint) {
            fixpoint = true;
            for (Block block : fn.blocks()) for (Node n : block.nodes()) {
                auto result = tryFold(ctx, fn, block, n);
                if (result.kind == Operand::Invalid)
                    continue;
                if (result.kind != Operand::Branch) { // We use branch operands to signify a control-flow-only change.
                    replace(n.operand(0), result);
                    n.nopify();
                }
                fixpoint = false;
            }
            for (Block block : fn.blocks()) {
                if (block.predecessorIndices().size() == 0 && block.index() != fn.entrypoint && block.successorIndices().size()) {
                    vec<EdgeIndex> edgesToRemove;
                    for (EdgeIndex e : block.successorIndices())
                        edgesToRemove.push(e);
                    for (EdgeIndex e : edgesToRemove)
                        fn.removeEdge(e);
                    for (Node n : block.nodes()) n.nopify();
                    fixpoint = false;
                    continue;
                }

                bitset<256> incoming, hasNonConstantValue;
                for (Edge edge : block.predecessors()) for (Move move : edge.moves()) if (!wasFoldedOut[move.dest.var])
                    incoming.on(move.dest.var);
                incoming.removeAll(hasNonConstantValue);
                for (u32 var : incoming) {
                    bool first = true;
                    Operand operand;
                    for (Edge edge : block.predecessors()) for (Move move : edge.moves()) if (move.dest.var == var) {
                        if (first && move.src.isConst())
                            operand = move.src;
                        else if (move.src.isConst()) {
                            if (move.src != operand)
                                goto fail;
                        } else if (!move.src.isConst())
                            goto fail;
                        first = false;
                    }
                    replace(fn.variableById(var), operand);
                }
                fail:;
            }

            for (Edge edge : fn.edges()) edge.removeMovesIf([&](const Move& move) -> bool {
                return wasFoldedOut[move.dest.var];
            });
        }
    }
}