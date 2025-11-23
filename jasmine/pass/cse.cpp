#include "jasmine/pass.h"
#include "jasmine/mod.h"

namespace jasmine {
    struct ValueNumbering;

    static Function* functionForComparison;
    static ValueNumbering* numberingForComparison;

    struct NodeHandle {
        NodeIndex node;
        i32 hash;

        inline NodeHandle(NodeIndex ni):
            node(ni), hash(0) {}

        inline bool operator==(const NodeHandle& other) const;
    };

    i64 hash(const NodeHandle& nh) {
        return nh.hash;
    }

    struct ValueNumbering {
        vec<i32, 16> numberings;
        vec<NodeIndex, 16> canonicalNodes;
        map<NodeHandle, i32>* tables[NUM_OPCODES];

        i32 number = 0;

        inline ValueNumbering() {
            for (u32 i = 0; i < NUM_OPCODES; i ++)
                tables[i] = nullptr;
        }

        i32 operator[](i32 var) const {
            return numberings[var];
        }

        i32& operator[](i32 var) {
            return numberings[var];
        }

        i32 next() {
            return number ++;
        }
    };

    inline bool NodeHandle::operator==(const NodeHandle& other) const {
        if (hash != other.hash)
            return false;
        Node ours = functionForComparison->node(node);
        Node theirs = functionForComparison->node(other.node);

        assert(ours.opcode() == theirs.opcode());
        if (ours.arity() != theirs.arity())
            return false;
        auto ourUses = ours.uses(), theirUses = theirs.uses();
        for (u32 i = 0; i < ourUses.size(); i ++) {
            if (ourUses[i].kind != theirUses[i].kind)
                return false;
            if (ourUses[i].kind == Operand::Var) {
                if (numberingForComparison->numberings[ourUses[i].var] != numberingForComparison->numberings[theirUses[i].var])
                    return false;
            } else if (ourUses[i] != theirUses[i])
                return false;
        }

        return true;
    }

    bool shouldCSE(Function& fn, Opcode opcode, TypeIndex type) {
        using enum Opcode;
        static constexpr Opcode opcodes[] = {
            BR_LT, BR_LE, BR_GT, BR_GE, BR_EQ, BR_NE, BR_IF, BR_IF_NOT,
            BR_ADD_O, BR_MUL_O, BR_SUB_O, STORE, STORE_FIELD, STORE_INDEX,
            LOAD, LOAD_FIELD, LOAD_INDEX, GET_FIELD, SET_FIELD, GET_INDEX,
            SET_INDEX, CALL, CALL_VOID,
        };

        constexpr u64 lowMask = makeMask(opcodes, 8, 0),
            highMask = makeMask(opcodes, 8, 64);

        if (opcode == MOV)
            return !isCompound(type) || isFunction(fn, type);
        if (((u32)opcode < 64 && (1ull << u32(opcode) & lowMask)) || ((u32)opcode >= 64 && (u32)opcode < 128 && (1ull << (u32(opcode) - 64) & highMask)))
            return false;
        return true;
    }

    i32 ensureNumbering(PassContext& ctx, Function& fn, ValueNumbering& numbering, i32 variable) {
        if (numbering[variable] != -1)
            return numbering[variable]; // Already computed.

        auto& defs = *ctx.defs;
        if (defs[variable] < 0) {
            numbering.canonicalNodes.push(-1);
            numbering[variable] = numbering.next(); // Defined by edge, meaning we don't try to CSE it. It gets a unique numbering.
            if UNLIKELY(config::verboseCSE)
                println("[CSE]\tVariable ", OperandLogger { fn, fn.variableById(variable) }, " is defined by an edge, numbered with ", numbering[variable]);
            return numbering[variable];
        }

        Node node = fn.node(defs[variable]);
        if (!shouldCSE(fn, node.opcode(), node.type())) {
            numbering.canonicalNodes.push(node.index());
            numbering[variable] = numbering.next(); // Any def that we don't want to CSE gets a unique numbering.
            if UNLIKELY(config::verboseCSE)
                println("[CSE]\tVariable ", OperandLogger { fn, fn.variableById(variable) }, " is defined by non-CSEable node ", node, ", numbered with ", numbering[variable]);
            return numbering[variable];
        }

        auto nh = NodeHandle(node.index());
        i64 h = intHash((u32)node.opcode());
        for (Operand o : node.uses()) switch (o.kind) {
            case Operand::Var:
                h = mixHash(h, intHash(ensureNumbering(ctx, fn, numbering, o.var)));
                break;
            case Operand::IntConst:
                h = mixHash(h, mixHash(intHash(o.kind), intHash(fn.intValueOf(o))));
                break;
            case Operand::F32Const:
                h = mixHash(h, mixHash(intHash(o.kind), intHash(bitcast<i32>(fn.f32ValueOf(o)))));
                break;
            case Operand::F64Const:
                h = mixHash(h, mixHash(intHash(o.kind), intHash(bitcast<i64>(fn.f64ValueOf(o)))));
                break;
            case Operand::Branch:
                h = mixHash(h, mixHash(intHash(o.kind), intHash(o.edge)));
                break;
            case Operand::Func:
            case Operand::Data:
            case Operand::Static:
            case Operand::String: // Do we need to include String here? Might not be necessary.
                h = mixHash(h, mixHash(intHash(o.kind), intHash(o.sym)));
                break;
            case Operand::Type:
            case Operand::Sizeof:
                h = mixHash(h, mixHash(intHash(o.kind), intHash(o.type)));
                break;
            case Operand::GP:
            case Operand::FP:
            case Operand::RegPair:
            case Operand::Memory:
            case Operand::Invalid:
                unreachable("Shouldn't be doing CSE on a register-allocated function.");
        }
        if UNLIKELY(!h)
            h = 1;

        auto& table = numbering.tables[(u32)node.opcode()];
        if (!table)
            table = new map<NodeHandle, i32>();
        auto it = table->find(nh);
        if (it == table->end()) {
            numbering.canonicalNodes.push(node.index());
            table->put(nh, numbering[variable] = numbering.next());
            if UNLIKELY(config::verboseCSE)
                println("[CSE]\tVariable ", OperandLogger { fn, fn.variableById(variable) }, " is defined by distinct node ", node, ", given new numbering ", numbering[variable]);
        } else {
            numbering[variable] = it->value;
            if UNLIKELY(config::verboseCSE)
                println("[CSE]\tVariable ", OperandLogger { fn, fn.variableById(variable) }, " defined by node ", node, ", given existing numbering ", numbering[variable], " (canonical node: ", fn.node(numbering.canonicalNodes[numbering[variable]]), ")");
        }

        return numbering[variable];
    }

    void eliminateCommonSubexpressions(PassContext& ctx, Function& fn) {
        JASMINE_PASS(CSE);
        ctx.require(SSA);

        if UNLIKELY(config::verboseCSE)
            println("[CSE]\tAnalyzing common subexpressions for function ", fn.name());

        ValueNumbering numbering;
        numbering.numberings.expandTo(fn.variableList.size(), -1);

        functionForComparison = &fn;
        numberingForComparison = &numbering;

        for (Block b : fn.blocks()) for (Node n : b.nodes()) if (hasDef(n.opcode()))
            ensureNumbering(ctx, fn, numbering, n.def(0).var);
        for (Edge edge : fn.edges()) for (Move move : edge.moves())
            ensureNumbering(ctx, fn, numbering, move.dest.var);

        if UNLIKELY(config::verboseCSE)
            println();
    }
}