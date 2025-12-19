#ifndef JASMINE_IR_H
#define JASMINE_IR_H

#include "asm/arch.h"
#include "jasmine/const.h"
#include "jasmine/type.h"
#include "util/bits.h"
#include "util/str.h"
#include "util/sort.h"
#include "util/config.h"

namespace jasmine {
    void ensureInited();

    #define FOR_EACH_OPCODE(macro) \
        /* No-op. */ \
        macro(NOP, nop) \
        \
        /* Simple value operations. */ \
        macro(MOV, mov) \
        macro(VAR, var) \
        \
        /* Arithmetic. */ \
        macro(ADD, add) \
        macro(SUB, sub) \
        macro(MUL, mul) \
        macro(DIV, div) \
        macro(REM, rem) \
        macro(NEG, neg) \
        macro(ABS, abs) \
        macro(MIN, min) \
        macro(MAX, max) \
        macro(SQRT, sqrt) \
        macro(ROUND, round) \
        macro(FLOOR, floor) \
        macro(CEIL, ceil) \
        \
        /* Bitwise operations. */ \
        macro(AND, and) \
        macro(OR, or) \
        macro(XOR, xor) \
        macro(NOT, not) \
        macro(SHL, shl) \
        macro(SHR, shr) \
        macro(ROL, rol) \
        macro(ROR, ror) \
        macro(LZCNT, lzcnt) \
        macro(TZCNT, tzcnt) \
        macro(POPCNT, popcnt) \
        \
        /* Comparisons. */ \
        macro(IS_LT, is_lt) \
        macro(IS_LE, is_le) \
        macro(IS_GT, is_gt) \
        macro(IS_GE, is_ge) \
        macro(IS_EQ, is_eq) \
        macro(IS_NE, is_ne) \
        macro(IS_INB, is_inb) \
        macro(IS_OOB, is_oob) \
        \
        /* Branches. */ \
        macro(BR, br) \
        macro(BR_IF, br_if) \
        macro(BR_IF_NOT, br_if_not) \
        macro(BR_LT, br_lt) \
        macro(BR_LE, br_le) \
        macro(BR_GT, br_gt) \
        macro(BR_GE, br_ge) \
        macro(BR_EQ, br_eq) \
        macro(BR_NE, br_ne) \
        macro(BR_INB, br_inb) \
        macro(BR_OOB, br_oob) \
        macro(BR_ADD_O, br_add_o) \
        macro(BR_SUB_O, br_sub_o) \
        macro(BR_MUL_O, br_mul_o) \
        \
        /* Aggregate value operations. */ \
        macro(PACK, pack) \
        macro(UNPACK, unpack) \
        macro(GET_FIELD, get_field) \
        macro(GET_INDEX, get_index) \
        macro(SET_FIELD, set_field) \
        macro(SET_INDEX, set_index) \
        \
        /* Memory operations. */ \
        macro(LOAD, load) \
        macro(LOAD_FIELD, load_field) \
        macro(LOAD_INDEX, load_index) \
        macro(STORE, store) \
        macro(STORE_FIELD, store_field) \
        macro(STORE_INDEX, store_index) \
        macro(ADDR, addr) \
        macro(ADDR_FIELD, addr_field) \
        macro(ADDR_INDEX, addr_index) \
        macro(OFFSET_FIELD, offset_field) \
        macro(OFFSET_INDEX, offset_index) \
        macro(PUSH, push) \
        macro(POP, pop) \
        macro(ALLOCA, alloca) \
        \
        /* Conversions. */ \
        macro(BITCAST, bitcast) \
        macro(CONVERT, convert) \
        \
        /* Functions. */ \
        macro(CALL, call) \
        macro(CALL_VOID, call_void) \
        macro(RET, ret) \
        \
        /* Misc. */ \
        macro(TRAP, trap) \
        macro(COMMENT, comment)

    #define DEFINE_ENUM_CASE(upper, ...) upper,
    enum class Opcode : i8 {
        FOR_EACH_OPCODE(DEFINE_ENUM_CASE)
    };
    #undef DEFINE_ENUM_CASE

    constexpr u32 NUM_OPCODES = u32(Opcode::COMMENT) + 1;

    extern const char* OPCODE_NAMES[NUM_OPCODES];
    extern const char* OPCODE_NAMES_UPPER[NUM_OPCODES];

    inline bool isExit(Opcode opcode) {
        switch (opcode) {
            case Opcode::RET:
                return true;
            default:
                return false;
        }
    }

    inline bool isCall(Opcode opcode) {
        switch (opcode) {
            case Opcode::CALL:
            case Opcode::CALL_VOID:
                return true;
            default:
                return false;
        }
    }

    inline bool hasEffects(Opcode opcode) {
        switch (opcode) {
            case Opcode::STORE:
            case Opcode::STORE_FIELD:
            case Opcode::STORE_INDEX:
            case Opcode::CALL:
            case Opcode::CALL_VOID:
            case Opcode::PUSH:
            case Opcode::POP:
            case Opcode::ALLOCA:
            case Opcode::TRAP:
                return true;
            default:
                return false;
        }
    }

    constexpr u64 makeMask(const Opcode* opcodes, u32 n, i32 bias) {
        u64 mask = 0;
        for (u32 i = 0; i < n; i ++) {
            i32 code = (i32)opcodes[i];
            if (code - bias >= 0 && code - bias < 64)
                mask |= 1ull << (code - bias);
        }
        return mask;
    }

    inline bool hasDef(Opcode opcode) {
        using enum Opcode;
        static constexpr Opcode opcodes[] = {
            BR, BR_IF, BR_IF_NOT,
            BR_LT, BR_LE, BR_GT, BR_GE, BR_EQ, BR_NE,
            BR_INB, BR_OOB,
            RET, CALL_VOID,
            NOP, TRAP, COMMENT,
            STORE, STORE_FIELD, STORE_INDEX,
            SET_FIELD, SET_INDEX
        };

        static constexpr u64
            lowMask = ~makeMask(opcodes, 21, 0),
            highMask = ~makeMask(opcodes, 21, 64);

        static_assert(NUM_OPCODES < 128);

        u32 val = u32(opcode);
        if (val < 64)
            return lowMask & 1ull << val;
        return highMask & 1ull << (val - 64);
    }

    inline bool isCompare(Opcode opcode) {
        using enum Opcode;

        static constexpr Opcode opcodes[] = {
            IS_LT, IS_LE, IS_EQ, IS_NE, IS_GT, IS_GE,
            IS_INB, IS_OOB
        };
        constexpr u64 lowMask = makeMask(opcodes, 8, 0),
            highMask = makeMask(opcodes, 8, 64);

        static_assert(!highMask);

        u32 val = u32(opcode);
        return val < 64 && lowMask & (1ull << val);
    }

    inline bool isBranch(Opcode opcode) {
        switch (opcode) {
            case Opcode::BR:
            case Opcode::BR_IF:
            case Opcode::BR_IF_NOT:
            case Opcode::BR_LT:
            case Opcode::BR_LE:
            case Opcode::BR_GT:
            case Opcode::BR_GE:
            case Opcode::BR_EQ:
            case Opcode::BR_NE:
            case Opcode::BR_INB:
            case Opcode::BR_OOB:
            case Opcode::BR_ADD_O:
            case Opcode::BR_SUB_O:
            case Opcode::BR_MUL_O:
                return true;
            default:
                return false;
        }
    }

    inline Opcode invert(Opcode opcode) {
        switch (opcode) {
            case Opcode::BR_IF: return Opcode::BR_IF_NOT;
            case Opcode::BR_IF_NOT: return Opcode::BR_IF;
            case Opcode::BR_LT: return Opcode::BR_GE;
            case Opcode::BR_LE: return Opcode::BR_GT;
            case Opcode::BR_GT: return Opcode::BR_LE;
            case Opcode::BR_GE: return Opcode::BR_LT;
            case Opcode::BR_EQ: return Opcode::BR_NE;
            case Opcode::BR_NE: return Opcode::BR_EQ;
            case Opcode::BR_INB: return Opcode::BR_OOB;
            case Opcode::BR_OOB: return Opcode::BR_INB;
            case Opcode::IS_LT: return Opcode::IS_GE;
            case Opcode::IS_LE: return Opcode::IS_GT;
            case Opcode::IS_GT: return Opcode::IS_LE;
            case Opcode::IS_GE: return Opcode::IS_LT;
            case Opcode::IS_EQ: return Opcode::IS_NE;
            case Opcode::IS_NE: return Opcode::IS_EQ;
            case Opcode::IS_INB: return Opcode::IS_OOB;
            case Opcode::IS_OOB: return Opcode::IS_INB;
            default: return opcode;
        }
    }

    inline bool isVariadic(Opcode opcode) {
        switch (opcode) {
            case Opcode::CALL:
            case Opcode::CALL_VOID:
            case Opcode::PACK:
            case Opcode::UNPACK:
                return true;
            default:
                return false;
        }
    }

    struct Operand {
        enum Kind : u32 {
            Var = 0, IntConst = 1, F32Const = 2, F64Const = 3,
            GP = 4, FP = 5, RegPair = 6, Memory = 7, Branch = 8,
            Func = 9, Data = 10, Static = 11, Type = 12,
            Sizeof = 13, String = 14,
            Invalid = 15
        };

        union {
            struct { Kind kind : 4; u32 var : 28; };
            struct { Kind : 4; i32 isInline : 1; i32 constant : 27; };
            struct { Kind : 4; TypeIndex regType : 20; i32 gp : 8; };
            struct { Kind : 4; u32 : 20; i32 fp : 8; };
            struct { Kind : 4; u32 : 12; i32 ra : 8, rb : 8; };
            struct { Kind : 4; u32 edge : 28; };
            struct { Kind : 4; i32 sym : 28; };
            struct { Kind : 4; i32 base : 8; i32 offset : 20; };
            struct { Kind : 4; TypeIndex type : 28; };
            u32 bits;
        };

        inline bool operator==(const Operand& other) const {
            return bits == other.bits;
        }

        inline bool operator!=(const Operand& other) const {
            return bits != other.bits;
        }

        inline bool isReg() const {
            return kind == GP || kind == FP;
        }

        inline bool isConst() const {
            return kind == IntConst || kind == F32Const || kind == F64Const;
        }

        inline bool isLabel() const {
            return kind == Func || kind == Data || kind == Static;
        }

        inline static Operand invalid() {
            Operand o;
            o.bits = 0;
            o.kind = Operand::Invalid;
            return o;
        }
    };

    struct NodeHeader {
        Opcode opcode;
        TypeKind type;
        u16 arity;
    };

    union NodeWord {
        NodeHeader header;
        Operand operand;
        TypeIndex type;
        u32 bits;
    };
    static_assert(sizeof(NodeWord) == 4);

    struct BlockHeader {
        u8 pred, succ;
        u16 index;
    };

    struct BlockDimensions {
        u16 length, capacity;
    };

    using EdgeIndex = u32;
    using BlockIndex = u32;
    using NodeIndex = u32;

    union BlockWord {
        BlockHeader header;
        BlockDimensions dimensions;
        EdgeIndex edge;
        NodeIndex node;
        u32 bits;
    };
    static_assert(sizeof(BlockWord) == 4);

    struct EdgeHeader {
        u16 src, dest;
        u16 length, capacity;
    };

    struct Move {
        Operand src, dest;

        inline bool operator==(const Move& other) const {
            return src == other.src && dest == other.dest;
        }

        inline bool operator!=(const Move& other) const {
            return src != other.src || dest != other.dest;
        }
    };

    union EdgeWord {
        EdgeHeader header;
        Move move;
        u64 bits;
    };
    static_assert(sizeof(EdgeWord) == 8);

    struct Function;

    enum class FunctionFlags : u32 {
        None = 0,
        Weak = 1
    };

    inline FunctionFlags operator|(FunctionFlags a, FunctionFlags b) {
        return FunctionFlags(u32(a) | u32(b));
    }

    inline FunctionFlags operator&(FunctionFlags a, FunctionFlags b) {
        return FunctionFlags(u32(a) & u32(b));
    }

    inline bool operator!(FunctionFlags a) {
        return !u32(a);
    }

    template<typename T>
    using IndexType = typename T::IndexType;

    template<typename T>
    struct Proxy {
        Function* function;
        IndexType<T>* ptr;

        inline Proxy& operator=(const T& t) {
            *ptr = t.index();
        }

        inline operator T() const;
    };

    template<typename T>
    struct BlockIterator {
        const Function* function;
        BlockIndex block;
        IndexType<T> i;

        inline T operator*() const;

        inline BlockIterator& operator++() {
            i ++;
            return *this;
        }

        inline BlockIterator operator++(int) {
            BlockIterator self = *this;
            operator++();
            return self;
        }

        inline bool operator==(const BlockIterator& other) const {
            return function == other.function && block == other.block && i == other.i;
        }

        inline bool operator!=(const BlockIterator& other) const {
            return function != other.function || block != other.block || i != other.i;
        }
    };

    template<typename T, u32 N>
    struct FunctionIterator {
        const Function* function;
        const vec<u32, N>* v;
        u32 i;

        inline T operator*() const;

        inline FunctionIterator& operator++() {
            i ++;
            return *this;
        }

        inline FunctionIterator operator++(int) {
            FunctionIterator self = *this;
            operator++();
            return self;
        }

        inline bool operator==(const FunctionIterator& other) const {
            return v == other.v && i == other.i;
        }

        inline bool operator!=(const FunctionIterator& other) const {
            return v != other.v || i != other.i;
        }
    };

    template<typename T, u32 N>
    struct FunctionView {
        const Function* function;
        const vec<u32, N>* v;

        FunctionIterator<T, N> begin() const {
            return { function, v, 0 };
        }

        FunctionIterator<T, N> end() const {
            return { function, v, v->size() };
        }
    };

    template<typename T>
    struct BlockView {
        const Function* function;
        BlockIndex block;
        u32 start;
        u32 length;

        BlockIterator<T> begin() const {
            return { function, block, start };
        }

        BlockIterator<T> end() const {
            return { function, block, start + length };
        }
    };

    struct Node {
        Function* function;
        u32 headidx;
        NodeIndex nodeidx;

        using IndexType = NodeIndex;
        using WordType = NodeWord;

        inline Node(Function* function_in, u32 headidx_in, NodeIndex nodeidx_in):
            function(function_in), headidx(headidx_in), nodeidx(nodeidx_in) {}

        inline const NodeHeader& header() const;
        inline NodeHeader& header();
        inline NodeIndex index() const;
        inline Opcode opcode() const;
        inline u32 arity() const;
        inline i32 base() const;
        inline u32 wordCount() const;
        inline void setOpcode(Opcode);
        inline void setArity(u16);

        inline TypeIndex type() const;
        inline Operand operand(u32 i) const;
        inline Operand& operand(u32 i);
        inline const_slice<Operand> operands() const;
        inline slice<Operand> operands();
        inline const_slice<Operand> uses() const;
        inline slice<Operand> uses();
        inline Operand use(u32 i) const;
        inline Operand& use(u32 i);
        inline const_slice<Operand> defs() const;
        inline slice<Operand> defs();
        inline Operand def(u32 i) const;
        inline Operand& def(u32 i);

        inline void nopify() {
            header().opcode = Opcode::NOP;
            header().arity = 0;
            header().type = VOID;
        }
    };

    struct Edge;

    struct Block {
        Function* function;
        u32 headidx;
        BlockIndex blockidx;

        static constexpr u32 defaultCapacity = 8;
        using IndexType = BlockIndex;
        using WordType = BlockWord;

        inline Block(Function* function_in, u32 headidx_in, BlockIndex blockidx_in):
            function(function_in), headidx(headidx_in), blockidx(blockidx_in) {}

        inline BlockHeader header() const;
        inline BlockHeader& header();
        inline const BlockDimensions& dims() const;
        inline BlockDimensions& dims();
        inline BlockIndex index() const;
        inline u32 length() const;
        inline BlockWord word(i32 i) const;
        inline BlockWord& word(i32 i);
        inline u32 wordCount() const;

        inline void grow();
        inline void growTo(u32 size);
        inline const_slice<EdgeIndex> predecessorIndices() const;
        inline slice<EdgeIndex> predecessorIndices();
        inline BlockView<Edge> predecessors() const;
        inline Edge predecessor(u32 i) const;
        inline void addPredecessor(Edge edge);
        inline void addPredecessor(EdgeIndex edge);
        inline void removePredecessor(Edge edge);
        inline void removePredecessor(EdgeIndex edge);
        inline void removeAllPredecessors();

        inline const_slice<EdgeIndex> successorIndices() const;
        inline slice<EdgeIndex> successorIndices();
        inline BlockView<Edge> successors() const;
        inline Edge successor(u32 i) const;
        inline void addSuccessor(Edge edge);
        inline void addSuccessor(EdgeIndex edge);
        inline void removeSuccessor(Edge edge);
        inline void removeSuccessor(EdgeIndex edge);
        inline void removeAllSuccessors();

        inline const_slice<NodeIndex> nodeIndices() const;
        inline slice<NodeIndex> nodeIndices();
        inline BlockView<Node> nodes() const;
        inline Node node(u32 i) const;
        inline Node first() const;
        inline Node last() const;
        inline void addNode(Node node);
        inline void replaceNode(u32 i, Node node);
        inline void shrinkTo(u32 i);

        template<typename Func>
        inline void removeIf(Func&& func);

        template<typename... Args>
        inline void addNode(Opcode opcode, Args... args);

        template<typename... Args>
        inline void replaceNode(u32 i, Opcode opcode, Args... args);
    };

    struct Edge {
        Function* function;
        u32 headidx;
        EdgeIndex edgeidx;

        static constexpr u32 defaultCapacity = 4;
        using IndexType = EdgeIndex;
        using WordType = EdgeWord;

        inline Edge(Function* function_in, u32 headidx_in, EdgeIndex edgeidx_in):
            function(function_in), headidx(headidx_in), edgeidx(edgeidx_in) {}

        inline EdgeHeader header() const;
        inline EdgeHeader& header();
        inline EdgeIndex index() const;
        inline u32 length() const;
        inline BlockIndex srcIndex() const;
        inline BlockIndex destIndex() const;
        inline Block src() const;
        inline Block dest() const;
        inline void setSrc(Block block);
        inline void setDest(Block block);
        inline u32 wordCount() const;

        inline void grow();
        inline const_slice<Move> moves() const;
        inline slice<Move> moves();
        inline void addMove(Operand src, Operand dest);
        inline void addMove(Move move);
        inline void removeMove(Move move);
        template<typename Pred>
        inline void removeMovesIf(Pred&& pred);
        inline void clearMoves();
    };

    struct Parameter {
        TypeIndex type;
        Operand operand;
    };

    struct Variable {
        slice<i8> name;
        i32 version = -1;
        i32 original = -1;
        TypeIndex type;
        i32 parameterIndex = -1;
    };

    struct VariableKey {
        union {
            struct { i8* text; i64 size; };
            struct { void* ptr; i64 id; };
        };

        inline static VariableKey Name(const i8* name) {
            VariableKey key;
            key.size = findc(name, 0);
            key.text = new i8[key.size];
            memory::copy(key.text, name, key.size);
            return key;
        }

        inline static VariableKey Name(const_slice<i8> name) {
            VariableKey key;
            key.size = name.size();
            key.text = new i8[key.size];
            memory::copy(key.text, name.data(), key.size);
            return key;
        }

        inline static VariableKey Id(i64 id) {
            VariableKey key;
            key.ptr = nullptr;
            key.id = id;
            return key;
        }

        inline bool operator==(const VariableKey& other) const {
            return (text && other.text && size == other.size && !memory::compare(text, other.text, size)) || (!text && !other.text && id == other.id);
        }

        inline static VariableKey PreinternedName(const_slice<i8> name) {
            VariableKey key;
            key.size = name.size();
            key.text = (i8*)name.data();
            return key;
        }
    };

    inline u64 hash(const VariableKey& key) {
        if (key.ptr)
            return ::hash(const_slice<i8>{ key.text, key.size });
        else
            return ::hash(key.id);
    }

    namespace FunctionMethods {
        // actually immensely stupid that the C++ standard requires this

        template<typename T>
        inline const_slice<typename T::WordType> storage(const Function&);
        template<typename T>
        inline slice<typename T::WordType> storage(Function&);

        template<typename T>
        inline const_slice<u32> indices(const Function& fn);
        template<typename T>
        inline slice<u32> indices(Function& fn);

        template<typename T>
        inline Operand constant(Function&, T);
        template<typename T>
        inline T valueOf(Function&, Operand);
    }

    struct Insertion {
        BlockIndex block;
        u32 indexInBlock;
        i32 offset;
        i32 length;
    };

    struct Function {
        Module* mod;
        Symbol sym;
        BlockIndex entrypoint;
        TypeIndex returnType;
        u32 indexInModule;
        vec<Parameter, 16> parameters;
        vec<u32, 16> nodeList;
        vec<u32, 8> blockList;
        vec<u32, 8> edgeList;
        vec<NodeWord, 64> nodeWords;
        vec<BlockWord, 32> blockWords;
        vec<EdgeWord, 16> edgeWords;
        ConstantTable* constants;
        map<VariableKey, i32> variables;
        vec<Variable, 8> variableList;
        bool addedAnything = false;
        bool isLeaf = true;
        bool hasAlloca = false;
        bool makesCalls = false;
        FunctionFlags flags;
        i64 nextTemp = 0;
        mutable u32 validatingCount = 0;
        vec<NodeIndex, 8> nodesToInsert;
        vec<Insertion, 8> insertions;
        vec<vec<u16, 12>, 4> blockInsertions;

        Function(Module& mod_in, Symbol sym_in, TypeIndex returnType_in, FunctionFlags flags_in);

        PREVENT_COPYING(Function);
        PREVENT_MOVING(Function);

        inline ~Function() {
            for (const auto& [k, v] : variables) if (k.text)
                delete[] k.text;
        }

        const TypeContext& typeContext() const;
        TypeContext& typeContext();

        template<typename TypeLike>
        inline Operand addParameter(TypeLike type) {
            assert(!addedAnything);
            Operand var = variable(parameters.size());
            variableList[var.var].parameterIndex = parameters.size();
            variableList[var.var].type = typeIndex(type);
            parameters.push({ typeIndex(type), var });
            assert(variableList.size() == parameters.size());
            return var;
        }

        template<typename TypeLike>
        inline Operand addParameter(TypeLike type, const i8* name) {
            assert(!addedAnything);
            Operand var = variable(name);
            variableList[var.var].parameterIndex = parameters.size();
            variableList[var.var].type = typeIndex(type);
            parameters.push({ typeIndex(type), var });
            assert(variableList.size() == parameters.size());
            return var;
        }

        template<typename TypeLike>
        inline Operand addParameter(TypeLike type, const_slice<i8> name) {
            assert(!addedAnything);
            Operand var = variable(name);
            variableList[var.var].parameterIndex = parameters.size();
            variableList[var.var].type = typeIndex(type);
            parameters.push({ typeIndex(type), var });
            assert(variableList.size() == parameters.size());
            return var;
        }

        inline void addParameters() {}

        template<typename TypeLike, typename... Args>
        inline void addParameters(TypeLike type, const i8* name, Args... args) {
            assert(!addedAnything);
            addParameter(type, name);
            addParameters(args...);
        }

        template<typename TypeLike, typename... Args>
        inline void addParameters(TypeLike type, const_slice<i8> name, Args... args) {
            assert(!addedAnything);
            addParameter(type, name);
            addParameters(args...);
        }

        template<typename TypeLike, typename... Args>
        inline void addParameters(TypeLike type, Args... args) {
            assert(!addedAnything);
            addParameter(type);
            addParameters(args...);
        }

        inline Block addBlock() {
            addedAnything = true;
            u32 blockidx = blockList.size();
            u32 headidx = blockWords.size();
            for (u32 i = 0; i < Block::defaultCapacity; i ++)
                blockWords.push({});
            blockList.push(headidx);
            blockInsertions.push({});
            Block bb { this, headidx, blockidx };
            bb.header().index = blockidx;
            bb.header().pred = bb.header().succ = 0;
            bb.dims().length = 2;
            bb.dims().capacity = Block::defaultCapacity;
            return bb;
        }

        inline Edge addEdge(Block& src, Block& dest) {
            Edge e = addEdge(src.index(), dest.index());
            src = block(src.index()), dest = block(dest.index());
            return e;
        }

        inline Edge addEdge(Block& src, BlockIndex dest) {
            Edge e = addEdge(src.index(), dest);
            src = block(src.index());
            return e;
        }

        inline Edge addEdge(BlockIndex src, Block& dest) {
            Edge e = addEdge(src, dest.index());
            dest = block(dest.index());
            return e;
        }

        inline Edge addEdge(BlockIndex src, BlockIndex dest) {
            addedAnything = true;
            u32 edgeidx = edgeList.size();
            u32 headidx = edgeWords.size();
            for (u32 i = 0; i < Edge::defaultCapacity; i ++)
                edgeWords.push({});
            edgeList.push(headidx);
            Edge e { this, headidx, edgeidx };
            e.header().src = src;
            e.header().dest = dest;
            e.header().length = 1;
            e.header().capacity = Edge::defaultCapacity;
            block(src).addSuccessor(e);
            block(dest).addPredecessor(e);
            return e;
        }

        inline void removeEdge(EdgeIndex e) {
            Edge edge = this->edge(e);
            edge.src().removeSuccessor(e);
            edge.dest().removePredecessor(e);
        }

        inline i32 computeArity() {
            return 0;
        }

        template<typename... Args>
        inline i32 computeArity(const const_slice<Operand>& operands, const Args&... args) {
            return operands.size() + computeArity(args...);
        }

        template<typename... Args>
        inline i32 computeArity(const Operand&, const Args&... args) {
            return 1 + computeArity(args...);
        }

        inline void addOperands() {}

        template<typename... Args>
        inline void addOperands(const const_slice<Operand>& operands, const Args&... args) {
            for (Operand operand : operands)
                nodeWords.push({ .operand = operand });
            addOperands(args...);
        }

        template<typename... Args>
        inline void addOperands(const Operand& operand, const Args&... args) {
            nodeWords.push({ .operand = operand });
            addOperands(args...);
        }

        inline void addOperands(NodeWord*) {}

        template<typename... Args>
        inline void addOperands(NodeWord* buffer, const const_slice<Operand>& operands, const Args&... args) {
            for (Operand operand : operands)
                *buffer ++ = { .operand = operand };
            addOperands(buffer, args...);
        }

        template<typename... Args>
        inline void addOperands(NodeWord* buffer, const Operand& operand, const Args&... args) {
            *buffer ++ = { .operand = operand };
            addOperands(buffer, args...);
        }

        inline Node addNode(Node node) {
            addedAnything = true;
            if (isCall(node.opcode()))
                isLeaf = false;
            Opcode opcode = node.opcode();
            TypeIndex type = node.type();
            const_slice<Operand> operands = node.operands();
            return addNode(opcode, type, operands);
        }

        template<typename... Args>
        inline Node addNode(Opcode opcode, TypeKind type, const Args&... args) {
            addedAnything = true;
            if (isCall(opcode))
                isLeaf = false;
            u32 headidx = nodeWords.size();
            u32 nodeidx = nodeList.size();
            i32 arity = computeArity(args...);
            assert(arity < 65536);
            assert(type != TypeKind::EXT);

            nodeList.push(headidx);
            nodeWords.push(NodeWord { .header = { opcode, type, u16(arity) } });
            addOperands(args...);

            return Node { this, headidx, nodeidx };
        }

        template<typename... Args>
        inline Node addNode(Opcode opcode, TypeIndex type, const Args&... args) {
            addedAnything = true;
            if (isCall(opcode))
                isLeaf = false;
            u32 headidx = nodeWords.size();
            u32 nodeidx = nodeList.size();
            i32 arity = computeArity(args...);
            assert(arity < 65536);

            nodeList.push(headidx);
            if (type >= 0) {
                nodeWords.push(NodeWord { .header = { opcode, TypeKind::EXT, u16(arity) } });
                nodeWords.push(NodeWord { .type = type });
            } else
                nodeWords.push(NodeWord { .header = { opcode, TypeKind(type), u16(arity) } });
            addOperands(args...);

            return Node { this, headidx, nodeidx };
        }

        template<typename... Args>
        inline void replaceNode(Node node, Opcode opcode, TypeIndex type, const Args&... args) {
            addedAnything = true;
            u32 headidx = nodeWords.size();
            i32 arity = computeArity(args...);
            i32 necessaryStorage = arity + 1 + (type >= 0 ? 1 : 0);
            i32 currentStorage = node.arity() + 1 + (node.type() >= 0 ? 1 : 0);
            if (necessaryStorage <= currentStorage) {
                u32 storage = node.headidx;
                if (type >= 0) {
                    nodeWords[storage ++] = NodeWord { .header = { opcode, TypeKind::EXT, u16(arity) }};
                    nodeWords[storage ++] = NodeWord { .type = type };
                } else
                    nodeWords[storage ++] = NodeWord { .header = { opcode, TypeKind(type), u16(arity) }};
                NodeWord* buffer = &nodeWords[storage];
                addOperands(buffer, args...);
            } else {
                // The new node exceeds the storage of the previous one, so we'll need to allocate
                // a new node and redirect the old one to the new storage.
                nodeList[node.index()] = headidx;
                if (type >= 0) {
                    nodeWords.push(NodeWord { .header = { opcode, TypeKind::EXT, u16(arity) } });
                    nodeWords.push(NodeWord { .type = type });
                } else
                    nodeWords.push(NodeWord { .header = { opcode, TypeKind(type), u16(arity) } });
                addOperands(args...);
            }
        }

        inline Node node(u32 i) const {
            assert(i >= 0 && i < nodeList.size());
            return Node { (Function*)this, nodeList[i], i };
        }

        inline Block block(u32 i) const {
            assert(i >= 0 && i < blockList.size());
            return Block { (Function*)this, blockList[i], i };
        }

        inline Edge edge(u32 i) const {
            assert(i >= 0 && i < edgeList.size());
            return Edge { (Function*)this, edgeList[i], i };
        }

        template<typename T>
        const_slice<typename T::WordType> storage() const { return FunctionMethods::storage<T>(*this); }
        template<typename T>
        slice<typename T::WordType> storage() { return FunctionMethods::storage<T>(*this); }

        template<typename T>
        const_slice<u32> indices() const { return FunctionMethods::indices<T>(*this); }
        template<typename T>
        slice<u32> indices() { return FunctionMethods::indices<T>(*this); }

        inline FunctionView<Edge, 8> edges() const {
            return FunctionView<Edge, 8> { this, &edgeList };
        }

        inline FunctionView<Block, 8> blocks() const {
            return FunctionView<Block, 8> { this, &blockList };
        }

        inline FunctionView<Node, 16> nodes() const {
            return FunctionView<Node, 16> { this, &nodeList };
        }

        inline Operand variable(const_slice<i8> name) {
            return variable(VariableKey::Name(name));
        }

        inline Operand variable(const i8* name) {
            return variable(VariableKey::Name(name));
        }

        inline Operand variable(i64 id) {
            nextTemp = max(id + 1, nextTemp);
            return variable(VariableKey::Id(id));
        }

        inline Operand variable() {
            Operand operand;
            operand.kind = Operand::Var;
            operand.var = variableList.size();

            Variable variable;
            variable.name = { nullptr, nextTemp ++ };
            variable.parameterIndex = -1;
            variable.type = TypeKind::UNDEFINED;
            variable.version = -1;
            variable.original = -1;
            variableList.push(variable);
            return operand;
        }

        inline i32 intern(VariableKey key) {
            auto it = variables.find(key);
            if (it != variables.end())
                return it->value;
            else {
                variables.put(key, variableList.size());
                Variable variable;
                if (key.ptr)
                    variable.name = { key.text, key.size };
                else
                    variable.name = { nullptr, key.id };
                variable.parameterIndex = -1;
                variable.type = TypeKind::UNDEFINED;
                variable.version = -1;
                variable.original = -1;
                variableList.push(variable);
                return variableList.size() - 1;
            }
        }

        inline Operand typeOperand(TypeIndex type) {
            Operand operand;
            operand.kind = Operand::Type;
            operand.type = type;
            return operand;
        }

        inline Operand sizeOf(TypeIndex type) {
            Operand operand;
            operand.kind = Operand::Sizeof;
            operand.type = type;
            return operand;
        }

        inline Operand stringOperand(Symbol sym) {
            Operand operand;
            operand.kind = Operand::String;
            operand.sym = sym;
            return operand;
        }

        inline Operand stringOperand(const_slice<i8> text) {
            return stringOperand(syms()[text]);
        }

        inline Operand variable(const Variable& var) {
            Operand operand;
            operand.kind = Operand::Var;
            operand.var = &var - &variableList[0];
            return operand;
        }

        inline Operand variable(VariableKey key) {
            Operand operand;
            operand.kind = Operand::Var;
            operand.var = intern(key);
            return operand;
        }

        inline Operand baseVariableOf(Operand var) {
        	while (variableList[var.var].original != -1)
        		var.var = variableList[var.var].original;
        	return var;
        }

        inline Operand newVersionOf(Operand base) {
            Operand operand;
            operand.kind = Operand::Var;
            operand.var = variableList.size();
            base = baseVariableOf(base);
            Variable newVar;
            Variable& oldVar = variableList[base.var];
            newVar.name = oldVar.name;
            newVar.type = oldVar.type;
            newVar.parameterIndex = -1;
            newVar.version = -(oldVar.version --);
            newVar.original = base.var;
            variableList.push(newVar);
            return operand;
        }

        inline Operand variableById(i32 id) {
            assert(id >= 0 && id < variableList.size());
            Operand operand;
            operand.kind = Operand::Var;
            operand.var = id;
            return operand;
        }

        inline Operand intConst(i64 i) {
            Operand operand;
            operand.kind = Operand::IntConst;
            if (i >= -0x04000000ll && i <= 0x03ffffffll) // Fits in i27, can be stored inline.
                operand.isInline = true, operand.constant = i;
            else
                operand.isInline = false, operand.constant = constants->intern(Constant::Int(i));
            return operand;
        }

        inline Operand f32Const(f32 f) {
            Operand operand;
            operand.kind = Operand::F32Const;
            i32 bits = bitcast<i32>(f);
            if (!(bits & 0b11111)) // Bottom 5 bits are all zero, so we can store the shifted constant.
                operand.isInline = true, operand.constant = bits >> 5;
            else
                operand.isInline = false, operand.constant = constants->intern(Constant::F32(f));
            return operand;
        }

        inline Operand f64Const(f64 f) {
            Operand operand;
            operand.kind = Operand::F64Const;
            i64 bits = bitcast<i64>(f);
            if (!(bits & 0x1fffffffffll)) // Bottom 37 bits are all zero, so we can store the shifted constant.
                operand.isInline = true, operand.constant = bits >> 37;
            else
                operand.isInline = false, operand.constant = constants->intern(Constant::F64(f));
            return operand;
        }

        template<typename T>
        inline Operand constant(T i) { return FunctionMethods::constant<T>(*this, i); }

        inline i64 intValueOf(Operand operand) const {
            assert(operand.kind == Operand::IntConst);
            if (operand.isInline)
                return operand.constant;
            else
                return (*constants)[operand.constant].i;
        }

        inline f32 f32ValueOf(Operand operand) const {
            assert(operand.kind == Operand::F32Const);
            if (operand.isInline)
                return bitcast<f32>(operand.constant << 5);
            else
                return (*constants)[operand.constant].f;
        }

        inline f64 f64ValueOf(Operand operand) const {
            assert(operand.kind == Operand::F64Const);
            if (operand.isInline)
                return bitcast<f64>(i64(operand.constant) << 37);
            else
                return (*constants)[operand.constant].d;
        }

        template<typename T>
        inline T valueOf(Operand i) { return FunctionMethods::valueOf<T>(*this, i); }

        inline Operand gp(mreg reg) {
            Operand operand;
            operand.bits = 0;
            operand.kind = Operand::GP;
            operand.gp = reg;
            return operand;
        }

        inline Operand fp(mreg reg) {
            Operand operand;
            operand.bits = 0;
            operand.kind = Operand::FP;
            operand.fp = reg;
            return operand;
        }

        inline Operand regPair(mreg a, mreg b) {
            Operand operand;
            operand.bits = 0;
            operand.kind = Operand::RegPair;
            operand.ra = a;
            operand.rb = b;
            return operand;
        }

        inline Operand branch(Block& src, Block& dest) {
            return branch(addEdge(src, dest));
        }

        inline Operand branch(Block& src, BlockIndex dest) {
            return branch(addEdge(src, dest));
        }

        inline Operand branch(BlockIndex src, Block& dest) {
            return branch(addEdge(src, dest));
        }

        inline Operand branch(BlockIndex src, BlockIndex dest) {
            return branch(addEdge(src, dest));
        }

        inline Operand branch(Edge edge) {
            return branch(edge.index());
        }

        inline Operand branch(EdgeIndex edge) {
            Operand operand;
            operand.kind = Operand::Branch;
            operand.edge = edge;
            return operand;
        }

        inline Operand memory(Operand base, i32 offset) {
            if (base.kind == Operand::GP)
                return memory(base.gp, offset);
            assert(base.kind == Operand::Memory);
            i32 netOffset = offset + base.offset;
            assert(netOffset >= -0x80000 && netOffset < 0x7ffff);
            Operand operand;
            operand.kind = Operand::Memory;
            operand.base = base.base;
            operand.offset = netOffset;
            return operand;
        }

        inline Operand memory(mreg base, i32 offset) {
            assert(offset >= -0x80000 && offset < 0x7ffff);
            Operand operand;
            operand.kind = Operand::Memory;
            operand.base = base;
            operand.offset = offset;
            return operand;
        }

        inline Operand symbolRef(Operand::Kind kind, Symbol name) {
            Operand operand;
            operand.kind = kind;
            operand.sym = name;
            return operand;
        }

        inline Operand invalid() const {
            Operand operand;
            operand.bits = 0;
            operand.kind = Operand::Invalid;
            return operand;
        }

        inline Operand func(Symbol name) {
            return symbolRef(Operand::Func, name);
        }

        inline Operand func(const_slice<i8> name) {
            return func(syms()[name]);
        }

        inline Operand func(const i8* name) {
            return func(syms()[name]);
        }

        inline Operand stat(Symbol name) {
            return symbolRef(Operand::Static, name);
        }

        inline Operand stat(const_slice<i8> name) {
            return stat(syms()[name]);
        }

        inline Operand stat(const i8* name) {
            return stat(syms()[name]);
        }

        inline Operand data(Symbol name) {
            return symbolRef(Operand::Data, name);
        }

        inline Operand data(const_slice<i8> name) {
            return data(syms()[name]);
        }

        inline Operand data(const i8* name) {
            return data(syms()[name]);
        }

        const TargetInterface& target() const;

        SymbolTable& syms();
        const SymbolTable& syms() const;

        inline const_slice<i8> name() const {
            return syms()[sym];
        }

        inline TypeIndex functionType() {
            auto b = typeContext().functionBuilder(this->returnType);
            for (const auto& p : parameters)
                b.addArgument(p.type);
            return b.build(typeContext());
        }

        inline void setEntrypoint(BlockIndex index) {
            assert(index >= 0 && index < blockList.size());
            entrypoint = index;
        }

        inline void setEntrypoint(Block block) {
            setEntrypoint(block.index());
        }

        void scheduleInPreorder(vec<BlockIndex>& indices) const;
        void scheduleInPostorder(vec<BlockIndex>& indices) const;
        void scheduleInReversePostorder(vec<BlockIndex>& indices) const;
        void scheduleTopologically(vec<BlockIndex>& indices) const;
        void validateAggressively() const;

        enum InsertionPoint {
            Early, Late
        };

        inline void addInsertion(Block block, u32 indexInBlock, InsertionPoint point = Early) {
            blockInsertions[block.index()].push(insertions.size());
            insertions.push({ block.index(), indexInBlock * 2 + (u32)point, (i32)nodesToInsert.size(), 0 });
        }

        inline void addNodeToInsertion(Node node) {
            nodesToInsert.push(node.index());
            insertions.back().length ++;
        }

        template<typename Func>
        void forEachInsertedNode(Func&& func) {
            for (NodeIndex n : nodesToInsert)
                func(node(n));
        }

        void executeInsertions();
    };

    inline bool isStruct(const Function& fn, TypeIndex type) {
        return isStruct(fn.typeContext(), type);
    }

    inline bool isVector(const Function& fn, TypeIndex type) {
        return isVector(fn.typeContext(), type);
    }

    inline bool isArray(const Function& fn, TypeIndex type) {
        return isArray(fn.typeContext(), type);
    }

    inline bool isFunction(const Function& fn, TypeIndex type) {
        return isFunction(fn.typeContext(), type);
    }

    namespace FunctionMethods {
        template<>
        inline const_slice<NodeWord> storage<Node>(const Function& fn) { return fn.nodeWords; }
        template<>
        inline slice<NodeWord> storage<Node>(Function& fn) { return fn.nodeWords; }

        template<>
        inline const_slice<BlockWord> storage<Block>(const Function& fn) { return fn.blockWords; }
        template<>
        inline slice<BlockWord> storage<Block>(Function& fn) { return fn.blockWords; }

        template<>
        inline const_slice<EdgeWord> storage<Edge>(const Function& fn) { return fn.edgeWords; }
        template<>
        inline slice<EdgeWord> storage<Edge>(Function& fn) { return fn.edgeWords; }

        template<>
        inline const_slice<u32> indices<Node>(const Function& fn) { return fn.nodeList; }
        template<>
        inline slice<u32> indices<Node>(Function& fn) { return fn.nodeList; }

        template<>
        inline const_slice<u32> indices<Block>(const Function& fn) { return fn.blockList; }
        template<>
        inline slice<u32> indices<Block>(Function& fn) { return fn.blockList; }

        template<>
        inline const_slice<u32> indices<Edge>(const Function& fn) { return fn.edgeList; }
        template<>
        inline slice<u32> indices<Edge>(Function& fn) { return fn.edgeList; }

        template<>
        inline Operand constant<i8>(Function& fn, i8 i) { return fn.intConst(i); }
        template<>
        inline Operand constant<i16>(Function& fn, i16 i) { return fn.intConst(i); }
        template<>
        inline Operand constant<i32>(Function& fn, i32 i) { return fn.intConst(i); }
        template<>
        inline Operand constant<i64>(Function& fn, i64 i) { return fn.intConst(i); }
        template<>
        inline Operand constant<u8>(Function& fn, u8 i) { return fn.intConst(i); }
        template<>
        inline Operand constant<u16>(Function& fn, u16 i) { return fn.intConst(i); }
        template<>
        inline Operand constant<u32>(Function& fn, u32 i) { return fn.intConst(i); }
        template<>
        inline Operand constant<u64>(Function& fn, u64 i) { return fn.intConst(i); }
        template<>
        inline Operand constant<f32>(Function& fn, f32 i) { return fn.f32Const(i); }
        template<>
        inline Operand constant<f64>(Function& fn, f64 i) { return fn.f64Const(i); }

        template<>
        inline i8 valueOf<i8>(Function& fn, Operand o) { return (i8)fn.intValueOf(o); }
        template<>
        inline i16 valueOf<i16>(Function& fn, Operand o) { return (i16)fn.intValueOf(o); }
        template<>
        inline i32 valueOf<i32>(Function& fn, Operand o) { return (i32)fn.intValueOf(o); }
        template<>
        inline i64 valueOf<i64>(Function& fn, Operand o) { return (i64)fn.intValueOf(o); }
        template<>
        inline u8 valueOf<u8>(Function& fn, Operand o) { return (u8)fn.intValueOf(o); }
        template<>
        inline u16 valueOf<u16>(Function& fn, Operand o) { return (u16)fn.intValueOf(o); }
        template<>
        inline u32 valueOf<u32>(Function& fn, Operand o) { return (u32)fn.intValueOf(o); }
        template<>
        inline u64 valueOf<u64>(Function& fn, Operand o) { return (u64)fn.intValueOf(o); }
        template<>
        inline f32 valueOf<f32>(Function& fn, Operand o) { return fn.f32ValueOf(o); }
        template<>
        inline f64 valueOf<f64>(Function& fn, Operand o) { return fn.f64ValueOf(o); }

        void preorderBlocks(vec<BlockIndex>& indices);
        void depthFirstBlocks(vec<BlockIndex>& indices);
    }

    template<typename T>
    inline Proxy<T>::operator T() const {
        return T { function, function->indices<T>()[*ptr], *ptr };
    }

    template<typename T>
    inline T BlockIterator<T>::operator*() const {
        auto id = function->block(block).word(i).bits;
        return T { (Function*)function, function->indices<T>()[id], id };
    }

    template<typename T, u32 N>
    inline T FunctionIterator<T, N>::operator*() const {
        return T { (Function*)function, (*v)[i], IndexType<T>(i) };
    }

    // Node method implementations.

    inline const NodeHeader& Node::header() const {
        return function->nodeWords[headidx].header;
    }

    inline NodeHeader& Node::header() {
        return function->nodeWords[headidx].header;
    }

    inline NodeIndex Node::index() const {
        return nodeidx;
    }

    inline Opcode Node::opcode() const {
        return header().opcode;
    }

    inline u32 Node::wordCount() const {
        return header().arity + (header().type == TypeKind::EXT ? 2 : 1);
    }

    inline u32 Node::arity() const {
        return header().arity;
    }

    inline TypeIndex Node::type() const {
        if (header().type != TypeKind::EXT)
            return TypeIndex(header().type);
        else
            return function->nodeWords[headidx + 1].type;
    }

    inline i32 Node::base() const {
        return headidx + (header().type == TypeKind::EXT ? 2 : 1);
    }

    inline Operand Node::operand(u32 i) const {
        return function->nodeWords[i + base()].operand;
    }

    inline Operand& Node::operand(u32 i) {
        return function->nodeWords[i + base()].operand;
    }

    inline const_slice<Operand> Node::operands() const {
        return { bitcast<const Operand*>(function->nodeWords.data() + base()), header().arity };
    }

    inline slice<Operand> Node::operands() {
        return { bitcast<Operand*>(function->nodeWords.data() + base()), header().arity };
    }

    inline const_slice<Operand> Node::defs() const {
        return { bitcast<const Operand*>(function->nodeWords.data() + base()), !!hasDef(opcode()) };
    }

    inline slice<Operand> Node::defs() {
        return { bitcast<Operand*>(function->nodeWords.data() + base()), !!hasDef(opcode()) };
    }

    inline Operand Node::def(u32 i) const {
        assert(hasDef(opcode()));
        assert(i == 0);
        return operand(0);
    }

    inline Operand& Node::def(u32 i) {
        assert(hasDef(opcode()));
        assert(i == 0);
        return operand(0);
    }

    inline const_slice<Operand> Node::uses() const {
        i32 offset = hasDef(opcode()) ? 1 : 0;
        return operands().drop(offset);
    }

    inline slice<Operand> Node::uses() {
        i32 offset = hasDef(opcode()) ? 1 : 0;
        return operands().drop(offset);
    }

    inline Operand Node::use(u32 i) const {
        return uses()[i];
    }

    inline Operand& Node::use(u32 i) {
        return uses()[i];
    }

    inline void Node::setOpcode(Opcode opcode) {
        header().opcode = opcode;
    }

    inline void Node::setArity(u16 arity) {
        header().arity = arity;
    }

    // Edge method implementations.

    inline EdgeHeader Edge::header() const {
        return function->edgeWords[headidx].header;
    }

    inline EdgeHeader& Edge::header() {
        return function->edgeWords[headidx].header;
    }

    inline EdgeIndex Edge::index() const {
        return edgeidx;
    }

    inline u32 Edge::length() const {
        return header().length;
    }

    inline u32 Edge::wordCount() const {
        return header().capacity;
    }

    inline BlockIndex Edge::srcIndex() const {
        return header().src;
    }

    inline BlockIndex Edge::destIndex() const {
        return header().dest;
    }

    inline Block Edge::src() const {
        return function->block(srcIndex());
    }

    inline Block Edge::dest() const {
        return function->block(destIndex());
    }

    inline void Edge::setSrc(Block block) {
        header().src = block.index();
    }

    inline void Edge::setDest(Block block) {
        header().dest = block.index();
    }

    inline void Edge::grow() {
        u32 oldHeader = headidx;
        u16 oldLength = header().length;
        u16 oldCapacity = header().capacity;
        headidx = function->edgeWords.size();
        i32 i = 0;
        for (i = 0; i < oldLength; i ++) {
            EdgeWord oldWord = function->edgeWords[oldHeader + i];
            function->edgeWords.push(oldWord);
        }
        for (; i < oldCapacity * 2; i ++)
            function->edgeWords.push({});
        header().capacity = oldCapacity * 2;
        function->edgeList[edgeidx] = headidx;
    }

    inline const_slice<Move> Edge::moves() const {
        return { &function->edgeWords[headidx + 1].move, header().length - 1 };
    }

    inline slice<Move> Edge::moves() {
        return { &function->edgeWords[headidx + 1].move, header().length - 1 };
    }

    inline void Edge::addMove(Move move) {
        if (header().length + 1 >= header().capacity)
            grow();
        function->edgeWords[headidx + header().length ++].move = move;
    }

    inline void Edge::addMove(Operand src, Operand dest) {
        addMove({ src, dest });
    }

    inline void Edge::clearMoves() {
        header().length -= moves().size();
    }

    inline void Edge::removeMove(Move move) {
        for (i32 i = 0; i < moves().size(); i ++) if (moves()[i] == move) {
            for (i32 j = i + 1; i < header().length - 1; i ++)
                moves()[j] = moves()[j + 1];
            header().length --;
            return;
        }
    }

    template<typename Pred>
    inline void Edge::removeMovesIf(Pred&& pred) {
        Move* writer = moves().begin();
        Move* reader = writer;
        Move* end = moves().end();
        while (reader != end) {
            if (!pred(*reader))
                *writer ++ = *reader;
            reader ++;
        }
        header().length -= reader - writer;
    }

    // Block method implementations.

    inline BlockHeader Block::header() const {
        return function->blockWords[headidx].header;
    }

    inline BlockHeader& Block::header() {
        return function->blockWords[headidx].header;
    }

    inline const BlockDimensions& Block::dims() const {
        return function->blockWords[headidx + 1].dimensions;
    }

    inline BlockDimensions& Block::dims() {
        return function->blockWords[headidx + 1].dimensions;
    }

    inline BlockIndex Block::index() const {
        return blockidx;
    }

    inline u32 Block::length() const {
        return dims().length;
    }

    inline u32 Block::wordCount() const {
        return dims().capacity;
    }

    inline BlockWord Block::word(i32 i) const {
        return function->blockWords[headidx + i];
    }

    inline BlockWord& Block::word(i32 i) {
        return function->blockWords[headidx + i];
    }

    inline void Block::grow() {
        growTo(dims().capacity * 2);
    }

    inline void Block::growTo(u32 newCapacity) {
        u32 oldHeader = headidx;
        u16 oldLength = dims().length;
        u16 oldCapacity = dims().capacity;
        if (dims().capacity >= newCapacity)
            return;
        headidx = function->blockWords.size();
        i32 i = 0;
        for (; i < oldLength; i ++) {
            BlockWord oldWord = function->blockWords[oldHeader + i];
            function->blockWords.push(oldWord);
        }
        for (; i < newCapacity; i ++)
            function->blockWords.push({});
        dims().capacity = newCapacity;
        function->blockList[blockidx] = headidx;
    }

    inline const_slice<EdgeIndex> Block::predecessorIndices() const {
        return { &function->blockWords[headidx + 2].edge, header().pred };
    }

    inline slice<EdgeIndex> Block::predecessorIndices() {
        return { &function->blockWords[headidx + 2].edge, header().pred };
    }

    inline BlockView<Edge> Block::predecessors() const {
        return BlockView<Edge> { function, index(), 2u, header().pred };
    }

    inline Edge Block::predecessor(u32 i) const {
        return function->edge(predecessorIndices()[i]);
    }

    inline Edge Block::successor(u32 i) const {
        return function->edge(successorIndices()[i]);
    }

    inline void Block::addPredecessor(Edge edge) {
        addPredecessor(edge.index());
    }

    inline void Block::addPredecessor(EdgeIndex edge) {
        for (EdgeIndex pred : predecessorIndices()) if (pred == edge)
            return;

        if (dims().length + 1 >= dims().capacity)
            grow();

        for (i32 i = dims().length; i > header().pred + 2; i --)
            word(i) = word(i - 1);
        word(header().pred ++ + 2).edge = edge;
        dims().length ++;
    }

    inline void Block::removePredecessor(Edge edge) {
        removePredecessor(edge.index());
    }

    inline void Block::removePredecessor(EdgeIndex edge) {
        for (auto [j, pred] : enumerate(predecessorIndices())) if (pred == edge) {
            for (i32 i = j + 2; i < dims().length - 1; i ++)
                word(i) = word(i + 1);
            dims().length --;
            header().pred --;
            return;
        }
    }

    inline void Block::removeAllPredecessors() {
        u32 pred = header().pred;
        for (i32 i = 2; i < dims().length - pred; i ++)
            word(i) = word(i + pred);
        dims().length -= pred;
        header().pred = 0;
    }

    inline const_slice<EdgeIndex> Block::successorIndices() const {
        return { &function->blockWords[headidx + 2 + header().pred].edge, header().succ };
    }

    inline slice<EdgeIndex> Block::successorIndices() {
        return { &function->blockWords[headidx + 2 + header().pred].edge, header().succ };
    }

    inline BlockView<Edge> Block::successors() const {
        return BlockView<Edge> { function, index(), 2u + header().pred, header().succ };
    }

    inline void Block::addSuccessor(Edge edge) {
        addSuccessor(edge.index());
    }

    inline void Block::addSuccessor(EdgeIndex edge) {
        for (EdgeIndex succ : successorIndices()) if (succ == edge)
            return;

        if (dims().length + 1 >= dims().capacity)
            grow();

        for (i32 i = dims().length; i > header().pred + header().succ + 2; i --)
            word(i) = word(i - 1);
        word(header().succ ++ + header().pred + 2).edge = edge;
        dims().length ++;
    }

    inline void Block::removeSuccessor(Edge edge) {
        removeSuccessor(edge.index());
    }

    inline void Block::removeSuccessor(EdgeIndex edge) {
        for (auto [j, succ] : enumerate(successorIndices())) if (succ == edge) {
            for (i32 i = header().pred + j + 2; i < dims().length - 1; i ++)
                word(i) = word(i + 1);
            dims().length --;
            header().succ --;
            return;
        }
    }

    inline void Block::removeAllSuccessors() {
        u32 succ = header().succ;
        for (i32 i = header().pred + 2; i < dims().length - succ; i ++)
            word(i) = word(i + succ);
        dims().length -= succ;
        header().succ = 0;
    }

    inline const_slice<NodeIndex> Block::nodeIndices() const {
        return { &function->blockWords[headidx + 2 + header().pred + header().succ].node, dims().length - header().pred - header().succ - 2 };
    }

    inline slice<NodeIndex> Block::nodeIndices() {
        return { &function->blockWords[headidx + 2 + header().pred + header().succ].node, dims().length - header().pred - header().succ - 2 };
    }

    inline BlockView<Node> Block::nodes() const {
        return BlockView<Node> { function, index(), 2u + header().pred + header().succ, dims().length - header().pred - header().succ - 2u };
    }

    inline Node Block::node(u32 i) const {
        return function->node(nodeIndices()[i]);
    }

    inline Node Block::first() const {
        return function->node(nodeIndices()[0]);
    }

    inline Node Block::last() const {
        return function->node(nodeIndices().last());
    }

    template<typename Func>
    inline void Block::removeIf(Func&& func) {
        auto indices = nodeIndices();
        NodeIndex* writer = nodeIndices().data();
        NodeIndex* reader = writer;
        for (u32 i = 0; i < indices.size(); i ++) {
            if (!func(function->node(*reader)))
                *writer ++ = *reader;
            ++ reader;
        }
        dims().length -= reader - writer;
    }

    inline void Block::addNode(Node node) {
        if (dims().length + 1 >= dims().capacity)
            grow();
        word(dims().length ++).node = node.index();
    }

    template<typename... Args>
    inline void Block::addNode(Opcode opcode, Args... args) {
        addNode(function->addNode(opcode, args...));
    }

    inline void Block::replaceNode(u32 i, Node node) {
        word(2 + header().pred + header().succ + i).node = node.index();
    }

    inline void Block::shrinkTo(u32 newSize) {
        dims().length = 2 + header().pred + header().succ + newSize;
    }

    template<typename... Args>
    inline void Block::replaceNode(u32 i, Opcode opcode, Args... args) {
        replaceNode(i, function->addNode(opcode, args...));
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Opcode& opcode) {
        return format(io, OPCODE_NAMES[(i32)opcode]);
    }

    struct OperandLogger {
        const Function& function;
        Operand operand;
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const OperandLogger& o) {
        bool isDOT = config::printJasmineDOTBeforeOpts || config::printJasmineDOTAfterOpts || config::printJasmineDOTBeforeEachPass;
        const i8* gray = isDOT ? "" : BLUE;
        const i8* reset = isDOT ? "" : RESET;
        switch (o.operand.kind) {
            case Operand::Var: {
                const Variable& var = o.function.variableList[o.operand.var];
                if (var.name.data())
                    io = format(io, "%", const_slice<i8>(var.name));
                else
                    io = format(io, "%", var.name.size());
                if (var.version > 0)
                    io = format(io, gray, "/", var.version, reset);
                return io;
            }
            case Operand::IntConst:
                return format(io, '#', o.function.intValueOf(o.operand));
            case Operand::F32Const:
                return format(io, '#', o.function.f32ValueOf(o.operand), "f32");
            case Operand::F64Const:
                return format(io, '#', o.function.f64ValueOf(o.operand), "f64");
            case Operand::Branch: {
                io = format(io, ".bb", o.function.edge(o.operand.edge).destIndex());
                Edge edge = o.function.edge(o.operand.edge);
                if (edge.moves().size() && !isDOT) {
                    io = format(io, " (");
                    bool first = true;
                    for (Move move : edge.moves()) {
                        if (!first) io = format(io, ", ");
                        first = false;
                        io = format(io, OperandLogger { o.function, move.src }, " => ", OperandLogger { o.function, move.dest });
                    }
                    io = format(io, ')');
                }
                return io;
            }
            case Operand::GP:
                return format(io, o.function.target().reg_name(o.operand.gp));
            case Operand::FP:
                return format(io, o.function.target().reg_name(o.operand.fp));
            case Operand::RegPair:
                return format(io, o.function.target().reg_name(o.operand.gp), ":", o.function.target().reg_name(o.operand.fp));
            case Operand::Memory:
                io = format(io, '[', o.function.target().reg_name(o.operand.base));
                if (o.operand.offset == 0)
                    return format(io, ']');
                else
                    return format(io, o.operand.offset < 0 ? " - " : " + ", abs(o.operand.offset), ']');
            case Operand::Func:
                return format(io, "func:", o.function.syms()[o.operand.sym]);
            case Operand::Data:
                return format(io, "data:", o.function.syms()[o.operand.sym]);
            case Operand::Static:
                return format(io, "static:", o.function.syms()[o.operand.sym]);
            case Operand::Type:
                return format(io, TypeLogger { o.function, o.operand.type });
            case Operand::Sizeof:
                return format(io, "sizeof ", TypeLogger { o.function, o.operand.type });
            case Operand::Invalid:
                return format(io, "INVALID");
            case Operand::String:
                return format(io, '#', o.function.syms()[o.operand.sym]);
            default:
                unreachable("Unexpected operand kind.");
        }
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Node& node) {
        if (node.opcode() == Opcode::COMMENT)
            return format(io, GRAY, "// ", node.function->syms()[node.operand(0).sym], RESET);
        io = format(io, OPCODE_NAMES[(i32)node.header().opcode], ' ', TypeLogger(*node.function->mod, node.type()));
        bool first = true;
        for (auto operand : node.operands()) {
            io = format(io, first ? " " : ", ");
            first = false;
            io = format(io, OperandLogger { *node.function, operand });
            if UNLIKELY(operand.kind == Operand::Type)
                first = true;
        }
        return io;
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Edge& e) {
        return io;
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Block& b) {
        io = format(io, ".bb", b.index(), ": ");
        const i8* gray = (config::printJasmineDOTBeforeOpts || config::printJasmineDOTAfterOpts || config::printJasmineDOTBeforeEachPass) ? "" : BLUE;
        const i8* reset = (config::printJasmineDOTBeforeOpts || config::printJasmineDOTAfterOpts || config::printJasmineDOTBeforeEachPass) ? "" : RESET;
        if (b.header().pred) {
            io = format(io, gray, "(");
            io = format(io, "pred: ");
            bool first = true;
            for (auto pred : b.predecessors()) {
                if (!first) io = format(io, ", ");
                first = false;
                io = format(io, "bb", pred.srcIndex());
            }
        }
        if (b.header().succ) {
            if (b.header().pred) io = format(io, "; ");
            else io = format(io, gray, "(");
            io = format(io, "succ: ");
            bool first = true;
            for (auto succ : b.successors()) {
                if (!first) io = format(io, ", ");
                first = false;
                io = format(io, "bb", succ.destIndex());
            }
        }
        if (b.header().pred || b.header().succ)
            io = format(io, ")", reset);
        io = format(io, '\n');

        for (auto node : b.nodes())
            io = format(io, "    ", node, '\n');
        return io;
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Function& f) {
        io = format(io, f.name(), '(');
        for (const auto& [i, p] : enumerate(f.parameters)) {
            if (i)
                io = format(io, ", ");
            io = format(io, OperandLogger { f, p.operand }, ": ", TypeLogger(*f.mod, p.type));
        }
        io = format(io, ") {\n");
        vec<BlockIndex> postorder;
        bitset<128> reachable;
        f.scheduleInReversePostorder(postorder);
        for (BlockIndex b : postorder)
            io = format(io, f.block(b)), reachable.on(b);
        for (Block b : f.blocks()) if (!reachable[b.index()])
            io = format(io, b);
        io = format(io, "}\n");
        return io;
    }

    template<typename Schedule>
    struct ScheduledFunction {
        const Schedule& schedule;
        const Function& fn;

        inline ScheduledFunction(const Schedule& schedule_in, const Function& fn_in):
            schedule(schedule_in), fn(fn_in) {}
    };

    template<typename IO, typename Schedule, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const ScheduledFunction<Schedule>& fn) {
        const auto& f = fn.fn;
        io = format(io, f.name(), '(');
        for (const auto& [i, p] : enumerate(f.parameters)) {
            if (i)
                io = format(io, ", ");
            io = format(io, OperandLogger { f, p.operand }, ": ", TypeLogger(*f.mod, p.type));
        }
        io = format(io, ") {\n");
        for (u32 i : fn.schedule)
            io = format(io, f.block(i));
        io = format(io, "}\n");
        return io;
    }
}

#endif
