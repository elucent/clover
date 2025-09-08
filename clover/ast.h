#ifndef CLOVER_AST_H
#define CLOVER_AST_H

#include "rt/def.h"
#include "util/hash.h"
#include "util/sym.h"
#include "util/vec.h"
#include "clover/compilation.h"
#include "clover/type.h"
#include "clover/lex.h"
#include "clover/scope.h"

namespace clover {
    #define FOR_EACH_AST_KIND(macro) \
        /* (uppercase, lowercase, symbol, isLeaf, arity) */ \
        \
        /* Terminals */ \
        macro(Local, local, _, true, 0) \
        macro(Capture, capture, _, true, 0) \
        macro(Const, const, _, true, 0) \
        macro(Typename, typename, _, true, 0) \
        macro(GenericTypename, genericTypename, _, true, 0) \
        macro(Global, global, _, true, 0) \
        macro(GlobalConst, globalConst, _, true, 0) \
        macro(GlobalTypename, globalTypename, _, true, 0) \
        macro(GlobalGenericTypename, globalGenericTypename, _, true, 0) \
        macro(Ident, ident, _, true, 0) \
        macro(Field, field, _, true, 0) \
        macro(Int, int, _, true, 0) \
        macro(Unsigned, unsigned, _, true, 0) \
        macro(Float, float, _, true, 0) \
        macro(String, string, _, true, 0) \
        macro(Bool, bool, _, true, 0) \
        macro(Char, char, _, true, 0) \
        macro(AnyType, anyType, any, true, 0) \
        macro(Wildcard, wildcard, wildcard, true, 0) /* Used in paths to indicate a glob. */ \
        macro(ResolvedFunction, resolvedFunction, resolved_function, true, 0) /* Encodes function index in symbol. */ \
        macro(Missing, missing, missing, true, 0) /* Used to mark missing expressions in nodes where some children are optional. */ \
        \
        /* Expressions */ \
        macro(Paren, paren, paren, false, 1) \
        macro(Add, add, +, false, 2) \
        macro(Sub, sub, -, false, 2) \
        macro(Mul, mul, *, false, 2) \
        macro(Div, div, /, false, 2) \
        macro(Rem, rem, %, false, 2) \
        macro(Exp, exp, **, false, 2) \
        macro(BitAnd, bitAnd, &, false, 2) \
        macro(BitOr, bitOr, |, false, 2) \
        macro(BitXor, bitXor, ^, false, 2) \
        macro(BitShl, bitShl, <<, false, 2) \
        macro(BitShr, bitShr, >>, false, 2) \
        macro(BitRol, bitRol, /<, false, 2) \
        macro(BitRor, bitRor, />, false, 2) \
        macro(And, and, and, false, 2) \
        macro(Or, or, or, false, 2) \
        macro(AddEq, addEq, +=, false, 2) \
        macro(SubEq, subEq, -=, false, 2) \
        macro(MulEq, mulEq, *=, false, 2) \
        macro(DivEq, divEq, /=, false, 2) \
        macro(RemEq, remEq, %=, false, 2) \
        macro(ExpEq, ExpEq, **=, false, 2) \
        macro(BitAndEq, bitAndEq, &=, false, 2) \
        macro(BitOrEq, bitOrEq, |=, false, 2) \
        macro(BitXorEq, bitXorEq, ^=, false, 2) \
        macro(BitShlEq, bitShlEq, <<=, false, 2) \
        macro(BitShrEq, bitShrEq, >>=, false, 2) \
        macro(BitRolEq, bitRolEq, /<=, false, 2) \
        macro(BitRorEq, bitRorEq, />=, false, 2) \
        macro(BitNot, bitNot, ~, false, 1) \
        macro(Not, not, not, false, 1) \
        macro(Less, less, <, false, 2) \
        macro(LessEq, lessEq, <=, false, 2) \
        macro(Greater, greater, >, false, 2) \
        macro(GreaterEq, greaterEq, >=, false, 2) \
        macro(Equal, equal, ==, false, 2) \
        macro(NotEqual, notEqual, !=, false, 2) \
        macro(Plus, plus, plus, false, 1) \
        macro(Minus, minus, minus, false, 1) \
        macro(Deref, deref, deref, false, 1) \
        macro(AddressOf, addressOf, address_of, false, 1) \
        macro(Assign, assign, =, false, 2) \
        macro(Store, store, store, false, 2) \
        macro(PreIncr, preIncr, pre_incr, false, 1) \
        macro(PreDecr, preDecr, pre_decr, false, 1) \
        macro(PostIncr, postIncr, post_incr, false, 1) \
        macro(PostDecr, postDecr, post_decr, false, 1) \
        macro(Range, range, .., false, 2) \
        macro(Length, length, length, false, 1) \
        macro(Is, is, is, false, 2) \
        macro(In, in, in, false, 2) \
        macro(FormatString, formatString, format_string, false, -1) \
        macro(WithRank, withRank, with_rank, false, 2) \
        macro(NamedParameter, namedParameter, named_parameter, false, 2) \
        macro(Stars, stars, stars, false, -1) /* Horrible syntax hack. It's lhs, rhs, then any number of 1s or 2s encoded as integers. */ \
        macro(New, new, new, false, 1) \
        macro(NewArray, newArray, new_array, false, 1) \
        macro(Del, del, del, false, 1) \
        \
        /* Aggregates */ \
        macro(Call, call, call, false, -1) \
        macro(Construct, construct, construct, false, -1) \
        macro(CallMethod, callMethod, call_method, false, -1) \
        macro(GetIndex, getIndex, get_index, false, 2) \
        macro(AddrIndex, addrIndex, addr_index, false, 2) \
        macro(SetIndex, setIndex, set_index, false, 3) \
        macro(GetIndices, getIndices, get_indices, false, -1) \
        macro(AddrIndices, addrIndices, addr_indices, false, -1) \
        macro(SetIndices, setIndices, set_indices, false, -1) \
        macro(GetField, getField, get_field, false, 2) \
        macro(AddrField, addrField, addr_field, false, 2) \
        macro(SetField, setField, set_field, false, 3) \
        macro(GetFields, getFields, get_fields, false, -1) \
        macro(AddrFields, addrFields, addr_fields, false, -1) \
        macro(SetFields, setFields, set_fields, false, -1) \
        macro(GetSlice, getSlice, get_slice, false, 3) \
        macro(SetSlice, setSlice, set_slice, false, 4) \
        macro(List, list, list, false, -1) \
        macro(Tuple, tuple, tuple, false, -1) \
        macro(Splat, splat, ..., false, 1) \
        \
        /* Type nodes */ \
        macro(ArrayType, arrayType, array_type, false, 2) \
        macro(SliceType, sliceType, slice_type, false, 1) \
        macro(PtrType, ptrType, ptr_type, false, 1) \
        macro(OwnType, ownType, own, false, 1) \
        macro(UninitType, uninitType, uninit, false, 1) \
        macro(GenericInst, genericInst, inst, false, -1) \
        macro(TupleType, tupleType, tuple_type, false, -1) \
        macro(FunType, funType, fun_type, false, -1) \
        macro(TypeField, typeField, type_field, false, 2) \
        \
        /* Declarations */ \
        macro(VarDecl, varDecl, var, false, 3) /* Type? Name Init? */ \
        macro(FunDecl, funDecl, fun, false, 5) /* Type? Name ArgsTuple Raises? Body */ \
        macro(GenericFunDecl, genericFunDecl, generic_fun, false, 5) /* Type? Link? ArgsTuple Raises? Body */ \
        macro(ConstVarDecl, constVarDecl, const_var, false, 3) \
        macro(ConstFunDecl, constFunDecl, const_fun, false, 5) \
        macro(AliasDecl, aliasDecl, alias, false, 2) \
        macro(NamedDecl, namedDecl, named, false, 2) /* Name Type? */ \
        macro(StructDecl, structDecl, struct, false, -1) /* Name Fields? */ \
        macro(UnionDecl, unionDecl, union, false, -1) /* Name Cases? */ \
        macro(NamedCaseDecl, namedCaseDecl, named_case, false, 2) \
        macro(StructCaseDecl, structCaseDecl, struct_case, false, -1) \
        macro(UnionCaseDecl, unionCaseDecl, union_case, false, -1) \
        macro(KindDecl, kindDecl, kind, false, -1) \
        macro(GenericTypeDecl, genericTypeDecl, generic_type, false, -1) \
        macro(UseModule, useModule, use_module, false, 1) \
        macro(UseType, useType, use_type, false, 1) \
        macro(As, as, as, false, 2) \
        macro(Export, export, export, false, 1) \
        \
        /* Control */ \
        macro(Do, do, do, false, -1) \
        macro(DoScoped, doScoped, do_scoped, false, -1) \
        macro(If, if, if, false, 2) \
        macro(IfElse, ifElse, if_else, false, 3) \
        macro(Ternary, ternary, ternary, false, 3) \
        macro(While, while, while, false, 2) \
        macro(For, for, for, false, 3) \
        macro(Then, then, then, false, 2) \
        macro(Break, break, break, false, 0) \
        macro(Continue, continue, continue, false, 0) \
        macro(Return, return, return, false, 1) \
        macro(Raise, raise, raise, false, 1) \
        macro(On, on, on, false, 2) \
        macro(Case, case, case, false, 2) \
        macro(Match, match, match, false, -1) \
        macro(Defer, defer, defer, false, 1) \
        macro(TopLevel, toplevel, toplevel, false, -1)

    struct Arity {
        u32 variadic : 1;
        u32 arityIfNotVariadic : 31;

        bool isVariadic() const {
            return variadic;
        }

        u32 arity() const {
            assert(!isVariadic());
            return arityIfNotVariadic;
        }

        static Arity Variadic() {
            return { .variadic = true, .arityIfNotVariadic = 0 };
        }

        static Arity Of(u32 arity) {
            return { .variadic = false, .arityIfNotVariadic = arity };
        }
    };

    struct ASTWord {
        #define DEFINE_ENUM(upper, ...) upper,
        enum Kind : u32 {
            FOR_EACH_AST_KIND(DEFINE_ENUM)
            NumKinds,
            LastLocal = GenericTypename,
            LastGlobal = GlobalGenericTypename,
            LastTerminal = Missing,
            FirstScoped = VarDecl,
            FirstDecl = VarDecl,
            LastDecl = FunDecl,
            Ref = 255
        };
        #undef DEFINE_ENUM

        constexpr static u32 KindBits = 8;
        constexpr static u32 MaxKinds = (1 << KindBits) - 1;
        static_assert(NumKinds < MaxKinds);

        constexpr static u32 MetaBits = 0;
        constexpr static u32 PayloadBits = 32 - KindBits;
        constexpr static u32 ConstantBits = PayloadBits - 1; // 1 bit to store whether it's inline or not.
        constexpr static u32 ArityBits = 24; // At least 24 bits to store arity.
        constexpr static u32 IndexBits = PayloadBits; // We could probably shuffle things around to widen this a bit. But 2^24 nodes per module seems approximately reasonable.

        static_assert(1 << PayloadBits >= Limits::SymbolsPerCompilation);
        static_assert(1 << PayloadBits >= Limits::IdsPerCompilation);
        static_assert(1 << ConstantBits >= Limits::ConstantsPerFunction);
        static_assert(1 << ArityBits >= Limits::MaxChildrenPerNode);

        union {
            struct { u32 ref : KindBits; NodeIndex child : PayloadBits; };
            struct { Kind kind : KindBits; u32 symbol : PayloadBits; };
            struct { Kind : KindBits; UniqueId id : PayloadBits; };
            struct { Kind : KindBits; u32 constantIndex : ConstantBits; u32 isInline : 1; };
            struct { Kind : KindBits; i32 inlineSigned : ConstantBits; u32 : 1; };
            struct { Kind : KindBits; u32 inlineUnsigned : ConstantBits; u32 : 1; };
            struct { Kind : KindBits; u32 arity : ArityBits; };
            u32 bits;
        };

        inline ASTWord() = default;

        inline static ASTWord fromBits(u32 bits) {
            ASTWord word;
            word.bits = bits;
            return word;
        }

        inline constexpr static bool isLeaf(Kind kind) {
            return u8(kind) <= u8(LastTerminal);
        }

        inline bool isLeaf() const {
            return isLeaf(kind);
        }

        inline bool isRef() const {
            return kind == Kind::Ref;
        }

        inline void makeRef(u32 node) {
            kind = Kind::Ref;
            child = node;
        }

        inline static ASTWord makeRefOf(u32 node) {
            ASTWord word;
            word.makeRef(node);
            return word;
        }

        inline constexpr static bool hasEnv(Kind kind) {
            return u8(kind) >= u8(FirstScoped);
        }

        inline bool hasEnv() const {
            return hasEnv(kind);
        }
    };

    using ASTKind = ASTWord::Kind;

    extern const i8* AST_NAMES_UPPER[ASTKind::NumKinds];
    extern const i8* AST_NAMES_LOWER[ASTKind::NumKinds];
    extern const i8* AST_NAMES_SYMBOLIC[ASTKind::NumKinds];

    struct VariableInfo;
    struct Function;

    struct AST {
        static constexpr u32 HeaderSlots = 1;
        static constexpr u32 HeaderSlot = 0;

        Module* module;
        NodeIndex node;
        ASTWord word;

        inline AST():
            module(nullptr), node(0) {}

        inline AST(Module* module_in, NodeIndex node_in, u32 word_in):
            module(module_in), node(node_in), word(ASTWord::fromBits(word_in)) {}

        inline AST(Module* module_in, ASTWord inlineWord):
            module(module_in), node(InvalidNode), word(inlineWord) {}

        inline ASTWord& firstWord();
        inline ASTWord firstWord() const;
        inline ASTWord& nthWord(u32 i);
        inline ASTWord nthWord(u32 i) const;
        inline ASTWord& refWord(u32 i);
        inline ASTWord refWord(u32 i) const;
        inline ASTWord asOperand() const;
        inline ASTKind kind() const;
        inline void setKind(ASTKind kind);
        inline u32 arity() const;
        inline void setArity(u32 newArity);
        inline i64 intConst() const;
        inline u64 uintConst() const;
        inline u32 fieldId() const;
        inline Function* resolvedFunction() const;
        inline f64 floatConst() const;
        inline Symbol stringConst() const;
        inline bool boolConst() const;
        inline u32 charConst() const;
        inline void removeChild(u32 i);
        inline void trimChildrenTo(u32 newArity);
        inline void setIntConst(i64 i);
        inline void setUintConst(u64 i);
        inline void setFieldId(u32 i);
        inline void setResolvedFunction(Function* function);
        inline void setFloatConst(f64 f);
        inline void setStringConst(Symbol s);
        inline void setBoolConst(bool b);
        inline void setCharConst(u32 c);
        inline Symbol symbol() const;
        inline void setSymbol(Symbol s);
        inline u32 variable() const;
        inline bool isLocal() const;
        inline bool isGlobal() const;
        inline bool isVariable() const;
        inline bool missing() const;
        inline const VariableInfo& varInfo() const;
        inline VariableInfo& varInfo();
        inline const VariableInfo& varInfo(Function* function) const;
        inline VariableInfo& varInfo(Function* function);
        inline Function* function() const;
        inline void setVariable(u32 var);
        inline AST child(u32 i) const;
        inline void setChild(u32 i, AST node);
        inline Pos pos() const;
        inline void setPos(Pos pos);
        inline Type type() const;
        inline Type type(Module* module) const;
        inline Type type(Function* function) const;
        inline TypeIndex typeIndex() const;
        inline TypeIndex typeIndex(Module* module) const;
        inline TypeIndex typeIndex(Function* function) const;
        inline explicit operator bool() const {
            return module;
        }

        template<TypeKind K>
        inline HandleType<K> type() const {
            return this->type().as<K>();
        }

        template<TypeKind K>
        inline HandleType<K> type(Module* module) const {
            return this->type(module).as<K>();
        }

        template<TypeKind K>
        inline HandleType<K> type(Function* function) const {
            return this->type(function).as<K>();
        }

        inline void setType(TypeIndex t);
        inline void setType(Type t);
        inline Scope* scope() const;
        inline void setScope(Scope* scope);

        inline bool isLeaf() const {
            return firstWord().isLeaf();
        }

        inline bool isRef() const {
            return firstWord().isRef();
        }

        struct Iterator {
            Module* module;
            u32 word;

            inline Iterator(Module* module_in, u32 word_in):
                module(module_in), word(word_in) {}

            inline AST operator*() const;

            inline bool operator==(const Iterator& other) const {
                return module == other.module && word == other.word;
            }

            inline bool operator!=(const Iterator& other) const {
                return module != other.module || word != other.word;
            }

            inline Iterator& operator++() {
                word ++;
                return *this;
            }

            inline Iterator operator++(i32) {
                Iterator i = *this;
                word ++;
                return i;
            }
        };

        Iterator begin() const {
            if (firstWord().isLeaf())
                return Iterator(module, word.bits);
            else
                return Iterator(module, word.bits + HeaderSlots);
        }

        Iterator end() const {
            if (firstWord().isLeaf())
                return Iterator(module, word.bits);
            else
                return Iterator(module, word.bits + HeaderSlots + arity());
        }

        Span<Iterator> children(u32 startIndex) const {
            assert(!firstWord().isLeaf());
            assert(startIndex <= arity());
            return { Iterator(module, word.bits + HeaderSlots + startIndex), end() };
        }

        Span<Iterator> children(u32 startIndex, u32 endIndex) const {
            assert(!firstWord().isLeaf());
            assert(endIndex <= arity());
            assert(startIndex <= endIndex);
            return { Iterator(module, word.bits + HeaderSlots + startIndex), Iterator(module, word.bits + HeaderSlots + endIndex) };
        }

        template<typename... Args>
        inline void replace(ASTKind kind, const Args&... args);
    };

    static_assert(sizeof(ASTWord) == 4);

    struct Identifier {
        Symbol name;

        inline Identifier(u32 symbol):
            name(symbol) {}

        inline Identifier(Symbol symbol):
            name(symbol) {}
    };

    struct Function {
        Module* module;
        Function* parent;
        NodeIndex decl;
        u32 index;
        TypeIndex typeIndex = InvalidType;
        Symbol name, mangledName = InvalidSymbol;
        vec<VariableInfo, 8> locals;
        u32 numTemps = 0;
        bool isGeneric = false;
        vec<TypeIndex, 4> typeParameters;

        inline Function(Module* module_in, Function* parent_in, NodeIndex decl_in);

        inline Local addLocalOverload(u32 overloads, Symbol name) {
            VariableInfo info;
            info.type = InvalidType;
            info.scope = VariableInfo::Local;
            info.kind = VariableKind::OverloadedFunction;
            info.name = name;
            info.overloads = overloads;
            locals.push(info);
            return Local(locals.size() - 1);
        }

        inline Local addLocal(VariableKind kind, AST decl, Symbol name) {
            return addLocal(kind, decl, InvalidType, name);
        }

        inline Local addLocal(VariableKind kind, AST decl, TypeIndex type, Symbol name) {
            VariableInfo info;
            info.type = type;
            info.scope = VariableInfo::Local;
            info.kind = kind;
            info.decl = decl.node;
            info.name = name;
            locals.push(info);
            return Local(locals.size() - 1);
        }

        inline Local addLocalFunctionImport(VariableKind kind, Function* function);

        inline Local addLocalImport(VariableKind kind, TypeIndex type, Symbol name) {
            assert(kind != VariableKind::Function);

            VariableInfo info;
            info.kind = kind;
            info.scope = VariableInfo::Local;
            info.isImport = true;
            info.functionIndex = InvalidNode;
            info.type = type;
            info.name = name;
            locals.push(info);
            return Local(locals.size() - 1);
        }

        inline Local addLocalIndirect(ScopeIndex scope, u32 index) {
            VariableInfo info;
            info.type = InvalidType;
            info.decl = InvalidNode;
            info.kind = VariableKind::Forward;
            info.scope = VariableInfo::Local;
            info.defScope = scope;
            info.index = index;
            locals.push(info);
            return Local(locals.size() - 1);
        }

        inline Local addTemp(TypeIndex type);

        inline Local addTemp() {
            return addTemp(InvalidType);
        }

        inline Type type() const;
    };

    struct Overloads {
        struct Member {
            uptr p;

            bool isFunction() const {
                return (p & 1) == 0;
            }

            bool isOverloads() const {
                return (p & 1);
            }

            Function* function() {
                assert(isFunction());
                return (Function*)(p & ~1ull);
            }

            Overloads* overloads() {
                assert(isOverloads());
                return (Overloads*)(p & ~1ull);
            }
        };
        u32 index;
        vec<Member, 8> functions;

        inline void add(Function* function) {
            functions.push(Member { uptr(function) });
        }

        inline void add(Overloads* overloads) {
            functions.push(Member { uptr(overloads) | 1ull });
        }

        template<typename Func>
        inline void forEachFunction(Func&& func) {
            for (Member m : functions) {
                if (m.isFunction())
                    func(m.function());
                else
                    m.overloads()->forEachFunction(func);
            }
        }
    };

    struct VariableHandle {
        union {
            Module* module;
            Function* function;
        };
        bool g;
        u32 i;

        inline VariableHandle():
            module(nullptr), i(0) {}

        inline VariableHandle(Module* module_in, u32 index_in):
            module(module_in), g(true), i(index_in) {}

        inline VariableHandle(Function* function_in, u32 index_in):
            function(function_in), g(false), i(index_in) {}

        inline operator bool() const {
            return module;
        }

        inline VariableHandle expand() const;
        inline bool isGlobal() const { return g; }
        inline u32 index() const { return i; }
        inline VariableKind kind() const;
        inline TypeIndex type() const;
        inline void setType(Type type);
        inline void setType(TypeIndex type);
        inline Symbol name() const;
        inline bool hasDecl() const;
        inline AST decl() const;
    };

    struct FieldId {
        u32 id;

        inline FieldId(u32 id_in): id(id_in) {}
    };

    struct Constant {
        union {
            i64 intConst;
            u64 uintConst;
            f64 floatConst;
            u32 stringConst;
            bool boolConst;
            u32 charConst;
        };
        enum Kind {
            Int, Unsigned, Float, String, Bool, Char
        };
        Kind kind;

        inline static Constant IntConst(i64 i) {
            return { .intConst = i, .kind = Int };
        }

        inline static Constant UnsignedConst(u64 i) {
            return { .uintConst = i, .kind = Unsigned };
        }

        inline static Constant FloatConst(f64 f) {
            return { .floatConst = f, .kind = Float };
        }

        inline static Constant StringConst(Symbol s) {
            return { .stringConst = s.symbol, .kind = String };
        }

        inline static Constant BoolConst(bool b) {
            return { .boolConst = b, .kind = Bool };
        }

        inline static Constant CharConst(u32 c) {
            return { .charConst = c, .kind = Char };
        }

        inline bool operator==(const Constant& other) const {
            if (kind != other.kind)
                return false;
            switch (kind) {
                case Int:
                    return intConst == other.intConst;
                case Unsigned:
                    return uintConst == other.uintConst;
                case Float:
                    return floatConst == other.floatConst;
                case String:
                    return stringConst == other.stringConst;
                case Bool:
                    return boolConst == other.boolConst;
                case Char:
                    return charConst == other.charConst;
            }
        }

        inline bool operator!=(const Constant& other) const {
            return !operator==(other);
        }
    };

    inline u64 hash(const Constant& constant) {
        u64 h = ::hash(i64(constant.kind)) * 4303321134630290291ull;
        switch (constant.kind) {
            case Constant::Int:
                return h + ::hash(constant.intConst);
            case Constant::Unsigned:
                return h + ::hash(constant.uintConst);
            case Constant::Float:
                return h + ::hash(bitcast<u64>(constant.floatConst));
            case Constant::String:
                return h + ::hash(constant.stringConst);
            case Constant::Bool:
                return h + ::hash((u32)constant.boolConst);
            case Constant::Char:
                return h + ::hash(constant.charConst);
        }
    }

    struct InlineResult {
        u32 canIntern : 1;
        i32 payload : ASTWord::ConstantBits;
    };

    inline u64 hash(Function* function) {
        return ::hash(u64(function));
    }

    struct Module : public ArtifactData {
        vec<ASTWord, 24> astWords;
        vec<u32, 8> ast;
        vec<Pos, 8> nodePositions;
        vec<TypeIndex, 8> nodeTypes;
        vec<ScopeIndex, 8> nodeScopes;
        vec<Constant, 8> constantList;
        map<Constant, u32> constants;
        Compilation* compilation;
        TypeSystem* types;
        vec<Scope*> scopes;
        vec<VariableInfo, 8> globals;
        vec<Function*, 8> functions;
        map<Function*, u32> importedFunctions;
        vec<Overloads*, 4> overloads;
        const_slice<i8> source;
        vec<u32> lineOffsets;
        NodeIndex topLevel;
        u32 numTemps = 0;
        bool noMangling = false;
        bool isMain = true;

        inline Module(Compilation* compilation_in, const_slice<i8> source, vec<u32>&& lineOffsets);
        ~Module() override;

        inline u32 intern(Constant constant) {
            auto it = constants.find(constant);
            if (it != constants.end())
                return it->value;
            else {
                constants.put(constant, constantList.size());
                constantList.push(constant);
                return constantList.size() - 1;
            }
        }

        inline maybe<const_slice<i8>> getSource() override {
            return some<const_slice<i8>>(source);
        }

        inline maybe<const_slice<u32>> getLineOffsets() override {
            return some<const_slice<u32>>(lineOffsets);
        }

        inline InlineResult tryIntern(Constant constant) {
            switch (constant.kind) {
                case Constant::Int:
                    if LIKELY(fitsSigned<ASTWord::ConstantBits>(constant.intConst))
                        return { .canIntern = true, .payload = (i32)constant.intConst };
                    return { .canIntern = false, .payload = (i32)intern(constant) };
                case Constant::Unsigned:
                    if LIKELY(fitsUnsigned<ASTWord::ConstantBits>(constant.uintConst))
                        return { .canIntern = true, .payload = (i32)constant.uintConst };
                    return { .canIntern = false, .payload = (i32)intern(constant) };
                case Constant::Float: {
                    u64 bits = bitcast<u64>(constant.floatConst);
                    constexpr u32 shiftAmount = 64 - ASTWord::ConstantBits;
                    if (!(bits & (1ull << shiftAmount) - 1))
                        return { .canIntern = true, .payload = (i32)(bits >> shiftAmount) };
                    return { .canIntern = false, .payload = (i32)intern(constant) };
                }
                case Constant::String:
                    // Strings are special-cased; since they're just symbols, and we require that
                    // symbols be stored losslessly in AST nodes, we skip the inline bit and use
                    // the symbol field of the AST word to store the string. This is handled in the
                    // caller of this function.
                    return { .canIntern = true, .payload = (i32)constant.stringConst };
                case Constant::Bool:
                    return { .canIntern = true, .payload = constant.boolConst ? 1 : 0 };
                case Constant::Char:
                    static_assert(ASTWord::ConstantBits >= 21); // Enough to store any one codepoint.
                    return { .canIntern = true, .payload = (i32)constant.charConst };
            }
        }

        inline Constant valueOf(ASTWord ast) {
            assert(ast.isLeaf());
            switch (ast.kind) {
                case ASTWord::Int:
                    if (ast.isInline)
                        return Constant::IntConst(ast.inlineSigned);
                    else
                        return constantList[ast.constantIndex];
                case ASTWord::Unsigned:
                    if (ast.isInline)
                        return Constant::UnsignedConst(ast.inlineUnsigned);
                    else
                        return constantList[ast.constantIndex];
                case ASTWord::Float: {
                    constexpr u64 shiftAmount = 64 - ASTWord::ConstantBits;
                    if (ast.isInline)
                        return Constant::FloatConst(bitcast<f64>(u64(ast.inlineUnsigned) << shiftAmount));
                    else
                        return constantList[ast.constantIndex];
                }
                case ASTWord::String:
                    return Constant::StringConst(ast.symbol);
                case ASTWord::Bool:
                    return Constant::BoolConst(ast.inlineUnsigned);
                case ASTWord::Char:
                    return Constant::CharConst(ast.inlineUnsigned);
                default:
                    unreachable("Not a constant AST node!");
            }
        }

        inline AST getTopLevel() {
            return node(topLevel);
        }

        inline void setTopLevel(AST ast) {
            topLevel = ast.node;
            tempTopLevel = nullptr;
        }

        inline AST addLeaf(ASTKind kind, const Identifier& identifier) {
            assert(kind == ASTKind::Ident);
            ASTWord ast;
            ast.kind = kind;
            ast.symbol = identifier.name.symbol;
            return AST(this, ast);
        }

        inline AST addLeaf(ASTKind kind, const Local& variable) {
            assert(kind == ASTKind::Local
                || kind == ASTKind::Const
                || kind == ASTKind::Typename
                || kind == ASTKind::GenericTypename);
            assert(nodeScopes.size() != 0); // Should only be producing these after scope resolution.
            ASTWord ast;
            ast.kind = kind;
            ast.id = variable.index;
            return AST(this, ast);
        }

        inline AST addLeaf(ASTKind kind, const Global& variable) {
            assert(kind == ASTKind::Global
                || kind == ASTKind::GlobalConst
                || kind == ASTKind::GlobalTypename
                || kind == ASTKind::GlobalGenericTypename);
            assert(nodeScopes.size() != 0); // Should only be producing these after scope resolution.
            ASTWord ast;
            ast.kind = kind;
            ast.id = variable.index;
            return AST(this, ast);
        }

        inline AST addLeaf(ASTKind kind, const Constant& constant) {
            switch (constant.kind) {
                case Constant::Int:
                    assert(kind == ASTKind::Int);
                    break;
                case Constant::Unsigned:
                    assert(kind == ASTKind::Unsigned);
                    break;
                case Constant::Float:
                    assert(kind == ASTKind::Float);
                    break;
                case Constant::String:
                    assert(kind == ASTKind::String);
                    break;
                case Constant::Bool:
                    assert(kind == ASTKind::Bool);
                    break;
                case Constant::Char:
                    assert(kind == ASTKind::Char);
                    break;
            }
            ASTWord ast;
            ast.kind = kind;
            if (constant.kind == Constant::String) {
                ast.symbol = constant.stringConst;
                this->astWords.push(ast);
                return AST(this, 0, this->astWords.size() - 1);
            }
            auto internResult = tryIntern(constant);
            ast.isInline = internResult.canIntern;
            ast.constantIndex = internResult.payload;
            return AST(this, ast);
        }

        inline AST addLeaf(ASTKind kind, const FieldId& field) {
            assert(kind == ASTKind::Field);
            assert(nodeScopes.size() != 0); // Should only be producing these after scope resolution.
            ASTWord ast;
            ast.kind = kind;
            auto internResult = tryIntern(Constant::UnsignedConst(field.id));
            ast.isInline = internResult.canIntern;
            ast.constantIndex = internResult.payload;
            return AST(this, ast);
        }

        inline AST addLeaf(ASTKind kind, Function* const& function) {
            assert(kind == ASTKind::ResolvedFunction);
            assert(nodeScopes.size() != 0); // Should only be producing these after scope resolution.
            assert(nodeTypes.size() != 0); // Should only be producing these after scope resolution.
            ASTWord ast;
            ast.kind = kind;
            auto internResult = tryIntern(Constant::UnsignedConst(functionIndex(function)));
            ast.isInline = internResult.canIntern;
            ast.constantIndex = internResult.payload;
            return AST(this, ast);
        }

        template<typename... Args>
        AST addLeaf(ASTKind kind) {
            ASTWord ast;
            ast.kind = kind;
            return AST(this, ast);
        }

        template<typename... Args>
        AST addLeaf(ASTKind kind, const Args&... args) {
            unreachable("Should have used one of the specializations.");
        }

        inline u32 computeArity() {
            return 0;
        }

        template<typename... Args>
        inline u32 computeArity(const Identifier&, const Args&... args) { return 1 + computeArity(args...); }
        template<typename... Args>
        inline u32 computeArity(const Local&, const Args&... args) { return 1 + computeArity(args...); }
        template<typename... Args>
        inline u32 computeArity(const Global&, const Args&... args) { return 1 + computeArity(args...); }
        template<typename... Args>
        inline u32 computeArity(const Constant&, const Args&... args) { return 1 + computeArity(args...); }
        template<typename... Args>
        inline u32 computeArity(const FieldId&, const Args&... args) { return 1 + computeArity(args...); }
        template<typename... Args>
        inline u32 computeArity(Function* const&, const Args&... args) { return 1 + computeArity(args...); }
        template<typename... Args>
        inline u32 computeArity(const AST&, const Args&... args) { return 1 + computeArity(args...); }
        template<typename T, u32 N, typename... Args>
        inline u32 computeArity(const vec<T, N>& vec, Args... args) {
            u32 arity = computeArity(args...);
            for (const auto& i : vec)
                arity += computeArity(i);
            return arity;
        }

        inline void addChild(const Local& local) {
            astWords.push(addLeaf(ASTKind::Local, local).firstWord());
        }

        inline void addChild(const Global& global) {
            astWords.push(addLeaf(ASTKind::Global, global).firstWord());
        }

        inline void addChild(const Identifier& ident) {
            astWords.push(addLeaf(ASTKind::Ident, ident).firstWord());
        }

        inline void addChild(const FieldId& field) {
            astWords.push(addLeaf(ASTKind::Field, field).firstWord());
        }

        inline void addChild(Function* const& function) {
            astWords.push(addLeaf(ASTKind::ResolvedFunction, function).firstWord());
        }

        inline void addChild(const Constant& constant) {
            switch (constant.kind) {
                case Constant::Int:
                    astWords.push(addLeaf(ASTKind::Int, constant).firstWord());
                    break;
                case Constant::Unsigned:
                    astWords.push(addLeaf(ASTKind::Unsigned, constant).firstWord());
                    break;
                case Constant::Float:
                    astWords.push(addLeaf(ASTKind::Float, constant).firstWord());
                    break;
                case Constant::String:
                    astWords.push(addLeaf(ASTKind::String, constant).firstWord());
                    break;
                case Constant::Bool:
                    astWords.push(addLeaf(ASTKind::Bool, constant).firstWord());
                    break;
                case Constant::Char:
                    astWords.push(addLeaf(ASTKind::Char, constant).firstWord());
                    break;
            }
        }

        inline void addChild(const AST& ast) {
            if (ASTWord::isLeaf(ast.kind()))
                astWords.push(ast.firstWord());
            else {
                ASTWord word;
                word.makeRef(ast.node);
                astWords.push(word);
            }
        }

        inline void addChildren() {}

        template<typename T, typename... Args>
        inline void addChildren(const T& child, const Args&... args) {
            addChild(child);
            addChildren(args...);
        }

        template<typename T, u32 N, typename... Args>
        inline void addChildren(const vec<T, N>& vec, const Args&... args) {
            for (auto child : vec)
                addChild(child);
            addChildren(args...);
        }

        template<typename... Args>
        inline AST addBranch(ASTKind kind, Pos pos, const Args&... args) {
            assert(kind < ASTKind::NumKinds);
            ASTWord ast;
            ast.kind = kind;
            ast.arity = computeArity(args...);
            u32 word = astWords.size();
            astWords.push(ast);
            addChildren(args...);

            u32 node = this->ast.size();
            this->ast.push(word);
            nodePositions.push(pos);
            assert(!nodeScopes.size());
            assert(!nodeTypes.size());
            return AST(this, node, word);
        }

        template<typename... Args>
        inline AST addBranch(ASTKind kind, Pos pos, ScopeIndex scope, const Args&... args) {
            assert(kind < ASTKind::NumKinds);
            ASTWord ast;
            ast.kind = kind;
            ast.arity = computeArity(args...);
            u32 word = astWords.size();
            astWords.push(ast);
            addChildren(args...);

            u32 node = this->ast.size();
            this->ast.push(word);
            nodePositions.push(pos);
            assert(nodeScopes.size());
            nodeScopes.push(scope);
            assert(!nodeTypes.size());
            return AST(this, node, word);
        }

        template<typename... Args>
        inline AST addBranch(ASTKind kind, Pos pos, ScopeIndex scope, TypeIndex type, const Args&... args) {
            assert(kind < ASTKind::NumKinds);
            ASTWord ast;
            ast.kind = kind;
            ast.arity = computeArity(args...);
            u32 word = astWords.size();
            astWords.push(ast);
            addChildren(args...);

            u32 node = this->ast.size();
            this->ast.push(word);
            nodePositions.push(pos);
            assert(nodeScopes.size());
            nodeScopes.push(scope);
            assert(nodeTypes.size());
            nodeTypes.push(type);
            return AST(this, node, word);
        }

        inline AST replace(AST existing, AST node) {
            ast[existing.node] = ast[node.node];
            return AST(this, existing.node, ast[existing.node]);
        }

        template<typename... Args>
        inline AST replace(AST existing, ASTKind kind, const Args&... args) {
            assert(kind < ASTKind::NumKinds);
            ASTWord word;
            word.kind = kind;
            word.arity = computeArity(args...);
            u32 end = astWords.size();
            u32 index = end;
            if (word.arity <= existing.arity())
                index = ast[existing.node];
            astWords.push(word);
            addChildren(args...);
            if (index == ast[existing.node]) {
                memory::copy(astWords.begin() + index, astWords.begin() + end, sizeof(ASTWord) * (word.arity + 1));
                astWords.shrinkBy(word.arity + 1);
            } else
                ast[existing.node] = index;
            return AST(this, existing.node, ast[existing.node]);
        }

        template<typename... Args>
        inline AST addBranchNoPos(ASTKind kind, const Args&... args) {
            return addBranch(kind, Pos(0, 0), args...);
        }

        template<typename... Args>
        inline AST add(ASTKind kind, Pos pos, const Args&... args) {
            assert(!ASTWord::isLeaf(kind));
            return addBranch(kind, pos, args...);
        }

        template<typename... Args>
        inline AST add(ASTKind kind, Pos pos, ScopeIndex scope, const Args&... args) {
            assert(!ASTWord::isLeaf(kind));
            return addBranch(kind, pos, scope, args...);
        }

        template<typename... Args>
        inline AST add(ASTKind kind, Pos pos, ScopeIndex scope, TypeIndex type, const Args&... args) {
            assert(!ASTWord::isLeaf(kind));
            return addBranch(kind, pos, scope, type, args...);
        }

        template<typename... Args>
        inline AST add(ASTKind kind, Pos pos, Scope* scope, const Args&... args) {
            return add(kind, pos, scope->index, args...);
        }

        template<typename... Args>
        inline AST add(ASTKind kind, Pos pos, Scope* scope, TypeIndex type, const Args&... args) {
            return add(kind, pos, scope->index, type, args...);
        }

        template<typename... Args>
        inline AST add(ASTKind kind, Pos pos, ScopeIndex scope, Type type, const Args&... args) {
            return add(kind, pos, scope, type.index, args...);
        }

        template<typename... Args>
        inline AST add(ASTKind kind, Pos pos, Scope* scope, Type type, const Args&... args) {
            return add(kind, pos, scope->index, type.index, args...);
        }

        template<typename... Args>
        inline AST add(ASTKind kind, const Args&... args) {
            bool leaf = ASTWord::isLeaf(kind);
            if (leaf)
                return addLeaf(kind, args...);
            else
                return addBranchNoPos(kind, args...);
        }

        inline AST node(NodeIndex index) {
            return AST(this, index, ast[index]);
        }

        template<TypeKind kind, typename... Args>
        inline Type type(const Args&... args) {
            return types->encode<kind>(args...);
        }

        inline AST fromOperand(ASTWord word) {
            if (word.isRef())
                return node(word.child);
            else
                return AST(this, word);
        }

        template<typename... Args>
        inline Type ptrType(const Args&... args) { return type<TypeKind::Pointer>(args...); }
        template<typename... Args>
        inline Type arrayType(const Args&... args) { return type<TypeKind::Array>(args...); }
        template<typename... Args>
        inline Type sliceType(const Args&... args) { return type<TypeKind::Slice>(args...); }
        template<typename... Args>
        inline Type namedType(const Args&... args) { return type<TypeKind::Named>(args...); }
        inline Type structType(const StructBuilder& builder) { return type<TypeKind::Struct>(builder); }
        inline Type tupleType(const TupleBuilder& builder) { return type<TypeKind::Tuple>(builder); }
        inline Type funType(const FunctionBuilder& builder) { return type<TypeKind::Function>(builder); }
        inline Type unionType(const UnionBuilder& builder) { return type<TypeKind::Union>(builder); }
        template<typename... Args>
        inline Type structType(const Args&... args) { return type<TypeKind::Struct>(StructBuilder(types, args...)); }
        template<typename... Args>
        inline Type tupleType(const Args&... args) { return type<TypeKind::Tuple>(TupleBuilder(types, args...)); }
        template<typename... Args>
        inline Type funType(const Args&... args) { return type<TypeKind::Function>(FunctionBuilder(types, args...)); }
        template<typename... Args>
        inline Type unionType(const Args&... args) { return type<TypeKind::Union>(UnionBuilder(types, args...)); }
        template<typename... Args>
        inline Type varType(const Args&... args) { return type<TypeKind::Var>(args...); }
        template<typename... Args>
        inline Type rangeType(const Args&... args) { return type<TypeKind::Range>(args...); }
        template<typename... Args>
        inline Type numType(const Args&... args) { return type<TypeKind::Numeric>(args...); }
        inline Type signedType(u32 bits) { return types->signedType(bits); }
        inline Type unsignedType(u32 bits) { return types->unsignedType(bits); }

        inline Type invalidType() { return types->invalidType(); }
        inline Type bottomType() { return types->get(Bottom); }
        inline Type voidType() { return types->get(Void); }
        inline Type boolType() { return types->get(Bool); }
        inline Type charType() { return types->get(Char); }
        inline Type stringType() { return types->get(String); }
        inline Type anyType() { return types->get(Any); }
        inline Type i8Type() { return types->get(I8); }
        inline Type i16Type() { return types->get(I16); }
        inline Type i32Type() { return types->get(I32); }
        inline Type i64Type() { return types->get(I64); }
        inline Type u8Type() { return types->get(U8); }
        inline Type u16Type() { return types->get(U16); }
        inline Type u32Type() { return types->get(U32); }
        inline Type u64Type() { return types->get(U64); }
        inline Type f32Type() { return types->get(F32); }
        inline Type f64Type() { return types->get(F64); }
        inline Type bottomNumberType() { return types->get(BottomNumber); }
        inline Type bottomFloatType() { return types->get(BottomFloat); }
        inline Type topIntegerType() { return types->get(TopInteger); }
        inline Type topNumberType() { return types->get(F64); }
        inline Type topFloatType() { return types->get(F64); }

        template<typename StringLike>
        inline Symbol sym(StringLike str) const;
        inline const_slice<i8> str(Symbol sym) const;

        inline Global addRootGlobal(VariableKind kind, TypeIndex type, Symbol name) {
            assert(this == compilation->rootModule);
            VariableInfo info;
            info.type = type;
            info.scope = VariableInfo::Global;
            info.kind = kind;
            info.decl = InvalidNode;
            info.name = name;
            globals.push(info);
            return Global(globals.size() - 1);
        }

        inline Global addGlobalOverload(u32 overloads, Symbol name) {
            VariableInfo info;
            info.type = InvalidType;
            info.scope = VariableInfo::Global;
            info.kind = VariableKind::OverloadedFunction;
            info.name = name;
            info.overloads = overloads;
            globals.push(info);
            return Global(globals.size() - 1);
        }

        inline Global addGlobal(VariableKind kind, Symbol name) {
            return addGlobal(kind, InvalidNode, InvalidType, name);
        }

        inline Global addGlobal(VariableKind kind, TypeIndex type, Symbol name) {
            return addGlobal(kind, InvalidNode, type, name);
        }

        inline Global addGlobal(VariableKind kind, AST decl, Symbol name) {
            return addGlobal(kind, decl.node, InvalidType, name);
        }

        inline Global addGlobal(VariableKind kind, AST decl, TypeIndex type, Symbol name) {
            return addGlobal(kind, decl.node, type, name);
        }

        inline Global addGlobal(VariableKind kind, NodeIndex decl, TypeIndex type, Symbol name) {
            VariableInfo info;
            info.kind = kind;
            info.scope = VariableInfo::Global;
            info.isImport = false;
            info.decl = decl;
            info.type = type;
            info.name = name;
            globals.push(info);
            return Global(globals.size() - 1);
        }

        inline u32 functionIndex(Function* function) {
            if (function->module == this)
                return function->index;
            auto it = importedFunctions.find(function);
            if (it == importedFunctions.end()) {
                importedFunctions.put(function, functions.size());
                functions.push(function);
                return functions.size() - 1;
            }
            return it->value;
        }

        inline Global addGlobalFunctionImport(VariableKind kind, Function* function) {
            assert(kind == VariableKind::Function);
            assert(function->typeIndex != InvalidType);

            VariableInfo info;
            info.kind = kind;
            info.scope = VariableInfo::Global;
            info.isImport = true;
            info.functionIndex = functionIndex(function);
            info.type = function->typeIndex;
            info.name = function->name;
            globals.push(info);
            return Global(globals.size() - 1);
        }

        inline Global addGlobalImport(VariableKind kind, TypeIndex type, Symbol name) {
            assert(kind != VariableKind::Function);

            VariableInfo info;
            info.kind = kind;
            info.scope = VariableInfo::Global;
            info.isImport = true;
            info.functionIndex = InvalidNode;
            info.type = type;
            info.name = name;
            globals.push(info);
            return Global(globals.size() - 1);
        }

        inline Global addGlobalIndirect(ScopeIndex scope, u32 index) {
            VariableInfo info;
            info.type = InvalidType;
            info.decl = InvalidNode;
            info.kind = VariableKind::Forward;
            info.scope = VariableInfo::Global;
            info.defScope = scope;
            info.index = index;
            globals.push(info);
            return Global(globals.size() - 1);
        }

        inline Global addTemp(TypeIndex type) {
            array<i8, 64> buf;
            Symbol name = sym(prints(buf, '_', numTemps ++));
            VariableInfo info;
            info.type = type;
            info.scope = VariableInfo::Global;
            info.kind = VariableInfo::Temp;
            info.decl = InvalidNode;
            info.name = name;
            globals.push(info);
            return Global(globals.size() - 1);
        }

        inline Global addTemp() {
            return addTemp(InvalidType);
        }

        inline VariableHandle naturalize(VariableHandle handle) {
            if (!handle)
                return handle;
            if (!handle.isGlobal()) {
                assert(handle.function->module == this);
                return handle;
            }
            assert(handle.isGlobal());
            Global global = naturalize(handle.module, handle.i);
            return VariableHandle(this, global.index).expand();
        }

        inline Global naturalize(Module* other, Global global) {
            if (this == other)
                return global;
            auto existing = getTopLevel().scope()->entries.find(other->globals[global.index].name);
            if (existing != getTopLevel().scope()->entries.end())
                panic("Tried to pull in definition for already-defined variable ", str(globals[existing->value].name));
            auto otherEntry = other->globals[global.index];
            return addGlobal(otherEntry.kind, otherEntry.type, otherEntry.name);
        }

        inline VariableHandle lookup(Scope* scope, Symbol name) {
            auto result = scope->find(name);
            if (result) {
                if (result.scope->function)
                    return VariableHandle(result.scope->function, result.index).expand();
                return VariableHandle(result.scope->module, result.index).expand();
            } else
                return VariableHandle();
        }

        inline Function* addFunction(AST owner, Function* parent) {
            functions.push(new Function(this, parent, owner.node));
            functions.back()->index = functions.size() - 1;
            return functions.back();
        }

        inline Overloads* addOverloads(Function* function) {
            overloads.push(new Overloads());
            overloads.back()->index = overloads.size() - 1;
            overloads.back()->add(function);
            return overloads.back();
        }

        inline Overloads* addOverloads(Overloads* existing) {
            overloads.push(new Overloads());
            overloads.back()->index = overloads.size() - 1;
            overloads.back()->add(existing);
            return overloads.back();
        }

        inline Scope* addScope(ScopeKind kind, NodeIndex owner, Scope* parent) {
            scopes.push(new Scope(kind, this, parent, owner, scopes.size()));
            return scopes.back();
        }

        inline Scope* addScope(ScopeKind kind, NodeIndex owner, Scope* parent, Function* function) {
            scopes.push(new Scope(kind, this, function, parent, owner, scopes.size()));
            return scopes.back();
        }

        void print(Compilation* compilation) override;
        void printScopes(Compilation* compilation);
        void printScope(Compilation* compilation, Scope* scope, const vec<vec<Scope*>>& children, u32 indent);
        u64 reportSize() override;

        vec<AST, 32>* tempTopLevel = nullptr; // Used only in aggressive validation.
        void setTempTopLevel(vec<AST, 32>& topLevel);
        void validateAggressively();
        void validateAggressively(AST ast);
        bool shouldValidate = false;
    };

    template<typename StringLike>
    inline Symbol Module::sym(StringLike str) const {
        return compilation->sym(str);
    }

    inline const_slice<i8> Module::str(Symbol sym) const {
        return compilation->str(sym);
    }

    // Function Methods

    inline Module::Module(Compilation* compilation_in, const_slice<i8> source_in, vec<u32>&& lineOffsets_in):
        compilation(compilation_in), types(compilation->types), source(source_in), lineOffsets(lineOffsets_in) {}

    inline Local Function::addTemp(TypeIndex type) {
        array<i8, 64> buf;
        Symbol name = module->sym(prints(buf, '_', numTemps ++));
        VariableInfo info;
        info.type = type;
        info.scope = VariableInfo::Local;
        info.kind = VariableInfo::Temp;
        info.decl = InvalidNode;
        info.name = name;
        locals.push(info);
        return Local(locals.size() - 1);
    }

    inline Type Function::type() const {
        assert(typeIndex != InvalidType);
        return module->types->get(typeIndex);
    }

    inline Function::Function(Module* module_in, Function* parent_in, NodeIndex decl_in):
        module(module_in), parent(parent_in), decl(decl_in) {
        name = module->node(decl).child(1).symbol();
    }

    inline Local Function::addLocalFunctionImport(VariableKind kind, Function* function) {
        assert(kind == VariableKind::Function);
        assert(function->typeIndex != InvalidType);

        VariableInfo info;
        info.kind = kind;
        info.scope = VariableInfo::Local;
        info.isImport = true;
        info.functionIndex = module->functionIndex(function);
        info.type = function->typeIndex;
        info.name = function->name;
        locals.push(info);
        return Local(locals.size() - 1);
    }

    // AST Methods

    inline AST AST::Iterator::operator*() const {
        auto n = module->astWords[word];
        if (n.isRef())
            return AST(module, n.child, module->ast[n.child]);
        else
            return AST(module, n);
    }

    inline ASTWord& AST::firstWord() {
        return nthWord(0);
    }

    inline ASTWord AST::firstWord() const {
        return nthWord(0);
    }

    inline ASTWord& AST::nthWord(u32 i) {
        if (node == InvalidNode) {
            assert(i == 0);
            return word;
        }
        return module->astWords[word.bits + i];
    }

    inline ASTWord AST::nthWord(u32 i) const {
        if (node == InvalidNode) {
            assert(i == 0);
            return word;
        }
        return module->astWords[word.bits + i];
    }

    inline ASTWord& AST::refWord(u32 i) {
        return nthWord(i + 1);
    }

    inline ASTWord AST::refWord(u32 i) const {
        return nthWord(i + 1);
    }

    inline ASTWord AST::asOperand() const {
        if (isLeaf())
            return word;
        else
            return ASTWord::makeRefOf(node);
    }

    inline ASTKind AST::kind() const {
        return firstWord().kind;
    }

    inline void AST::setKind(ASTKind kind) {
        firstWord().kind = kind;
    }

    inline void AST::setArity(u32 newArity) {
        firstWord().arity = newArity;
    }

    inline u32 AST::arity() const {
        return firstWord().arity;
    }

    inline void AST::removeChild(u32 i) {
        assert(i < arity());
        for (u32 k = i; k < arity() - 1; k ++)
            nthWord(k) = nthWord(k + 1);
        firstWord().arity --;
    }

    inline void AST::trimChildrenTo(u32 newArity) {
        assert(newArity <= arity());
        firstWord().arity = newArity;
    }

    inline i64 AST::intConst() const {
        assert(kind() == ASTKind::Int);
        return module->valueOf(firstWord()).intConst;
    }

    inline u64 AST::uintConst() const {
        assert(kind() == ASTKind::Unsigned);
        return module->valueOf(firstWord()).intConst;
    }

    inline u32 AST::fieldId() const {
        assert(kind() == ASTKind::Field);
        if (firstWord().isInline)
            return firstWord().inlineUnsigned;
        else
            return module->constantList[firstWord().constantIndex].uintConst;
    }

    inline Function* AST::resolvedFunction() const {
        assert(kind() == ASTKind::ResolvedFunction);
        if (firstWord().isInline)
            return module->functions[firstWord().inlineUnsigned];
        else
            return module->functions[module->constantList[firstWord().constantIndex].uintConst];
    }

    inline f64 AST::floatConst() const {
        assert(kind() == ASTKind::Float);
        return module->valueOf(firstWord()).floatConst;
    }

    inline Symbol AST::stringConst() const {
        assert(kind() == ASTKind::String);
        return symbol();
    }

    inline bool AST::boolConst() const {
        assert(kind() == ASTKind::Bool);
        return module->valueOf(firstWord()).boolConst;
    }

    inline u32 AST::charConst() const {
        assert(kind() == ASTKind::Char);
        return module->valueOf(firstWord()).charConst;
    }

    inline void AST::setIntConst(i64 i) {
        auto intern = module->tryIntern(Constant::IntConst(i));
        if (intern.canIntern)
            firstWord().isInline = true, firstWord().inlineSigned = intern.payload;
        else
            firstWord().isInline = false, firstWord().constantIndex = intern.payload;
    }

    inline void AST::setUintConst(u64 i) {
        auto intern = module->tryIntern(Constant::UnsignedConst(i));
        if (intern.canIntern)
            firstWord().isInline = true, firstWord().inlineSigned = intern.payload;
        else
            firstWord().isInline = false, firstWord().constantIndex = intern.payload;
    }

    inline void AST::setFieldId(u32 i) {
        auto intern = module->tryIntern(Constant::UnsignedConst(i));
        if (intern.canIntern)
            firstWord().isInline = true, firstWord().inlineSigned = intern.payload;
        else
            firstWord().isInline = false, firstWord().constantIndex = intern.payload;
    }

    inline void AST::setResolvedFunction(Function* function) {
        auto intern = module->tryIntern(Constant::UnsignedConst(module->functionIndex(function)));
        if (intern.canIntern)
            firstWord().isInline = true, firstWord().inlineSigned = intern.payload;
        else
            firstWord().isInline = false, firstWord().constantIndex = intern.payload;
    }

    inline void AST::setFloatConst(f64 f) {
        auto intern = module->tryIntern(Constant::FloatConst(f));
        if (intern.canIntern)
            firstWord().isInline = true, firstWord().inlineUnsigned = intern.payload;
        else
            firstWord().isInline = false, firstWord().constantIndex = intern.payload;
    }

    inline void AST::setStringConst(Symbol string) {
        auto intern = module->tryIntern(Constant::StringConst(string));
        assert(intern.canIntern);
        assert(string.symbol < (1u << ASTWord::PayloadBits));
        firstWord().symbol = string.symbol;
    }

    inline void AST::setBoolConst(bool b) {
        auto intern = module->tryIntern(Constant::BoolConst(b));
        assert(intern.canIntern);
        firstWord().inlineUnsigned = intern.payload;
    }

    inline void AST::setCharConst(u32 c) {
        auto intern = module->tryIntern(Constant::CharConst(c));
        assert(intern.canIntern);
        firstWord().inlineUnsigned = intern.payload;
    }

    inline Symbol AST::symbol() const {
        return firstWord().symbol;
    }

    inline void AST::setSymbol(Symbol s) {
        assert(s.symbol < (1u << ASTWord::PayloadBits));
        firstWord().symbol = s.symbol;
    }

    inline u32 AST::variable() const {
        return firstWord().id;
    }

    inline Function* AST::function() const {
        assert(!isLeaf());
        return scope()->function;
    }

    inline bool AST::isLocal() const {
        return (u32)kind() <= ASTKind::LastLocal;
    }

    inline bool AST::isGlobal() const {
        return (u32)kind() > ASTKind::LastLocal && (u32)kind() <= ASTKind::LastGlobal;
    }

    inline bool AST::isVariable() const {
        return (u32)kind() <= ASTKind::LastGlobal;
    }

    inline bool AST::missing() const {
        return kind() == ASTKind::Missing;
    }

    inline const VariableInfo& AST::varInfo() const {
        assert(!isLocal());
        const auto& info = module->globals[variable()];
        if (info.kind == VariableKind::Forward) {
            const Scope* scope = module->scopes[info.scope];
            return scope->isGlobal() ? module->globals[info.index] : scope->function->locals[info.index];
        }
        return info;
    }

    inline VariableInfo& AST::varInfo() {
        assert(!isLocal());
        auto& info = module->globals[variable()];
        if (info.kind == VariableKind::Forward) {
            Scope* scope = module->scopes[info.scope];
            return scope->isGlobal() ? module->globals[info.index] : scope->function->locals[info.index];
        }
        return info;
    }

    inline const VariableInfo& AST::varInfo(Function* function) const {
        if (isGlobal())
            return this->varInfo();
        const auto& info = function->locals[variable()];
        if (info.kind == VariableKind::Forward) {
            Scope* scope = module->scopes[info.scope];
            return scope->isGlobal() ? module->globals[info.index] : scope->function->locals[info.index];
        }
        return info;
    }

    inline VariableInfo& AST::varInfo(Function* function) {
        if (isGlobal())
            return this->varInfo();
        auto& info = function->locals[variable()];
        if (info.kind == VariableKind::Forward) {
            Scope* scope = module->scopes[info.scope];
            return scope->isGlobal() ? module->globals[info.index] : scope->function->locals[info.index];
        }
        return info;
    }

    inline void AST::setVariable(u32 s) {
        assert(s < (1u << ASTWord::PayloadBits));
        firstWord().id = s;
    }

    inline AST AST::child(u32 i) const {
        assert(i < arity());
        auto n = nthWord(i + HeaderSlots);
        if (n.isRef())
            return AST(module, n.child, module->ast[n.child]);
        else
            return AST(module, n);
    }

    inline void AST::setChild(u32 i, AST child) {
        assert(i < arity());
        if (child.isLeaf())
            nthWord(i + HeaderSlots) = child.word;
        else
            nthWord(i + HeaderSlots).makeRef(child.node);
    }

    inline Pos AST::pos() const {
        assert(!isLeaf());
        return module->nodePositions[node];
    }

    inline void AST::setPos(Pos pos) {
        assert(!isLeaf());
        module->nodePositions[node] = pos;
    }

    inline Scope* AST::scope() const {
        assert(!isLeaf());
        return module->scopes[module->nodeScopes[node]];
    }

    inline void AST::setScope(Scope* scope) {
        assert(!isLeaf());
        module->nodeScopes[node] = scope->index;
    }

    inline TypeIndex AST::typeIndex() const {
        assert(!isLeaf());
        return module->nodeTypes[node];
    }

    inline TypeIndex AST::typeIndex(Function* function) const {
        if (isVariable()) {
            if LIKELY(isLocal())
                return varInfo(function).type;
            else
                return varInfo().type;
        } else {
            assert(!isLeaf());
            return typeIndex();
        }
    }

    inline TypeIndex AST::typeIndex(Module* module) const {
        if (isVariable()) {
            assert(isGlobal());
            return varInfo().type;
        } else {
            assert(!isLeaf());
            return typeIndex();
        }
    }

    inline Type AST::type() const {
        return module->types->get(typeIndex());
    }

    inline Type AST::type(Function* function) const {
        if (!function)
            return type(module);
        return function->module->types->get(typeIndex(function));
    }

    inline Type AST::type(Module* module) const {
        return module->types->get(typeIndex(module));
    }

    inline void AST::setType(TypeIndex t) {
        assert(!isLeaf());
        module->nodeTypes[node] = t;
    }

    inline void AST::setType(Type t) {
        setType(t.index);
    }

    template<typename... Args>
    inline void AST::replace(ASTKind kind, const Args&... args) {
        module->replace(*this, kind, args...);
    }

    inline VariableHandle VariableHandle::expand() const {
        if (kind() == VariableKind::Forward) {
            const auto& info = isGlobal() ? module->globals[i] : function->locals[i];
            Scope* scope = isGlobal() ? module->scopes[info.scope] : function->module->scopes[info.scope];
            return scope->isGlobal() ? VariableHandle(scope->module, info.index) : VariableHandle(scope->function, info.index);
        }
        return *this;
    }

    inline VariableKind VariableHandle::kind() const {
        if (isGlobal())
            return module->globals[i].kind;
        return function->locals[i].kind;
    }

    inline TypeIndex VariableHandle::type() const {
        if (isGlobal())
            return module->globals[i].type;
        return function->locals[i].type;
    }

    inline void VariableHandle::setType(Type type) {
        setType(type.index);
    }

    inline void VariableHandle::setType(TypeIndex type) {
        if (isGlobal())
            module->globals[i].type = type;
        else
            function->locals[i].type = type;
    }

    inline Symbol VariableHandle::name() const {
        if (isGlobal())
            return module->globals[i].name;
        return function->locals[i].name;
    }

    inline bool VariableHandle::hasDecl() const {
        const auto& info = isGlobal() ? module->globals[i] : function->locals[i];
        return !info.isImport && info.decl != InvalidNode;
    }

    inline AST VariableHandle::decl() const {
        assert(hasDecl());
        return module->node((isGlobal() ? module->globals[i] : function->locals[i]).decl);
    }

    bool isTypeExpression(AST);

    inline AST VarType::owner(Module* module) const {
        return module->node(this->owner());
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_tree_to(IO io, AST parent, const AST& ast, u32 indentAmount, bool allowMultiline, i32 remainingDepth) {
        auto indent = [&](u32 amount) {
            while (amount --)
                io = format(io, ' ');
        };
        auto typeColor = isTypeExpression(ast) ? CYAN : GREEN;
        auto typeReset = RESET;
        bool isDOT = config::printTypeConstraintsAsDOT || config::jasmineASTComments;
        if (isDOT)
            typeColor = typeReset = "";

        if ((remainingDepth == 0 && !ast.isLeaf())
            || (remainingDepth >= 0 && (ast.kind() == ASTKind::Do || ast.kind() == ASTKind::TopLevel)))
            return format(io, '(', AST_NAMES_SYMBOLIC[(u32)ast.kind()], " ...)");
        else if (remainingDepth > 0)
            remainingDepth -= 1;

        auto kind = ast.kind();
        Module* module = ast.module;
        if (ASTWord::isLeaf(kind)) switch (kind) {
            case ASTKind::Int:
                return format(io, ast.intConst());
            case ASTKind::Unsigned:
                return format(io, ast.uintConst());
            case ASTKind::Float:
                return format(io, ast.floatConst());
            case ASTKind::String:
                return format(io, '"', ast.module->str(ast.stringConst()), '"');
            case ASTKind::Bool:
                return format(io, ast.boolConst() ? "true" : "false");
            case ASTKind::Char:
                return format(io, rune(ast.charConst()));
            case ASTKind::Ident:
                return format(io, module->str(ast.symbol()));
            case ASTKind::Local:
            case ASTKind::Capture:
            case ASTKind::Const:
            case ASTKind::Typename:
            case ASTKind::GenericTypename:
                io = format(io, module->str(ast.varInfo(parent.scope()->function).name));
                if (module->nodeTypes.size())
                    io = format(io, typeColor, ":", module->types->get(ast.varInfo(parent.scope()->function).type), typeReset);
                return io;
            case ASTKind::Global:
            case ASTKind::GlobalConst:
            case ASTKind::GlobalTypename:
            case ASTKind::GlobalGenericTypename:
                io = format(io, module->str(ast.varInfo().name));
                if (ast.module->nodeTypes.size())
                    io = format(io, typeColor, ":", module->types->get(ast.varInfo().type), typeReset);
                return io;
            case ASTKind::Field:
                return format(io, ".", ast.fieldId());
            case ASTKind::ResolvedFunction:
                io = format(io, module->str(ast.resolvedFunction()->name));
                io = format(io, "/", ast.module->functionIndex(ast.resolvedFunction()));
                return io;
            case ASTKind::Wildcard:
                return format(io, "...");
            case ASTKind::Missing:
                return format(io, "?");
            case ASTKind::AnyType:
                return format(io, "any");
            default:
                unreachable("Unimplemented formatting for leaf AST node ", AST_NAMES_UPPER[(u32)kind]);
        } else switch (ast.kind()) {
            case ASTKind::TopLevel:
            case ASTKind::Do:
            case ASTKind::If:
            case ASTKind::IfElse:
            case ASTKind::While:
            case ASTKind::For:
            case ASTKind::Match:
            case ASTKind::Case:
            case ASTKind::StructDecl:
            case ASTKind::UnionDecl:
            case ASTKind::StructCaseDecl:
            case ASTKind::UnionCaseDecl:
            case ASTKind::FunDecl:
            case ASTKind::ConstFunDecl: {
                io = format(io, '(', AST_NAMES_SYMBOLIC[(u32)kind]);
                if (ast.arity() == 0)
                    return format(io, ')');
                else
                    io = format(io, ' ');

                u32 newIndent = cstring(AST_NAMES_SYMBOLIC[(u32)kind]).size() + 2 + indentAmount;
                for (u32 i : indices(ast)) {
                    io = format_tree_to(io, ast, ast.child(i), newIndent, allowMultiline, remainingDepth);
                    if (allowMultiline && i < ast.arity() - 1) {
                        io = format(io, '\n');
                        indent(newIndent);
                    } else if (!allowMultiline && i < ast.arity() - 1)
                        io = format(io, ' ');
                }
                io = format(io, ')');
                return io;
            }
            default:
                io = format(io, '(', AST_NAMES_SYMBOLIC[(u32)kind]);
                for (AST child : ast) {
                    io = format(io, ' ');
                    io = format_tree_to(io, ast, child, indentAmount, allowMultiline, remainingDepth);
                }
                io = format(io, ')');
                if (ast.module->nodeTypes.size() && ast.typeIndex() != InvalidType)
                    io = format(io, typeColor, ":", ast.type(), typeReset);
                return io;
        }
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const AST& ast) {
        return format_tree_to(io, AST(), ast, 0, false, -1);
    }

    struct ASTWithParent {
        const AST& parent;
        const AST& ast;
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const ASTWithParent& ast) {
        return format_tree_to(io, ast.parent, ast.ast, 0, false, -1);
    }

    struct ASTWithDepth {
        const AST& ast;
        i32 maxDepth;
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const ASTWithDepth& ast) {
        return format_tree_to(io, AST(), ast.ast, 0, false, ast.maxDepth);
    }

    struct ASTWithDepthAndParent {
        const AST& parent;
        const AST& ast;
        i32 maxDepth;
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const ASTWithDepthAndParent& ast) {
        return format_tree_to(io, ast.parent, ast.ast, 0, false, ast.maxDepth);
    }

    struct Multiline {
        const AST& ast;

        inline Multiline(const AST& ast_in): ast(ast_in) {}
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Multiline& ast) {
        return format_tree_to(io, AST(), ast.ast, 0, true, -1);
    }
}

#endif