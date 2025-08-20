#ifndef CLOVER_TYPE_H
#define CLOVER_TYPE_H

#include "util/config.h"
#include "util/hash.h"
#include "util/vec.h"
#include "util/bits.h"
#include "util/pool.h"
#include "clover/limits.h"
#include "clover/compilation.h"

#define type_assert(...) do { if (!(__VA_ARGS__)) panic("Type assertion failed: ", TOSTRING(__VA_ARGS__)); } while (false)
#define type_error(...) do { panic("Type error: ", __VA_ARGS__); } while (false)

namespace clover {
    struct TypeSystem;

    // This is a big file, so it's divided into sections for easier reading.

    /*
     * Section 1 - Common Definitions
     *
     * This section contains the shared definitions for all types, and their
     * underlying representation. Generally, types are built out of "type
     * words", which are 32-bit unions distinguished by a kind in the first
     * word. Types are stored in a TypeSystem, which contains a big list of
     * TypeWords. The "Type" type is a handle to some of these type words,
     * taking the form of a tuple of TypeSystem and the offset of the first
     * TypeWord of that type. It's through these Type handles that we work
     * with types throughout the rest of the codebase.
     */

    enum RefTraits : u32 {
        Unowned = 0, Own = 1, Uninit = 2
    };

    inline RefTraits operator|(RefTraits a, RefTraits b) {
        return RefTraits((u32)a | (u32)b);
    }

    struct TypeWord {
        enum class Kind : u32 {
            Bottom,         // Nothing, uninhabited type.
            Void,           // Single-valued type, generally representing the absence of another value.
            Bool,           // Boolean, true or false type.
            Char,           // UTF-32 character/code-point.
            Numeric,        // Allows numeric coercion.
            Pointer,        // Pointer to some child type.
            Slice,          // Slice of array of some element type.
            Array,          // Contiguous array of elements.
            Tuple,          // Heterogeneous, fixed-length vector of fields.
            Function,       // Function with arguments and a return type.
            Any,            // Pointer to any value.
            Var,            // Type variable.
            Range,          // Interval of two known type bounds.
            Named,          // Named wrapper around an inner type.
            Struct,         // Named tuple of fields.
            Union,          // Union of multiple cases.
            FirstNamed = Named,
            LastTypeKind = Union,
        };

        constexpr static u32 KindBits = 5;
        static_assert((u32)Kind::LastTypeKind < 1u << KindBits);

        constexpr static u32 CompactArrayTypeBits = 11;
        constexpr static u32 CompactArrayLengthBits = 12;
        constexpr static u32 MaxCompactArrayType = 1u << CompactArrayLengthBits;
        constexpr static u32 MaxCompactArrayLength = 1u << CompactArrayLengthBits;
        constexpr static u32 MetaBits = 3;
        constexpr static u32 PayloadBits = 24;

        union {
            struct { Kind kind : KindBits; u32 isConcrete : 1; u32 : 2; u32 isSigned : 1; u32 isFloat : 1; u32 bitCount : 8; u32 pad : 14; };
            struct { Kind : KindBits; u32 : 3; u32 fieldCount : 24; };
            struct { Kind : KindBits; u32 : 2; u32 isCompactArray : 1; TypeIndex compactElement : CompactArrayTypeBits; u32 compactLength : CompactArrayLengthBits; };
            struct { Kind : KindBits; u32 : 1; RefTraits refTraits : 2; u32 : 24; };
            struct { Kind : KindBits; u32 : 3; TypeIndex extElement : 24; };
            struct { Kind : KindBits; u32 markedEqual : 1; u32 hasOwner : 1; u32 : 1; TypeIndex typeBound : 24; };
            struct { Kind : KindBits; u32 : 1; u32 isCase : 1; u32 : 25; };
            struct { u32 extLength; };
            struct { u32 name; };
            struct { u32 hasName : 1; u32 isBitField : 1; u32 bitFieldSize : 6; TypeIndex fieldType : 24; };
            struct { ConstraintIndex constraintNode; };
            struct { NodeIndex owner; };
            struct { ScopeIndex scope; };
            struct { TypeIndex tag; };
            u8 bytes[4];
            u32 bits;
        };
    };

    static_assert(sizeof(TypeWord) == 4);

    using TypeKind = TypeWord::Kind;

    template<TypeKind Kind>
    struct HandleTypeStruct;
    template<TypeKind Kind>
    using HandleType = typename HandleTypeStruct<Kind>::Type;

    template<TypeKind Kind>
    struct BuilderTypeStruct;
    template<TypeKind Kind>
    using BuilderType = typename BuilderTypeStruct<Kind>::Type;

    struct VarType;
    struct RangeType;

    struct UnifyResult {
        enum SuccessTag {
            UnifySuccess,
            UnifyFailure
        };

        u32 bits;

        inline UnifyResult(SuccessTag tag):
            bits(tag) {}

        inline explicit UnifyResult(u32 bits_in):
            bits(bits_in) {}

        inline explicit operator bool() const {
            return !bits;
        }

        inline bool operator!() const {
            return bits;
        }

        inline bool operator==(UnifyResult other) const {
            return bits == other.bits;
        }

        inline bool operator!=(UnifyResult other) const {
            return bits != other.bits;
        }
    };

    using enum UnifyResult::SuccessTag;

    inline UnifyResult operator&(UnifyResult a, UnifyResult b) {
        if (!a || !b)
            return UnifyFailure; // TODO: Join error messages/location info together here.
        return UnifySuccess;
    }

    inline UnifyResult& operator&=(UnifyResult& a, UnifyResult b) {
        a = a & b;
        return a;
    }

    enum UnifyMode {
        Query = 0,          // Queries if a type can unify onto another, but doesn't change any state.
        InPlace = 1,        // Unifies a type onto another, updating variable bounds in-place.
        Constraining = 2,   // Unifies a type onto another, adding discovered constraints to the out-of-line graph.
        ModeMask = 15,      // Masks out the "mode". Above this are bits reserved for flags.

        NoUnifyFlags = 0,
        MustSubstitute = 16 // Not only does this type need to be a subtype of the target, it needs to be structurally substitutable.
    };

    inline UnifyMode operator&(UnifyMode a, UnifyMode b) {
        return UnifyMode(u32(a) & u32(b));
    }

    inline UnifyMode operator|(UnifyMode a, UnifyMode b) {
        return UnifyMode(u32(a) | u32(b));
    }

    struct Constraints;

    struct Type {
        TypeSystem* types;
        TypeIndex index;
        u32 word;

        inline u32 numWords() const;
        inline TypeWord& firstWord() const;
        inline TypeWord& nthWord(u32 i) const;
        inline TypeKind kind() const;
        inline bool hasName() const;
        inline u64 kindHash() const;
        inline bool isConcrete() const;
        inline void concretify();
        inline bool hasConstraintNode() const;
        inline bool hasConstraintNodeIn(Constraints* constraints) const;
        inline ConstraintIndex constraintNode() const;

        template<TypeKind Kind>
        inline bool is() const {
            return kind() == Kind;
        }

        template<TypeKind Kind>
        inline bool isnt() const {
            return kind() != Kind;
        }

        template<TypeKind Kind>
        inline auto as() const -> HandleType<Kind> {
            assert(kind() == Kind);
            return HandleType<Kind>{ { types, index, word } };
        }

        inline VarType asVar() const;
        inline bool isVar() const;
        inline bool isntVar() const;
        inline RangeType asRange() const;
        inline bool isRange() const;
        inline bool isntRange() const;

        inline explicit operator bool() const {
            return index != InvalidType;
        }

        inline bool operator==(TypeIndex other) const {
            return index == other;
        }

        inline bool operator!=(TypeIndex other) const {
            return index != other;
        }

        inline bool contains(TypeIndex var) const;

        inline bool contains(Type type) const {
            assert(type.kind() == TypeKind::Var);
            return contains(type.index);
        }

        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func);
    };

    struct Bits {
        u32 count;

        inline Bits(u32 count_in):
            count(count_in) {}
    };

    /*
     * Section 2 - Type Definitions
     *
     * This section contains the definitions for specific derivations of Type.
     * Broadly, we define a subclass of Type for each different class of type
     * supported in the language. Some TypeKinds map onto the same class, for
     * example Void and Any are both PrimitiveTypes.
     *
     * Type should never have virtual methods, since we generally don't want
     * handles to specific Types. Instead, we expect all Type derivations to
     * implement some common methods, and the common form of the method in
     * the base Type class will call these implementations in a big switch on
     * the type kind. This lets us add special cases and fast paths for certain
     * kinds, as well as common behavior such as all types being subtypes of
     * Any.
     *
     * We also associated TypeKinds with these types using specializations of
     * the HandleTypeStruct template. This lets us map from a compile-time
     * TypeKind onto whichever structure we use to represent that type.
     */

    namespace ReservedTypes {
        constexpr TypeIndex
            Ext = 0,        // Marks that the type index was too big to store locally, so the embedder should look for an extension.

            Bottom = 1,
            Void = 2,
            Bool = 3,
            Char = 4,
            Any = 5,

            I8 = 6,             // Signed integers of various widths.
            I16 = 7,
            I32 = 8,
            I64 = 9,

            U8 = 10,            // Unsigned integers of various widths.
            U16 = 11,
            U32 = 12,
            U64 = 13,

            F32 = 14,          // Floating-point numbers.
            F64 = 15,

            String = 16,
            BottomNumber = 17, // Top and bottom numeric types used in inference.
            BottomFloat = 18,
            TopInteger = 19,
            RangeF32F64 = 20;

        constexpr TypeIndex
            FirstAbstractReservedType = BottomNumber,
            NumReservedTypes = 21;
    };

    using namespace ReservedTypes;

    struct Field {
        TypeSystem* types;
        u32 hasName : 1;
        u32 isBitField : 1;
        u32 bitFieldSize : 6;
        TypeIndex index : 24;
        Symbol name;

        inline Field(TypeSystem* types_in, TypeIndex type_in):
            types(types_in), hasName(false), isBitField(false), bitFieldSize(0), index(type_in), name() {}
        inline Field(TypeSystem* types_in, Symbol name_in, TypeIndex type_in):
            types(types_in), hasName(true), isBitField(false), bitFieldSize(0), index(type_in), name(name_in) {}
        inline Field(TypeSystem* types_in, TypeIndex type_in, Bits bits):
            types(types_in), hasName(false), isBitField(true), bitFieldSize(bits.count), index(type_in), name() {}
        inline Field(TypeSystem* types_in, Symbol name_in, TypeIndex type_in, Bits bits):
            types(types_in), hasName(true), isBitField(true), bitFieldSize(bits.count), index(type_in), name(name_in) {}
        inline Field(TypeSystem* types_in, const Type& type_in):
            Field(types_in, type_in.index) {}
        inline Field(TypeSystem* types_in, Symbol name_in, const Type& type_in):
            Field(types_in, name_in, type_in.index) {}
        inline Field(TypeSystem* types_in, const Type& type_in, Bits bits):
            Field(types_in, type_in.index, bits) {}
        inline Field(TypeSystem* types_in, Symbol name_in, const Type& type_in, Bits bits):
            Field(types_in, name_in, type_in.index, bits) {}

        inline Type type() const;
    };

    struct PrimitiveType : public Type {
        inline bool operator==(PrimitiveType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {}
    };

    template<>
    struct HandleTypeStruct<TypeKind::Bottom> { using Type = PrimitiveType; };
    template<>
    struct HandleTypeStruct<TypeKind::Void> { using Type = PrimitiveType; };
    template<>
    struct HandleTypeStruct<TypeKind::Bool> { using Type = PrimitiveType; };
    template<>
    struct HandleTypeStruct<TypeKind::Char> { using Type = PrimitiveType; };
    template<>
    struct HandleTypeStruct<TypeKind::Any> { using Type = PrimitiveType; };

    struct NumericType : public Type {
        inline bool isSigned() const;
        inline bool isFloat() const;
        inline u8 bitCount() const;
        inline bool operator==(NumericType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {}
    };

    template<>
    struct HandleTypeStruct<TypeKind::Numeric> { using Type = NumericType; };

    struct ArrayType : public Type {
        inline u32 length() const;
        inline TypeIndex elementTypeIndex() const;
        inline Type elementType() const;
        inline bool operator==(ArrayType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            elementType().forEachVar(func);
        }
    };

    struct ArrayBuilder {
        Type elementType;
        u32 length;

        inline ArrayBuilder(Type elementType_in, u32 length_in):
            elementType(elementType_in), length(length_in) {}
        inline Type build(TypeSystem* types);
    };

    template<>
    struct HandleTypeStruct<TypeKind::Array> { using Type = ArrayType; };
    template<>
    struct BuilderTypeStruct<TypeKind::Array> { using Type = ArrayBuilder; };

    struct SliceType : public Type {
        inline bool isOwn() const;
        inline bool isUninit() const;
        inline RefTraits traits() const;
        inline TypeIndex elementTypeIndex() const;
        inline Type elementType() const;
        inline bool operator==(SliceType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            elementType().forEachVar(func);
        }
    };

    template<>
    struct HandleTypeStruct<TypeKind::Slice> { using Type = SliceType; };

    struct PointerType : public Type {
        inline bool isOwn() const;
        inline bool isUninit() const;
        inline RefTraits traits() const;
        inline TypeIndex elementTypeIndex() const;
        inline Type elementType() const;
        inline bool operator==(PointerType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            elementType().forEachVar(func);
        }
    };

    template<>
    struct HandleTypeStruct<TypeKind::Pointer> { using Type = PointerType; };

    struct NamedType : public Type {
        inline Symbol name() const;
        inline TypeIndex innerTypeIndex() const;
        inline Type innerType() const;
        inline bool operator==(NamedType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        // These are used when a NamedType is a member of a UnionType.
        inline bool isCase() const;
        inline TypeIndex parentTypeIndex() const;
        inline Type parentType() const;
        inline void setParentType(TypeIndex i);

        inline ScopeIndex scope() const;
        inline void setScope(ScopeIndex scope);

        inline TypeIndex typeTag() const;
        inline void setTypeTag(TypeIndex tag);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            // Named types of all kinds shouldn't have real unification vars.
            // If there is a var here, it means we have a recursive type, and
            // forEachVar-ing it will just stack overflow.
        }
    };

    template<>
    struct HandleTypeStruct<TypeKind::Named> { using Type = NamedType; };

    struct TupleType : public Type {
        inline u32 count() const;
        inline Field field(u32 i) const;
        inline TypeIndex fieldTypeIndex(u32 i) const;
        inline Type fieldType(u32 i) const;
        inline bool fieldHasName(u32 i) const;
        inline Symbol fieldName(u32 i) const;
        inline bool operator==(TupleType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            for (u32 i = 0; i < count(); i ++)
                fieldType(i).forEachVar(func);
        }
    };

    struct TupleBuilder {
        TypeSystem* types;
        vec<Field, 8> fields;

        template<typename... Args>
        inline TupleBuilder(TypeSystem* types_in, const Args&... args):
            types(types_in) {
            add(args...);
        }

        inline void add(const TypeIndex& type) {
            fields.push(Field(types, type));
        }

        inline void add(const Type& type) {
            fields.push(Field(types, type));
        }

        inline void add(const Field& field) {
            fields.push(field);
        }

        template<typename T, u32 N, typename... Args>
        inline void add(const vec<T, N>& v, const Args&... args) {
            for (const auto& t : v)
                add(t);
            add(args...);
        }

        template<typename T, typename... Args>
        inline void add(const T& v, const Args&... args) {
            add(v);
            add(args...);
        }

        inline void add() {}

        inline Type build(TypeSystem* types);
    };

    template<>
    struct HandleTypeStruct<TypeKind::Tuple> { using Type = TupleType; };
    template<>
    struct BuilderTypeStruct<TypeKind::Tuple> { using Type = TupleBuilder; };

    struct FunctionType : public Type {
        inline u32 parameterCount() const;
        inline TypeIndex returnTypeIndex() const;
        inline Type returnType() const;
        inline Field parameter(u32 i) const;
        inline TypeIndex parameterTypeIndex(u32 i) const;
        inline Type parameterType(u32 i) const;
        inline bool parameterHasName(u32 i) const;
        inline Symbol parameterName(u32 i) const;
        inline bool operator==(FunctionType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            returnType().forEachVar(func);
            for (u32 i = 0; i < parameterCount(); i ++)
                parameterType(i).forEachVar(func);
        }
    };

    inline Type get(TypeSystem* types, TypeIndex index);

    struct FunctionBuilder {
        TypeSystem* types;
        Type returnType;
        vec<Field, 8> parameterTypes;

        template<typename... Args>
        inline FunctionBuilder(TypeSystem* types_in, TypeIndex returnType_in, const Args&... args):
            types(types_in), returnType(get(types_in, returnType_in)) {
            addParameters(args...);
        }

        template<typename... Args>
        inline FunctionBuilder(TypeSystem* types_in, Type returnType_in, const Args&... args):
            types(types_in), returnType(returnType_in) {
            addParameters(args...);
        }

        inline void addParameter(const TypeIndex& type) {
            parameterTypes.push(Field(types, type));
        }

        inline void addParameter(const Type& type) {
            parameterTypes.push(Field(types, type));
        }

        inline void addParameter(const Field& field) {
            parameterTypes.push(field);
        }

        template<typename T, u32 N, typename... Args>
        inline void addParameters(const vec<T, N>& v, const Args&... args) {
            for (const auto& t : v)
                addParameter(t);
            addParameters(args...);
        }

        template<typename T, typename... Args>
        inline void addParameters(const T& v, const Args&... args) {
            addParameter(v);
            addParameters(args...);
        }

        inline void addParameters() {}

        inline Type build(TypeSystem* types);
    };

    template<>
    struct HandleTypeStruct<TypeKind::Function> { using Type = FunctionType; };
    template<>
    struct BuilderTypeStruct<TypeKind::Function> { using Type = FunctionBuilder; };

    // ConstraintList is a special vector-like type that stores specifically a
    // linear-searched array of type indices. Unlike vectors, where we really
    // want to avoid branching on access, here we expect most variables to not
    // have many constraints, and exploit the structure of the list to achieve
    // a higher inline capacity within less space.

    struct Constraint {
        enum Kind : u32 {
            Subtype,
            Order,
            Substitute
        };

        ConstraintIndex index : 28;
        Kind kind : 4;
    };

    struct ConstraintList {
        u32 forwarded : 1;
        u32 needsRefinement : 1;
        u32 hasRefinementList : 1;
        u32 sz : 29;

        union {
            struct { u32 capacity; u8 dataPointer[sizeof(Constraint*)]; };
            Constraint items[3];
        };

        inline ConstraintList():
            forwarded(0), needsRefinement(0), hasRefinementList(0), sz(0) {}

        inline void free() {
            if (sz > 3 && !forwarded)
                delete[] data();
        }

        inline void copy(const ConstraintList& other) {
            sz = other.sz;
            forwarded = other.forwarded;
            needsRefinement = other.needsRefinement;
            hasRefinementList = other.hasRefinementList;
            if (forwarded)
                return;
            if (sz <= 3) for (u32 i = 0; i < sz; i ++)
                items[i] = other.items[i];
            else {
                capacity = other.capacity;
                store<const Constraint*>(new Constraint[capacity], dataPointer);
                for (u32 i = 0; i < sz; i ++)
                    data()[i] = other[i];
            }
        }

        inline void take(ConstraintList&& other) {
            sz = other.sz;
            forwarded = other.forwarded;
            if (forwarded)
                return;
            if (sz <= 3) for (u32 i = 0; i < sz; i ++)
                items[i] = other.items[i];
            else {
                capacity = other.capacity;
                store<const Constraint*>(other.data(), dataPointer);
                other.sz = 0;
            }
        }

        inline ~ConstraintList() {
            free();
        }

        inline ConstraintList(const ConstraintList& other) {
            copy(other);
        }

        inline ConstraintList(ConstraintList&& other) {
            take(move(other));
        }

        inline ConstraintList& operator=(const ConstraintList& other) {
            if (this == &other)
                return *this;
            free();
            copy(other);
            return *this;
        }

        inline ConstraintList& operator=(ConstraintList&& other) {
            if (this == &other)
                return *this;
            free();
            take(move(other));
            return *this;
        }

        inline Constraint* data() {
            assert(!forwarded);
            if (sz <= 3)
                return items;
            return ::load<Constraint*>(dataPointer);
        }

        inline const Constraint* data() const {
            assert(!forwarded);
            if (sz <= 3)
                return items;
            return ::load<const Constraint*>(dataPointer);
        }

        inline u32 size() const {
            return sz;
        }

        inline Constraint operator[](u32 i) const {
            assert(i < sz);
            return data()[i];
        }

        inline Constraint& operator[](u32 i) {
            assert(i < sz);
            return data()[i];
        }

        inline void grow() {
            Constraint* old = data();
            u32 oldCapacity = sz <= 3 ? 4 : capacity;
            Constraint* buf = new Constraint[oldCapacity * 2];
            memory::copy(buf, old, sz * sizeof(Constraint));
            capacity = oldCapacity * 2;
            store<const Constraint*>(buf, dataPointer);
        }

        inline void setNeedsRefinement() {
            needsRefinement = 1;
        }

        inline void unsetNeedsRefinement() {
            needsRefinement = 0;
        }

        inline bool doesNeedRefinement() const {
            return needsRefinement;
        }

        inline vec<ConstraintIndex>& refinementList(Constraints&);
        inline vec<ConstraintIndex>& ensureRefinementList(Constraints&);
        inline void dropRefinementList(Constraints&);

        inline bool add(Constraint t) {
            Constraint* dat = data();
            for (u32 i = 0; i < sz; i ++)
                if (dat[i].index == t.index && dat[i].kind == t.kind)
                    return false;
            if ((sz > 3 && sz >= capacity) || sz == 3) {
                grow();
                ::load<Constraint*>(dataPointer)[sz ++] = t;
                return true;
            }
            data()[sz ++] = t;
            return true;
        }

        inline Constraint* begin() {
            if UNLIKELY(hasRefinementList)
                return data() + 1;
            return data();
        }

        inline Constraint* end() {
            return data() + sz;
        }

        inline const Constraint* begin() const {
            if UNLIKELY(hasRefinementList)
                return data() + 1;
            return data();
        }

        inline const Constraint* end() const {
            return data() + sz;
        }

        inline void clear() {
            assert(!forwarded);
            if (sz > 3)
                delete[] data();
            sz = 0;
        }

        inline bool isForwarded() const {
            return forwarded;
        }

        inline void forwardTo(ConstraintIndex index) {
            free();
            forwarded = 1;
            sz = index;
        }

        inline maybe<ConstraintIndex> expand() const {
            if (forwarded)
                return some<ConstraintIndex>(sz);
            return none<ConstraintIndex>();
        }

        template<typename Func>
        inline void removeIf(Func&& func) {
            assert(!forwarded);

            Constraint* writer = data();
            Constraint* reader = writer;
            while (reader != end()) {
                if (func(*reader))
                    *writer ++ = *reader;
                reader ++;
            }
            if (sz <= 3) {
                sz = writer - data();
                return;
            }
            if (writer - data() <= 3) {
                // We allocated a buffer, but need to regress back to the
                // inline storage since we don't store the capacity.
                auto old = data();
                memory::copy(items, old, sizeof(Constraint) * (writer - old));
                delete[] old;
            }
            sz = writer - data();
        }
    };

    static_assert(sizeof(ConstraintList) == 16);

    struct PackedConstraintNode {
        u32 bits;

        inline PackedConstraintNode(Constraints* constraints, ConstraintIndex index);

        inline PackedConstraintNode(): bits(0xffffffffu) {}

        ConstraintIndex index() const {
            return bits & 0xffffffu;
        }

        u32 depth() const {
            return bits >> 24;
        }
    };

    // Instead of immediately modifying the state of each type variable, we
    // instead maintain a "database" of type constraints discovered by each
    // pass of the type inference algorithm. Each node in the constraint graph
    // represents a type, which doesn't have to be a variable. Each node has
    // some number of edges, which can be either:
    //
    //  - A "type" edge, where for an edge A -> B, B <: A. These are
    //    essentially equivalent to subtyping edges going the opposite
    //    direction, but we use supertype edges to simplify search later.
    //
    //  - An "order" edge, where for an edge A -> B, we need to solve A before
    //    we can solve B. In these cases, we require B is a variable with a
    //    known owner node, since we use these edges to mark nodes that can't
    //    fully encode their typechecking in subtype edges and need to rely on
    //    partial inference information to refine their types.

    struct Constraints {
        Module* module;
        TypeSystem* types;
        vec<ConstraintList, 31> constraints;
        vec<TypeIndex, 16> constrainedTypes; // Kind of a pain to have to store these too...
        vec<PackedConstraintNode, 16> outerIndices;
        pool<vec<ConstraintIndex>> refinementLists;
        u32 depth;

        inline Constraints(Module* module_in, TypeSystem* types_in, u32 depth_in):
            module(module_in), types(types_in), depth(depth_in) {}

        inline ~Constraints();

        inline ConstraintIndex index(Type type);
        inline ConstraintList& list(Type type);
        inline bool constrainType(Type subType, Type superType);
        inline bool constrainSubstitute(Type subType, Type superType);
        inline bool constrainOrder(Type before, Type after);

        inline ConstraintIndex expand(ConstraintIndex node) {
            while (auto idx = constraints[node].expand())
                node = *idx;
            return node;
        }

        inline void clear();
        void validateConstraintGraph();
    };

    inline PackedConstraintNode::PackedConstraintNode(Constraints* constraints, ConstraintIndex index) {
        assert(constraints->depth <= 255);
        bits = constraints->depth << 24 | index;
    }

    struct AST;
    struct Module;

    struct VarType : public Type {
        // These methods let us query the upper and lower bounds of a type
        // variable. They should be equal if the variable is marked equal
        // to another type.

        inline Type lowerBound() const;
        inline Type upperBound() const;
        inline UnifyResult setLowerBound(TypeIndex type);
        inline UnifyResult setUpperBound(TypeIndex type);

        inline UnifyResult setLowerBound(Type type) {
            return setLowerBound(type.index);
        }

        inline UnifyResult setUpperBound(Type type) {
            return setUpperBound(type.index);
        }

        // These methods let us check if this variable has been marked
        // "equal to" another type, and see what that type is.

        inline bool isEqual() const;
        inline Type equalType() const;
        inline void makeEqual(Type t);
        inline void replaceEqualType(Type t);
        inline void concretify();

        // These methods let us check if the variable is associated with a
        // particular AST node, and see what node that is.

        inline bool hasOwner() const;
        inline NodeIndex owner() const;
        inline AST owner(Module* module) const;

        // Helper for when we have readable variable names.

        inline u32 varNumber() const;

        inline bool operator==(VarType other) const;
        inline u64 hash() const;
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            unreachable("Should've already called the function on the var.");
        }
    };

    template<>
    struct HandleTypeStruct<TypeKind::Var> { using Type = VarType; };

    struct RangeType : public Type {
        inline TypeIndex lowerBoundIndex() const;
        inline TypeIndex upperBoundIndex() const;
        inline Type lowerBound() const;
        inline Type upperBound() const;
        inline bool operator==(RangeType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            lowerBound().forEachVar(func);
            upperBound().forEachVar(func);
        }
    };

    template<>
    struct HandleTypeStruct<TypeKind::Range> { using Type = RangeType; };

    struct StructType : public Type {
        inline Symbol name() const;
        inline u32 count() const;
        inline Field field(u32 i) const;
        inline TypeIndex fieldTypeIndex(u32 i) const;
        inline Type fieldType(u32 i) const;
        inline Symbol fieldName(u32 i) const;
        inline bool operator==(StructType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        // These are used when a StructType is a member of a UnionType.
        inline bool isCase() const;
        inline TypeIndex parentTypeIndex() const;
        inline Type parentType() const;
        inline void setParentType(TypeIndex i);

        inline ScopeIndex scope() const;
        inline void setScope(ScopeIndex scope);

        inline TypeIndex typeTag() const;
        inline void setTypeTag(TypeIndex tag);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            // Named types of all kinds shouldn't have real unification vars.
            // If there is a var here, it means we have a recursive type, and
            // forEachVar-ing it will just stack overflow.
        }
    };

    enum IsCaseTag {
        IsCase
    };

    struct StructBuilder {
        TypeSystem* types;
        Symbol name;
        vec<Field, 8> fields;
        ScopeIndex scope;
        bool isCase;

        template<typename... Args>
        inline StructBuilder(TypeSystem* types_in, Symbol name_in, const Args&... args):
            types(types_in), name(name_in) {
            add(args...);
        }

        void add(Scope* scope);

        inline void add(const TypeIndex& type) {
            fields.push(Field(types, type));
        }

        inline void add(const Type& type) {
            fields.push(Field(types, type));
        }

        inline void add(const Field& field) {
            fields.push(field);
        }

        inline void add(const IsCaseTag&) {
            isCase = true;
        }

        template<typename T, u32 N, typename... Args>
        inline void add(const vec<T, N>& v, const Args&... args) {
            for (const auto& t : v)
                add(t);
            add(args...);
        }

        template<typename T, typename... Args>
        inline void add(const T& v, const Args&... args) {
            add(v);
            add(args...);
        }

        inline void add() {}

        inline Type build(TypeSystem* types);
    };

    template<>
    struct HandleTypeStruct<TypeKind::Struct> { using Type = StructType; };
    template<>
    struct BuilderTypeStruct<TypeKind::Struct> { using Type = StructBuilder; };

    struct UnionType : public Type {
        inline Symbol name() const;
        inline u32 count() const;
        inline TypeIndex caseTypeIndex(u32 i) const;
        inline Type caseType(u32 i) const;
        inline bool operator==(UnionType other) const;
        inline bool contains(TypeIndex type) const;
        inline u64 hash() const;
        inline void concretify();
        inline UnifyResult unifyOnto(Type other, Constraints* constraints, UnifyMode mode);

        // These are used when a UnionType is a member of another UnionType.
        inline bool isCase() const;
        inline TypeIndex parentTypeIndex() const;
        inline Type parentType() const;
        inline void setParentType(TypeIndex i);

        inline ScopeIndex scope() const;
        inline void setScope(ScopeIndex scope);

        template<typename Func>
        inline void forEachVar(Func&& func) {
            // Named types of all kinds shouldn't have real unification vars.
            // If there is a var here, it means we have a recursive type, and
            // forEachVar-ing it will just stack overflow.
        }
    };

    struct UnionBuilder {
        Symbol name;
        vec<Type, 8> cases;
        ScopeIndex scope;
        bool isCase;

        inline UnionBuilder(Symbol name_in): name(name_in) {}

        void add(Scope* scope);

        inline void add(Type type) {
            cases.push(type);
        }

        inline void add(const IsCaseTag&) {
            isCase = true;
        }

        inline Type build(TypeSystem* types);
    };

    template<>
    struct HandleTypeStruct<TypeKind::Union> { using Type = UnionType; };
    template<>
    struct BuilderTypeStruct<TypeKind::Union> { using Type = UnionBuilder; };

    /*
     * Section 3 - Encoding
     *
     * In this section, we define TypeSystem, which manages the storage and
     * uniqueness of all types in a compilation. We only expect types to be
     * compared or otherwise used together if they belong to the same
     * TypeSystem.
     *
     * Types have both unique, incremental indices within a TypeSystem, of
     * which a prefix are reserved for certain primitives (see ReservedTypes
     * below); as well as an offset into the TypeWord storage in the
     * TypeSystem. The TypeWords for a given type are generated by an overload
     * of the encode() method for a specific TypeKind - think of these as
     * constructors for different Types.
     */

    namespace TypeSystemMethods {
        template<TypeKind Kind, typename... Args>
        inline u32 encode(TypeSystem& types, const Args&... args);
    }

    struct TypeKey {
        static TypeSystem* sys;

        TypeIndex index;
        mutable u32 hash;

        inline TypeKey(TypeIndex index_in);

        inline bool operator==(const TypeKey& other) const;
    };

    inline u64 hash(const TypeKey& key);

    struct TypeSystem {
        using VarCycle = vec<u32>;

        vec<TypeWord, 64> words;
        vec<u32, 64> typeList;
        vec<PackedConstraintNode, 64> constraintNodes;
        set<TypeKey> typeSet;
        Compilation* compilation;
        SymbolTable* symbols;
        vec<TypeIndex> vars;

        TypeIndex signedTypeCache[66];
        TypeIndex unsignedTypeCache[65];
        TypeIndex signedRangeCache[66];
        TypeIndex unsignedRangeCache[65];

        inline TypeSystem(Compilation* compilation_in):
            compilation(compilation_in), symbols(&compilation->symbols) {
            typeList.push(0); // Ext
            constraintNodes.push({});

            type_assert(encode<TypeKind::Bottom>().index == ReservedTypes::Bottom);
            type_assert(encode<TypeKind::Void>().index == ReservedTypes::Void);
            type_assert(encode<TypeKind::Bool>().index == ReservedTypes::Bool);
            type_assert(encode<TypeKind::Char>().index == ReservedTypes::Char);
            type_assert(encode<TypeKind::Any>().index == ReservedTypes::Any);

            type_assert(encode<TypeKind::Numeric>(true, false, 8u).index == ReservedTypes::I8);
            type_assert(encode<TypeKind::Numeric>(true, false, 16u).index == ReservedTypes::I16);
            type_assert(encode<TypeKind::Numeric>(true, false, 32u).index == ReservedTypes::I32);
            type_assert(encode<TypeKind::Numeric>(true, false, 64u).index == ReservedTypes::I64);

            type_assert(encode<TypeKind::Numeric>(false, false, 8u).index == ReservedTypes::U8);
            type_assert(encode<TypeKind::Numeric>(false, false, 16u).index == ReservedTypes::U16);
            type_assert(encode<TypeKind::Numeric>(false, false, 32u).index == ReservedTypes::U32);
            type_assert(encode<TypeKind::Numeric>(false, false, 64u).index == ReservedTypes::U64);

            type_assert(encode<TypeKind::Numeric>(true, true, 32u).index == ReservedTypes::F32);
            type_assert(encode<TypeKind::Numeric>(true, true, 64u).index == ReservedTypes::F64);
            type_assert(encode<TypeKind::Slice>(get(I8)).index == ReservedTypes::String);

            type_assert(encode<TypeKind::Numeric>(false, false, 0u).index == ReservedTypes::BottomNumber);
            type_assert(encode<TypeKind::Numeric>(true, true, 0u).index == ReservedTypes::BottomFloat);
            type_assert(encode<TypeKind::Numeric>(true, false, 65u).index == ReservedTypes::TopInteger);
            type_assert(encode<TypeKind::Range>(get(F32), get(F64)).index == ReservedTypes::RangeF32F64);

            for (u32 i = 0; i < 66; i ++)
                signedTypeCache[i] = unsignedTypeCache[i] = signedRangeCache[i] = unsignedRangeCache[i] = InvalidType;
            signedTypeCache[8] = ReservedTypes::I8;
            signedTypeCache[16] = ReservedTypes::I16;
            signedTypeCache[32] = ReservedTypes::I32;
            signedTypeCache[64] = ReservedTypes::I64;
            signedTypeCache[65] = ReservedTypes::TopInteger;

            unsignedTypeCache[0] = ReservedTypes::BottomNumber;
            unsignedTypeCache[8] = ReservedTypes::U8;
            unsignedTypeCache[16] = ReservedTypes::U16;
            unsignedTypeCache[32] = ReservedTypes::U32;
            unsignedTypeCache[64] = ReservedTypes::U64;
        }

        inline static constexpr bool shouldntHash(TypeKind kind) {
            // These types are always unique when we define them, so it's a
            // waste of time to hash them and enter them into the type map.

            switch (kind) {
                case TypeKind::Var:
                case TypeKind::Struct:
                case TypeKind::Named:
                case TypeKind::Union:
                    return true;
                default:
                    return false;
            }
        }

        template<TypeKind Kind, typename... Args>
        inline auto encode(const Args&... args) -> HandleType<Kind> {
            u32 t = TypeSystemMethods::encode<Kind>(*this, forward<decltype(args)>(args)...);
            typeList.push(t);
            if constexpr (shouldntHash(Kind)) {
                constraintNodes.push({});
                return HandleType<Kind> { { this, typeList.size() - 1, typeList.back() } };
            }
            TypeKey::sys = this;
            TypeKey key = TypeKey { typeList.size() - 1 };
            auto it = typeSet.find(key);
            if (it == typeSet.end()) {
                typeSet.insert(key);
                constraintNodes.push({});
                return HandleType<Kind> { { this, typeList.size() - 1, typeList.back() } };
            }
            typeList.pop();
            words.trim(words.size() - t);
            return HandleType<Kind> { { this, it->index, typeList[it->index] } };
        }

        inline Type signedType(u32 bits) {
            assert(bits >= 0 && bits <= 65);
            if (signedTypeCache[bits] != InvalidType)
                return get(signedTypeCache[bits]);
            Type result = encode<TypeKind::Numeric>(true, false, bits);
            signedTypeCache[bits] = result.index;
            return result;
        }

        inline Type unsignedType(u32 bits) {
            assert(bits >= 0 && bits <= 64);
            if (unsignedTypeCache[bits] != InvalidType)
                return get(unsignedTypeCache[bits]);
            Type result = encode<TypeKind::Numeric>(false, false, bits);
            unsignedTypeCache[bits] = result.index;
            return result;
        }

        inline Type signedRange(u32 bits) {
            assert(bits >= 0 && bits <= 65);
            if (signedRangeCache[bits] != InvalidType)
                return get(signedRangeCache[bits]);
            Type result = encode<TypeKind::Range>(signedType(bits), get(F64));
            signedRangeCache[bits] = result.index;
            return result;
        }

        inline Type unsignedRange(u32 bits) {
            assert(bits >= 0 && bits <= 64);
            if (unsignedRangeCache[bits] != InvalidType)
                return get(unsignedRangeCache[bits]);
            Type result = encode<TypeKind::Range>(unsignedType(bits), get(F64));
            unsignedRangeCache[bits] = result.index;
            return result;
        }

        inline Type invalidType() {
            return { this, InvalidType, 0 };
        }

        inline Type get(TypeIndex index) {
            if (index == InvalidType)
                return invalidType();
            return { this, index, typeList[index] };
        }

        template<TypeKind Kind>
        inline auto get(TypeIndex index) -> HandleType<Kind> {
            return get(index).as<Kind>();
        }

        inline VarType var() {
            return encode<TypeKind::Var>();
        }

        inline VarType var(const Type& bottom, const Type& top) {
            return encode<TypeKind::Var>(bottom, top);
        }
    };

    namespace TypeSystemMethods {
        template<>
        inline u32 encode<TypeKind::Bottom>(TypeSystem& sys) {
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Bottom;
            word.isConcrete = true;
            sys.words.push(word);
            return sys.words.size() - 1;
        }

        template<>
        inline u32 encode<TypeKind::Void>(TypeSystem& sys) {
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Void;
            word.isConcrete = true;
            sys.words.push(word);
            return sys.words.size() - 1;
        }

        template<>
        inline u32 encode<TypeKind::Bool>(TypeSystem& sys) {
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Bool;
            word.isConcrete = true;
            sys.words.push(word);
            return sys.words.size() - 1;
        }

        template<>
        inline u32 encode<TypeKind::Char>(TypeSystem& sys) {
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Char;
            word.isConcrete = true;
            sys.words.push(word);
            return sys.words.size() - 1;
        }

        template<>
        inline u32 encode<TypeKind::Any>(TypeSystem& sys) {
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Any;
            word.isConcrete = true;
            sys.words.push(word);
            return sys.words.size() - 1;
        }

        template<>
        inline u32 encode<TypeKind::Numeric>(TypeSystem& sys, const bool& isSigned, const bool& isFloat, const u32& bitCount) {
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Numeric;
            word.isConcrete = true;
            word.isSigned = isSigned || isFloat; // All floats are signed.
            word.isFloat = isFloat;
            word.bitCount = bitCount;
            sys.words.push(word);
            return sys.words.size() - 1;
        }

        template<>
        inline u32 encode<TypeKind::Array>(TypeSystem& sys, const TypeIndex& elementType, const u32& length) {
            TypeWord word;
            word.bits = 0;
            word.isConcrete = sys.get(elementType).isConcrete();
            word.kind = TypeKind::Array;
            if (length < TypeWord::MaxCompactArrayLength && elementType < TypeWord::MaxCompactArrayType) {
                word.isCompactArray = true;
                word.compactElement = elementType;
                word.compactLength = length;
            } else {
                word.isCompactArray = false;
                word.extElement = elementType;
            }
            sys.words.push(word);
            if (!word.isCompactArray)
                sys.words.push({ .extLength = length });
            return sys.words.size() - (word.isCompactArray ? 1 : 2);
        }

        template<>
        inline u32 encode<TypeKind::Pointer>(TypeSystem& sys, const RefTraits& traits, const TypeIndex& elementType) {
            TypeWord word;
            word.bits = 0;
            word.isConcrete = sys.get(elementType).isConcrete();
            word.kind = TypeKind::Pointer;
            word.refTraits = traits;
            word.extElement = elementType;
            sys.words.push(word);
            return sys.words.size() - 1;
        }

        template<>
        inline u32 encode<TypeKind::Pointer>(TypeSystem& sys, const TypeIndex& elementType) {
            return encode<TypeKind::Pointer>(sys, RefTraits::Unowned, elementType);
        }

        template<>
        inline u32 encode<TypeKind::Slice>(TypeSystem& sys, const RefTraits& traits, const TypeIndex& elementType) {
            TypeWord word;
            word.bits = 0;
            word.isConcrete = sys.get(elementType).isConcrete();
            word.kind = TypeKind::Slice;
            word.refTraits = traits;
            word.extElement = elementType;
            sys.words.push(word);
            return sys.words.size() - 1;
        }

        template<>
        inline u32 encode<TypeKind::Slice>(TypeSystem& sys, const TypeIndex& elementType) {
            return encode<TypeKind::Slice>(sys, Unowned, elementType);
        }

        template<>
        inline u32 encode<TypeKind::Named>(TypeSystem& sys, const Symbol& name, const ScopeIndex& scope, const TypeIndex& inner) {
            u32 index = sys.words.size();
            TypeWord word;
            word.bits = 0;
            word.isConcrete = true;
            word.kind = TypeKind::Named;
            word.extElement = inner;
            sys.words.push(word);
            sys.words.push({ .name = name.symbol });
            sys.words.push({ .scope = scope });
            return index;
        }

        template<>
        inline u32 encode<TypeKind::Named>(TypeSystem& sys, const Symbol& name, const ScopeIndex& scope, const TypeIndex& inner, const IsCaseTag&) {
            u32 index = sys.words.size();
            TypeWord word;
            word.bits = 0;
            word.isConcrete = true;
            word.isCase = true;
            word.kind = TypeKind::Named;
            word.extElement = inner;
            sys.words.push(word);
            sys.words.push({ .name = name.symbol });
            sys.words.push({ .scope = scope }); // Scope
            sys.words.push({ .typeBound = InvalidType }); // Parent type
            sys.words.push({ .tag = InvalidType }); // Tag
            return index;
        }

        template<>
        inline u32 encode<TypeKind::Array>(TypeSystem& sys, const Type& elementType, const u32& length) {
            return encode<TypeKind::Array>(sys, elementType.index, length);
        }

        template<>
        inline u32 encode<TypeKind::Pointer>(TypeSystem& sys, const RefTraits& traits, const Type& elementType) {
            return encode<TypeKind::Pointer>(sys, traits, elementType.index);
        }

        template<>
        inline u32 encode<TypeKind::Slice>(TypeSystem& sys, const RefTraits& traits, const Type& elementType) {
            return encode<TypeKind::Slice>(sys, traits, elementType.index);
        }

        template<>
        inline u32 encode<TypeKind::Pointer>(TypeSystem& sys, const Type& elementType) {
            return encode<TypeKind::Pointer>(sys, elementType.index);
        }

        template<>
        inline u32 encode<TypeKind::Slice>(TypeSystem& sys, const Type& elementType) {
            return encode<TypeKind::Slice>(sys, elementType.index);
        }

        template<>
        inline u32 encode<TypeKind::Named>(TypeSystem& sys, const Symbol& name, const ScopeIndex& scope, const Type& inner) {
            return encode<TypeKind::Named>(sys, name, scope, inner.index);
        }

        template<>
        inline u32 encode<TypeKind::Named>(TypeSystem& sys, const Symbol& name, const ScopeIndex& scope, const Type& inner, const IsCaseTag&) {
            return encode<TypeKind::Named>(sys, name, scope, inner.index, IsCase);
        }

        template<>
        inline u32 encode<TypeKind::Tuple>(TypeSystem& sys, const TupleBuilder& builder) {
            u32 index = sys.words.size();
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Tuple;
            word.isConcrete = true;
            for (const Field& f : builder.fields) if (!sys.get(f.index).isConcrete()) {
                word.isConcrete = false;
                break;
            }
            word.fieldCount = builder.fields.size();
            sys.words.push(word);
            for (const Field& f : builder.fields) {
                sys.words.push({ .hasName = f.hasName, .isBitField = f.isBitField, .bitFieldSize = f.bitFieldSize, .fieldType = f.index });
                sys.words.push({ .name = f.hasName ? f.name.symbol : InvalidSymbol });
            }
            return index;
        }

        template<>
        inline u32 encode<TypeKind::Function>(TypeSystem& sys, const FunctionBuilder& builder) {
            u32 index = sys.words.size();
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Function;
            word.isConcrete = builder.returnType.isConcrete();
            if (word.isConcrete) for (const Field& f : builder.parameterTypes) if (!sys.get(f.index).isConcrete()) {
                word.isConcrete = false;
                break;
            }
            word.fieldCount = builder.parameterTypes.size();
            sys.words.push(word);
            TypeWord returnType;
            returnType.bits = 0;
            returnType.fieldType = builder.returnType.index;
            sys.words.push(returnType);
            for (const Field& f : builder.parameterTypes) {
                sys.words.push({ .hasName = f.hasName, .isBitField = f.isBitField, .bitFieldSize = f.bitFieldSize, .fieldType = f.index });
                sys.words.push({ .name = f.hasName ? f.name.symbol : InvalidSymbol });
            }
            return index;
        }

        template<>
        inline u32 encode<TypeKind::Var>(TypeSystem& sys, const Type& bottom, const Type& top) {
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Var;
            word.isConcrete = false;
            word.typeBound = bottom.index;
            sys.words.push(word);

            word.bits = 0;
            word.markedEqual = false;
            word.hasOwner = false;
            word.typeBound = top.index;
            sys.words.push(word);

            if UNLIKELY(config::readableTypeVars) {
                word.bits = sys.vars.size();
                sys.vars.push(sys.typeList.size());
                sys.words.push(word);
                return sys.words.size() - 3;
            }

            return sys.words.size() - 2;
        }

        template<>
        inline u32 encode<TypeKind::Var>(TypeSystem& sys) {
            return encode<TypeKind::Var>(sys, sys.get(Bottom), sys.get(Any));
        }

        template<>
        inline u32 encode<TypeKind::Var>(TypeSystem& sys, const Type& bottom, const Type& top, const NodeIndex& owner) {
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Var;
            word.isConcrete = false;
            word.typeBound = bottom.index;
            sys.words.push(word);

            word.bits = 0;
            word.markedEqual = false;
            word.hasOwner = true;
            word.typeBound = top.index;
            sys.words.push(word);

            word.owner = owner;
            sys.words.push(word);

            if UNLIKELY(config::readableTypeVars) {
                word.bits = sys.vars.size();
                sys.vars.push(sys.typeList.size());
                sys.words.push(word);
                return sys.words.size() - 4;
            }

            return sys.words.size() - 3;
        }

        template<>
        inline u32 encode<TypeKind::Var>(TypeSystem& sys, const NodeIndex& owner) {
            return encode<TypeKind::Var>(sys, sys.get(Bottom), sys.get(Any), owner);
        }

        template<>
        inline u32 encode<TypeKind::Range>(TypeSystem& sys, const Type& bottom, const Type& top) {
            assert(bottom.isConcrete());
            assert(top.isConcrete());

            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Range;
            word.isConcrete = true;
            word.typeBound = bottom.index;
            sys.words.push(word);

            word.bits = 0;
            word.typeBound = top.index;
            sys.words.push(word);
            return sys.words.size() - 2;
        }

        template<>
        inline u32 encode<TypeKind::Struct>(TypeSystem& sys, const StructBuilder& builder) {
            u32 index = sys.words.size();
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Struct;
            word.isConcrete = true;
            word.isCase = builder.isCase;
            word.fieldCount = builder.fields.size();
            sys.words.push(word);
            sys.words.push({ .name = builder.name.symbol });
            sys.words.push({ .scope = builder.scope });
            for (const Field& f : builder.fields) {
                sys.words.push({ .hasName = f.hasName, .isBitField = f.isBitField, .bitFieldSize = f.bitFieldSize, .fieldType = f.index });
                sys.words.push({ .name = f.hasName ? f.name.symbol : InvalidSymbol });
            }
            if (builder.isCase) {
                sys.words.push({ .typeBound = InvalidType }); // Parent type
                sys.words.push({ .tag = InvalidType }); // Type tag
            }
            return index;
        }

        template<>
        inline u32 encode<TypeKind::Union>(TypeSystem& sys, const UnionBuilder& builder) {
            u32 index = sys.words.size();
            TypeIndex typeIndex = sys.typeList.size();
            TypeWord word;
            word.bits = 0;
            word.kind = TypeKind::Union;
            word.isConcrete = true;
            word.isCase = builder.isCase;
            word.fieldCount = builder.cases.size();
            sys.words.push(word);
            sys.words.push({ .name = builder.name.symbol });
            sys.words.push({ .scope = builder.scope });
            for (const Type& t : builder.cases) {
                TypeWord w;
                w.bits = 0;
                w.fieldType = t.index;
                sys.words.push(w);

                if (t.is<TypeKind::Named>())
                    t.as<TypeKind::Named>().setParentType(typeIndex);
                else if (t.is<TypeKind::Struct>())
                    t.as<TypeKind::Struct>().setParentType(typeIndex);
                else if (t.is<TypeKind::Union>())
                    t.as<TypeKind::Union>().setParentType(typeIndex);
                else
                    unreachable("Tried to set parent type of non-case type ", t);
            }
            if (builder.isCase)
                sys.words.push({ .typeBound = InvalidType });
            return index;
        }
    }

    /*
     * Section 4 - Type Methods
     *
     * This section contains the implementation of the methods defined for
     * each Type subclass earlier, as well as a few useful top-level functions
     * for working with Types.
     */

    inline Type expand(Type t) {
        // Expands any equality relations if t is a var.

        if LIKELY(!t.isVar() || !t.asVar().isEqual())
            return t;

        return t.asVar().equalType();
    }

    inline Type lowerBound(Type t) {
        if (t.isVar())
            return t.asVar().lowerBound();
        if (t.isRange())
            return t.asRange().lowerBound();
        return t;
    }

    inline Type upperBound(Type t) {
        if (t.isVar())
            return t.asVar().upperBound();
        if (t.isRange())
            return t.asRange().upperBound();
        return t;
    }

    inline bool operator==(Type a, Type b) {
        if (a.isVar())
            a = expand(a);
        if (b.isVar())
            b = expand(b);
        if (a.kind() != b.kind())
            return false;
        switch (a.kind()) {
            case TypeKind::Bottom:
                return a.as<TypeKind::Bottom>() == b.as<TypeKind::Bottom>();
            case TypeKind::Void:
                return a.as<TypeKind::Void>() == b.as<TypeKind::Void>();
            case TypeKind::Bool:
                return a.as<TypeKind::Bool>() == b.as<TypeKind::Bool>();
            case TypeKind::Char:
                return a.as<TypeKind::Char>() == b.as<TypeKind::Char>();
            case TypeKind::Numeric:
                return a.as<TypeKind::Numeric>() == b.as<TypeKind::Numeric>();
            case TypeKind::Pointer:
                return a.as<TypeKind::Pointer>() == b.as<TypeKind::Pointer>();
            case TypeKind::Slice:
                return a.as<TypeKind::Slice>() == b.as<TypeKind::Slice>();
            case TypeKind::Array:
                return a.as<TypeKind::Array>() == b.as<TypeKind::Array>();
            case TypeKind::Tuple:
                return a.as<TypeKind::Tuple>() == b.as<TypeKind::Tuple>();
            case TypeKind::Function:
                return a.as<TypeKind::Function>() == b.as<TypeKind::Function>();
            case TypeKind::Any:
                return a.as<TypeKind::Any>() == b.as<TypeKind::Any>();
            case TypeKind::Var:
                return a.asVar() == b.asVar();
            case TypeKind::Range:
                return a.as<TypeKind::Range>() == b.as<TypeKind::Range>();
            case TypeKind::Named:
                return a.as<TypeKind::Named>() == b.as<TypeKind::Named>();
            case TypeKind::Struct:
                return a.as<TypeKind::Struct>() == b.as<TypeKind::Struct>();
            case TypeKind::Union:
                return a.as<TypeKind::Union>() == b.as<TypeKind::Union>();
        }
    }

    inline bool operator!=(const Type& a, const Type& b) {
        return !(a == b);
    }

    inline u64 hash(const Type& type) {
        switch (type.kind()) {
            case TypeKind::Bottom:
                return type.as<TypeKind::Bottom>().hash();
            case TypeKind::Void:
                return type.as<TypeKind::Void>().hash();
            case TypeKind::Bool:
                return type.as<TypeKind::Bool>().hash();
            case TypeKind::Char:
                return type.as<TypeKind::Char>().hash();
            case TypeKind::Numeric:
                return type.as<TypeKind::Numeric>().hash();
            case TypeKind::Pointer:
                return type.as<TypeKind::Pointer>().hash();
            case TypeKind::Slice:
                return type.as<TypeKind::Slice>().hash();
            case TypeKind::Array:
                return type.as<TypeKind::Array>().hash();
            case TypeKind::Tuple:
                return type.as<TypeKind::Tuple>().hash();
            case TypeKind::Function:
                return type.as<TypeKind::Function>().hash();
            case TypeKind::Any:
                return type.as<TypeKind::Any>().hash();
            case TypeKind::Var:
                return type.asVar().hash();
            case TypeKind::Range:
                return type.as<TypeKind::Range>().hash();
            case TypeKind::Named:
                return type.as<TypeKind::Named>().hash();
            case TypeKind::Struct:
                return type.as<TypeKind::Struct>().hash();
            case TypeKind::Union:
                return type.as<TypeKind::Union>().hash();
        }
    }

    inline Type get(TypeSystem* types, TypeIndex index) {
        return types->get(index);
    }

    // TypeKey

    inline TypeKey::TypeKey(TypeIndex index_in):
        index(index_in), hash(clover::hash(sys->get(index))) {}

    inline bool TypeKey::operator==(const TypeKey& other) const {
        if (hash != other.hash)
            return false;
        return Type { sys, index, sys->typeList[index] } == Type { sys, other.index, sys->typeList[other.index] };
    }

    inline u64 hash(const TypeKey& key) {
        return key.hash;
    }

    // Type

    inline TypeWord& Type::firstWord() const {
        return types->words[word];
    }

    inline TypeWord& Type::nthWord(u32 i) const {
        return types->words[word + i];
    }

    inline TypeKind Type::kind() const {
        return firstWord().kind;
    }

    inline u64 Type::kindHash() const {
        return intHash(u64(kind()));
    }

    inline bool Type::hasName() const {
        return u32(kind()) >= u32(TypeKind::FirstNamed);
    }

    inline bool Type::isConcrete() const {
        return firstWord().isConcrete;
    }

    inline VarType Type::asVar() const {
        return as<TypeKind::Var>();
    }

    inline bool Type::isVar() const {
        return is<TypeKind::Var>();
    }

    inline bool Type::isntVar() const {
        return isnt<TypeKind::Var>();
    }

    inline RangeType Type::asRange() const {
        return as<TypeKind::Range>();
    }

    inline bool Type::isRange() const {
        return is<TypeKind::Range>();
    }

    inline bool Type::isntRange() const {
        return isnt<TypeKind::Range>();
    }

    inline bool Type::contains(TypeIndex var) const {
        assert(types->get(var).isVar());
        if (isConcrete())
            return false;
        if (index == var)
            return true;
        switch(kind()) {
            case TypeKind::Bottom:
                return as<TypeKind::Bottom>().contains(var);
            case TypeKind::Void:
                return as<TypeKind::Void>().contains(var);
            case TypeKind::Bool:
                return as<TypeKind::Bool>().contains(var);
            case TypeKind::Char:
                return as<TypeKind::Char>().contains(var);
            case TypeKind::Any:
                return as<TypeKind::Any>().contains(var);
            case TypeKind::Numeric:
                return as<TypeKind::Numeric>().contains(var);
            case TypeKind::Pointer:
                return as<TypeKind::Pointer>().contains(var);
            case TypeKind::Slice:
                return as<TypeKind::Slice>().contains(var);
            case TypeKind::Array:
                return as<TypeKind::Array>().contains(var);
            case TypeKind::Tuple:
                return as<TypeKind::Tuple>().contains(var);
            case TypeKind::Function:
                return as<TypeKind::Function>().contains(var);
            case TypeKind::Var:
                return asVar().contains(var);
            case TypeKind::Range:
                return asRange().contains(var);
            case TypeKind::Named:
                return as<TypeKind::Named>().contains(var);
            case TypeKind::Struct:
                return as<TypeKind::Struct>().contains(var);
            case TypeKind::Union:
                return as<TypeKind::Union>().contains(var);
        }
    }

    inline void Type::concretify() {
        switch(kind()) {
            case TypeKind::Bottom:
                as<TypeKind::Bottom>().concretify();
                break;
            case TypeKind::Void:
                as<TypeKind::Void>().concretify();
                break;
            case TypeKind::Bool:
                as<TypeKind::Bool>().concretify();
                break;
            case TypeKind::Char:
                as<TypeKind::Char>().concretify();
                break;
            case TypeKind::Any:
                as<TypeKind::Any>().concretify();
                break;
            case TypeKind::Numeric:
                as<TypeKind::Numeric>().concretify();
                break;
            case TypeKind::Pointer:
                as<TypeKind::Pointer>().concretify();
                break;
            case TypeKind::Slice:
                as<TypeKind::Slice>().concretify();
                break;
            case TypeKind::Array:
                as<TypeKind::Array>().concretify();
                break;
            case TypeKind::Tuple:
                as<TypeKind::Tuple>().concretify();
                break;
            case TypeKind::Function:
                as<TypeKind::Function>().concretify();
                break;
            case TypeKind::Var:
                asVar().concretify();
                break;
            case TypeKind::Range:
                as<TypeKind::Range>().concretify();
                break;
            case TypeKind::Named:
                as<TypeKind::Named>().concretify();
                break;
            case TypeKind::Struct:
                as<TypeKind::Struct>().concretify();
                break;
            case TypeKind::Union:
                as<TypeKind::Union>().concretify();
                break;
        }
    }

    inline Type leastCommonSupertype(Type, Type, UnifyMode);
    inline Type greatestCommonSubtype(Type, Type, UnifyMode);

    inline maybe<UnifyResult> tryConstrainLocally(Type sub, Type super, UnifyMode substituteFlag) {
        // This function serves as a pretty critical optimization: if we are
        // some variable and can immediately resolve its type bound without any
        // propagative effects, i.e. we know the variable bounds and the types
        // we're refining it with are both concrete, then we can update it
        // right away instead of waiting for constraint reduction. Even more
        // than the fact we're saving more expensive constraint operations is
        // the fact that this often means we can easily refine chains of
        // operations that would otherwise require multiple passes to fully
        // propagate up.

        assert(substituteFlag == MustSubstitute || !substituteFlag);

        sub = expand(sub);
        super = expand(super);
        Type subl = expand(sub.isVar() ? sub.asVar().lowerBound() : sub), subu = expand(sub.isVar() ? sub.asVar().upperBound() : sub);
        Type superl = expand(super.isVar() ? super.asVar().lowerBound() : super), superu = expand(super.isVar() ? super.asVar().upperBound() : super);

        if (sub.isVar() && subu.isConcrete() && super.isConcrete()) {
            auto ub = greatestCommonSubtype(subu, super, substituteFlag);
            if (!ub)
                return none<UnifyResult>();
            if (config::verboseUnify >= 2)
                println("[TYPE]\tRefined upper bound of ", sub, " from ", subu, " to ", ub);
            return some<UnifyResult>(sub.asVar().setUpperBound(ub));
        } else if (super.isVar() && superl.isConcrete() && sub.isConcrete()) {
            auto lb = leastCommonSupertype(sub, superl, substituteFlag);
            if (!lb)
                return none<UnifyResult>();
            if (config::verboseUnify >= 2)
                println("[TYPE]\tRefined lower bound of ", super, " from ", superl, " to ", lb);
            return some<UnifyResult>(super.asVar().setLowerBound(lb));
        }
        return none<UnifyResult>();
    }

    inline bool isCaseTypeKind(TypeKind kind) {
        constexpr u32 CaseTypeKinds = 0
            | 1u << u32(TypeKind::Named)
            | 1u << u32(TypeKind::Struct)
            | 1u << u32(TypeKind::Union);

        return 1u << u32(kind) & CaseTypeKinds;
    }

    inline bool isCase(Type type) {
        if (!isCaseTypeKind(type.kind()))
            return false;
        return type.firstWord().isCase;
    }

    inline bool isAtom(Type type) {
        return type.is<TypeKind::Named>() && type.as<TypeKind::Named>().innerType() == Void;
    }

    inline UnifyResult Type::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        if (config::verboseUnify >= 2)
            println("[TYPE]\tUnifying ", *this, " onto ", other);

        other = expand(other);
        if (index == other.index)
            return UnifySuccess;

        if (isVar() && asVar().isEqual())
            return expand(*this).unifyOnto(other, constraints, mode);

        // If the other type is a range, act as if it's the upper bound.

        if (other.isRange())
            other = other.asRange().upperBound();

        // If we're a range, act as if we're the lower bound.

        if (isRange())
            return asRange().lowerBound().unifyOnto(other, constraints, mode);

        // If either of us are variables, then:
        //
        //  - If we are doing an in-place unification, then we update our
        //    bounds as appropriate.
        //
        //  - If we are okay doing effectful unification, then we just add an
        //    edge to the constraint graph - it'll be figured out later.
        //
        //  - If we are just querying the current bounds, then any involved
        //    variables just act like their lower/upper bounds.

        bool hasAnyVar = isVar() || other.isVar();
        if ((mode & ModeMask) == InPlace && hasAnyVar) {
            if (isVar()) {
                Type upper = greatestCommonSubtype(asVar().upperBound(), other.isVar() ? other.asVar().upperBound() : other, mode & MustSubstitute);
                if (!upper)
                    return UnifyFailure;
                if UNLIKELY(config::verboseUnify >= 3)
                    println("[TYPE]\tUpdating upper bound of variable ", *this, " to ", upper, " in-place");
                if (mode & MustSubstitute && !isCaseTypeKind(upper.kind())) {
                    asVar().makeEqual(other);
                    return UnifySuccess; // Important to return early to avoid creating cycle.
                } else
                    asVar().setUpperBound(upper);
            }
            if (other.isVar()) {
                Type lower = leastCommonSupertype(isVar() ? asVar().lowerBound() : *this, other.asVar().lowerBound(), mode & MustSubstitute);
                if (!lower)
                    return UnifyFailure;
                if UNLIKELY(config::verboseUnify >= 3)
                    println("[TYPE]\tUpdating lower bound of variable ", other, " to ", lower, " in-place");
                if (mode & MustSubstitute && !isCaseTypeKind(lower.kind())) {
                    other.asVar().makeEqual(*this);
                    return UnifySuccess;
                } else
                    other.asVar().setLowerBound(lower);
            }
            return UnifySuccess;
        }

        if ((mode & ModeMask) == Constraining && hasAnyVar) {
            if (auto result = tryConstrainLocally(*this, other, mode & MustSubstitute))
                return *result;
            if (index == other.index)
                return UnifySuccess;
            if (mode & MustSubstitute)
                constraints->constrainSubstitute(*this, other);
            else
                constraints->constrainType(*this, other);
            return UnifySuccess;
        } else if ((mode & ModeMask) == Query) {
            if (other.isVar())
                other = other.asVar().upperBound();
            if (isVar()) {
                auto result = asVar().lowerBound().unifyOnto(other, constraints, mode);
                if (!result && mode & MustSubstitute) {
                    // We aren't a substitutable subtype. But we might be able
                    // to make ourselves equal to the other type.
                    return asVar().lowerBound().unifyOnto(other, constraints, Query)
                        & other.unifyOnto(asVar().upperBound(), constraints, Query);
                }
                return result;
            }
        }

        // Now we shouldn't be a variable, and we shouldn't be unifying onto a
        // variable. We defer to each individual type kind to handle its own
        // subtyping/unification logic.

        assert(!isVar());
        assert(!other.isVar());

        // The bottom type can unify onto anything.

        if (*this == Bottom)
            return UnifySuccess;

        // All types can unify onto the top type.

        if (other == Any)
            return UnifySuccess;

        switch(kind()) {
            case TypeKind::Bottom:
                unreachable("Should have already exited early.");
            case TypeKind::Void:
                return as<TypeKind::Void>().unifyOnto(other, constraints, mode);
            case TypeKind::Bool:
                return as<TypeKind::Bool>().unifyOnto(other, constraints, mode);
            case TypeKind::Char:
                return as<TypeKind::Char>().unifyOnto(other, constraints, mode);
            case TypeKind::Any:
                return as<TypeKind::Any>().unifyOnto(other, constraints, mode);
            case TypeKind::Numeric:
                return as<TypeKind::Numeric>().unifyOnto(other, constraints, mode);
            case TypeKind::Pointer:
                return as<TypeKind::Pointer>().unifyOnto(other, constraints, mode);
            case TypeKind::Slice:
                return as<TypeKind::Slice>().unifyOnto(other, constraints, mode);
            case TypeKind::Array:
                return as<TypeKind::Array>().unifyOnto(other, constraints, mode);
            case TypeKind::Tuple:
                return as<TypeKind::Tuple>().unifyOnto(other, constraints, mode);
            case TypeKind::Function:
                return as<TypeKind::Function>().unifyOnto(other, constraints, mode);
            case TypeKind::Var:
                unreachable("Should have handled type variable already.");
            case TypeKind::Range:
                return as<TypeKind::Range>().unifyOnto(other, constraints, mode);
            case TypeKind::Named:
                return as<TypeKind::Named>().unifyOnto(other, constraints, mode);
            case TypeKind::Struct:
                return as<TypeKind::Struct>().unifyOnto(other, constraints, mode);
            case TypeKind::Union:
                return as<TypeKind::Union>().unifyOnto(other, constraints, mode);
        }
    }

    template<typename Func>
    inline void Type::forEachVar(Func&& func) {
        if (isVar()) {
            func(*this);
            return;
        }
        switch (kind()) {
            case TypeKind::Bottom:
            case TypeKind::Void:
            case TypeKind::Bool:
            case TypeKind::Char:
            case TypeKind::Any:
            case TypeKind::Numeric:
                return;
            case TypeKind::Pointer:
                return as<TypeKind::Pointer>().forEachVar(func);
            case TypeKind::Slice:
                return as<TypeKind::Slice>().forEachVar(func);
            case TypeKind::Array:
                return as<TypeKind::Array>().forEachVar(func);
            case TypeKind::Tuple:
                return as<TypeKind::Tuple>().forEachVar(func);
            case TypeKind::Function:
                return as<TypeKind::Function>().forEachVar(func);
            case TypeKind::Var:
                unreachable("Should have handled type variable already.");
            case TypeKind::Range:
                return as<TypeKind::Range>().forEachVar(func);
            case TypeKind::Named:
                return as<TypeKind::Named>().forEachVar(func);
            case TypeKind::Struct:
                return as<TypeKind::Struct>().forEachVar(func);
            case TypeKind::Union:
                return as<TypeKind::Union>().forEachVar(func);
        }
    }

    inline u32 Type::numWords() const {
        switch (kind()) {
            case TypeKind::Bottom:
            case TypeKind::Any:
            case TypeKind::Void:
            case TypeKind::Bool:
            case TypeKind::Char:
            case TypeKind::Numeric:
            case TypeKind::Pointer:
            case TypeKind::Slice:
                return 1;
            case TypeKind::Array:
                return 2 - firstWord().isCompactArray;
            case TypeKind::Named:
                return 3 + firstWord().isCase;
            case TypeKind::Tuple:
                return 1 + as<TypeKind::Tuple>().count();
            case TypeKind::Function:
                return 2 + as<TypeKind::Function>().parameterCount() * 2;
            case TypeKind::Var:
                return UNLIKELY(config::readableTypeVars) ? 3 : 2;
            case TypeKind::Range:
                return 2;
            case TypeKind::Struct:
                return 3 + as<TypeKind::Struct>().count() * 2 + firstWord().isCase;
            case TypeKind::Union:
                return 3 + as<TypeKind::Union>().count() + firstWord().isCase;
        }
    }

    inline bool Type::hasConstraintNode() const {
        return constraintNode() != InvalidConstraint;
    }

    inline bool Type::hasConstraintNodeIn(Constraints* constraints) const {
        return types->constraintNodes[index].depth() == constraints->depth;
    }

    inline ConstraintIndex Type::constraintNode() const {
        return types->constraintNodes[index].index();
    }

    // Field

    inline Type Field::type() const {
        return types->get(index);
    }

    inline Type parentType(Type type) {
        assert(isCase(type));
        switch (type.kind()) {
            case TypeKind::Named:
                return type.as<TypeKind::Named>().parentType();
            case TypeKind::Struct:
                return type.as<TypeKind::Struct>().parentType();
            case TypeKind::Union:
                return type.as<TypeKind::Union>().parentType();
            default:
                unreachable("Not a case type.");
        }
    }

    inline Type greatestCommonSubtype(Type a, Type b, UnifyMode substituteFlag) {
        assert(substituteFlag == MustSubstitute || !substituteFlag);

        TypeSystem* types = a.types;
        assert(b.types == types);

        a = expand(a);
        b = expand(b);
        assert(!a.isRange() && !b.isRange());
        if (a == b)
            return a;
        if (a.unifyOnto(b, nullptr, Query | substituteFlag))
            return a;
        if (b.unifyOnto(a, nullptr, Query | substituteFlag))
            return b;
        TypeKind ak = a.kind(), bk = b.kind();

        if (ak == TypeKind::Numeric && bk == TypeKind::Numeric && !substituteFlag) {
            // This handles the case where we have a signed v.s. unsigned
            // integer type, and need to find a smaller common integer.
            const auto& an = a.as<TypeKind::Numeric>(), bn = b.as<TypeKind::Numeric>();
            assert(!an.isFloat());
            assert(!bn.isFloat());
            assert(an.isSigned() != bn.isSigned());

            // Return an unsigned integer type with one less bit, i.e.
            // gcs(u3, i3) is u2.
            assert(an.bitCount() > 0 && bn.bitCount() > 0);
            return types->encode<TypeKind::Numeric>(false, false, u32(min(an.bitCount(), bn.bitCount()) - 1));
        }

        return types->invalidType();
    }

    inline Type caseLCA(Type a, Type b) {
        // Finds the least common ancestor (parent union) of two case types.

        set<TypeIndex> parents;
        Type parent = a;
        while (isCase(parent)) {
            parent = parentType(parent);
            parents.insert(parent.index);
        }

        parent = b;
        while (isCase(parent)) {
            parent = parentType(parent);
            if (parents.contains(parent.index))
                return parent;
        }

        return a.types->invalidType();
    }

    inline bool operator<=(RefTraits a, RefTraits b) {
        // own T[] <: T[]
        if ((a & Uninit) && !(b & Uninit))
            return false;

        // T[] <: uninit T[]
        if (!(a & Own) && (b & Own))
            return false;

        return true;
    }

    inline Type leastCommonSupertype(Type a, Type b, UnifyMode substituteFlag) {
        assert(substituteFlag == MustSubstitute || !substituteFlag);

        TypeSystem* types = a.types;
        assert(b.types == types);

        a = expand(a);
        b = expand(b);
        assert(!a.isRange() && !b.isRange());
        if (a == b)
            return a;
        if (a.unifyOnto(b, nullptr, Query)) {
            a.unifyOnto(b, nullptr, InPlace | substituteFlag);
            return b;
        }
        if (b.unifyOnto(a, nullptr, Query)) {
            b.unifyOnto(a, nullptr, InPlace | substituteFlag);
            return a;
        }
        TypeKind ak = a.kind(), bk = b.kind();

        if (ak == TypeKind::Numeric && bk == TypeKind::Numeric && !substituteFlag) {
            // This handles the case where we have a signed v.s. unsigned
            // integer type, and need an extra bit to store all possible
            // values. Otherwise, we should be able to rely on normal
            // subtyping rules.
            const auto& an = a.as<TypeKind::Numeric>(), bn = b.as<TypeKind::Numeric>();
            assert(!an.isFloat());
            assert(!bn.isFloat());
            assert(an.isSigned() != bn.isSigned());

            // Return a signed integer type with an extra bit, i.e.
            // lcs(u3, i3) is i4.
            return types->encode<TypeKind::Numeric>(true, false, u32(max(an.bitCount(), bn.bitCount()) + 1));
        }

        if (isCase(a) && isCase(b)) {
            if (Type result = caseLCA(a, b))
                return result;
        }

        if (isAtom(a) && b.is<TypeKind::Pointer>()) {
            if (Type common = leastCommonSupertype(a, b.as<TypeKind::Pointer>().elementType(), MustSubstitute))
                return types->encode<TypeKind::Pointer>(b.as<TypeKind::Pointer>().traits(), common);
        }

        if (isAtom(b) && a.is<TypeKind::Pointer>()) {
            if (Type common = leastCommonSupertype(b, a.as<TypeKind::Pointer>().elementType(), MustSubstitute))
                return types->encode<TypeKind::Pointer>(a.as<TypeKind::Pointer>().traits(), common);
        }

        if (a.is<TypeKind::Pointer>() && b.is<TypeKind::Pointer>()
            && isCase(a.as<TypeKind::Pointer>().elementType()) && isCase(b.as<TypeKind::Pointer>().elementType())) {
            RefTraits traits;
            if (a.as<TypeKind::Pointer>().traits() <= b.as<TypeKind::Pointer>().traits())
                traits = b.as<TypeKind::Pointer>().traits();
            else if (b.as<TypeKind::Pointer>().traits() <= a.as<TypeKind::Pointer>().traits())
                traits = a.as<TypeKind::Pointer>().traits();
            else
                return types->invalidType();

            if (Type result = caseLCA(a.as<TypeKind::Pointer>().elementType(), b.as<TypeKind::Pointer>().elementType()))
                return types->encode<TypeKind::Pointer>(traits, result);
        }

        return types->invalidType();
    }

    // PrimitiveType

    inline bool PrimitiveType::operator==(PrimitiveType other) const {
        return kind() == other.kind();
    }

    inline bool PrimitiveType::contains(TypeIndex other) const {
        return false;
    }

    inline u64 PrimitiveType::hash() const {
        return kindHash();
    }

    inline void PrimitiveType::concretify() {}

    inline UnifyResult PrimitiveType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        return kind() == other.kind() ? UnifySuccess : UnifyFailure;
    }

    // NumericType

    inline bool NumericType::isSigned() const {
        return firstWord().isSigned;
    }

    inline bool NumericType::isFloat() const {
        return firstWord().isFloat;
    }

    inline u8 NumericType::bitCount() const {
        return firstWord().bitCount;
    }

    inline bool NumericType::operator==(NumericType other) const {
        return isSigned() == other.isSigned() && isFloat() == other.isFloat() && bitCount() == other.bitCount();
    }

    inline bool NumericType::contains(TypeIndex other) const {
        return false;
    }

    inline u64 NumericType::hash() const {
        u64 bitsTogether = firstWord().bits & 0b00000111'00000000'00000000'00000000u;
        return intHash(bitsTogether | u64(kind()));
    }

    inline void NumericType::concretify() {}

    inline UnifyResult NumericType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        // Different numeric types have different representations and meanings
        // for the same bits, so we can't do numeric subtyping if
        // substitutability is a requirement.
        if (mode & MustSubstitute)
            return *this == other ? UnifySuccess : UnifyFailure;

        if (other.is<TypeKind::Numeric>()) {
            auto otherNumeric = other.as<TypeKind::Numeric>();
            if (isFloat() && otherNumeric.isFloat())
                return otherNumeric.bitCount() >= bitCount() ? UnifySuccess : UnifyFailure;
            else if (otherNumeric.isFloat()) // We are an int, they are a float, even if lossy we always allow this.
                return UnifySuccess;
            else if (!isFloat()) { // We are an int, they are also an int, only allow coercion if lossless.
                if (isSigned() == otherNumeric.isSigned())
                    return otherNumeric.bitCount() >= bitCount() ? UnifySuccess : UnifyFailure;
                else if (otherNumeric.isSigned())
                    return otherNumeric.bitCount() >= bitCount() + 1 ? UnifySuccess : UnifyFailure; // An N+1 bit signed integer can represent all values of an N-bit unsigned integer.
            }
        }
        return UnifyFailure;
    }

    // ArrayType

    inline u32 ArrayType::length() const {
        auto w = firstWord();
        if LIKELY(w.isCompactArray)
            return w.compactLength;
        else
            return nthWord(1).extLength;
    }

    inline TypeIndex ArrayType::elementTypeIndex() const {
        auto w = firstWord();
        if LIKELY(w.isCompactArray)
            return w.compactElement;
        else
            return w.extElement;
    }

    inline Type ArrayType::elementType() const {
        return types->get(elementTypeIndex());
    }

    inline bool ArrayType::operator==(ArrayType other) const {
        return elementType() == other.elementType() && length() == other.length();
    }

    inline bool ArrayType::contains(TypeIndex var) const {
        return elementTypeIndex() == var || elementType().contains(var);
    }

    inline u64 ArrayType::hash() const {
        return mixHash(kindHash(), mixHash(length(), clover::hash(elementType())));
    }

    inline void ArrayType::concretify() {
        elementType().concretify();
        firstWord().isConcrete = true;
    }

    inline UnifyResult ArrayType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        if (other.is<TypeKind::Array>()) {
            if (length() != other.as<TypeKind::Array>().length())
                return UnifyFailure;
            auto otherElement = other.as<TypeKind::Array>().elementType();
            return elementType().unifyOnto(otherElement, constraints, mode)
                & otherElement.unifyOnto(elementType(), constraints, mode);
        }

        // Slices and arrays have different representations, so we can't take
        // this path if we are required to substitute.
        if (other.is<TypeKind::Slice>() && !(mode & MustSubstitute)) {
            if (other.as<TypeKind::Slice>().isOwn())
                return UnifyFailure;
            auto otherElement = other.as<TypeKind::Slice>().elementType();
            return elementType().unifyOnto(otherElement, constraints, mode)
                & otherElement.unifyOnto(elementType(), constraints, mode);
        }
        return UnifyFailure;
    }

    // ArrayBuilder

    inline Type ArrayBuilder::build(TypeSystem* types) {
        return types->encode<TypeKind::Array>(elementType, length);
    }

    // SliceType

    inline bool SliceType::isOwn() const {
        return firstWord().refTraits & Own;
    }

    inline bool SliceType::isUninit() const {
        return firstWord().refTraits & Uninit;
    }

    inline RefTraits SliceType::traits() const {
        return firstWord().refTraits;
    }

    inline TypeIndex SliceType::elementTypeIndex() const {
        return firstWord().extElement;
    }

    inline Type SliceType::elementType() const {
        return types->get(elementTypeIndex());
    }

    inline bool SliceType::operator==(SliceType other) const {
        return elementType() == other.elementType() && isOwn() == other.isOwn() && isUninit() == other.isUninit();
    }

    inline bool SliceType::contains(TypeIndex var) const {
        return elementTypeIndex() == var || elementType().contains(var);
    }

    inline u64 SliceType::hash() const {
        u64 h = intHash(u64(kind()) << 2 | u64(traits()));
        if (elementType().isVar())
            return mixHash(h, intHash(elementTypeIndex()));
        return mixHash(h, clover::hash(elementType()));
    }

    inline void SliceType::concretify() {
        elementType().concretify();
        firstWord().isConcrete = true;
    }

    inline UnifyResult SliceType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        if (other.is<TypeKind::Slice>()) {
            if (!(traits() <= other.as<TypeKind::Slice>().traits()))
                return UnifyFailure;
            auto otherElement = other.as<TypeKind::Slice>().elementType();
            return elementType().unifyOnto(otherElement, constraints, mode)
                & otherElement.unifyOnto(elementType(), constraints, mode);
        }
        return UnifyFailure;
    }

    // PointerType

    inline bool PointerType::isOwn() const {
        return firstWord().refTraits & Own;
    }

    inline bool PointerType::isUninit() const {
        return firstWord().refTraits & Uninit;
    }

    inline RefTraits PointerType::traits() const {
        return firstWord().refTraits;
    }

    inline TypeIndex PointerType::elementTypeIndex() const {
        return firstWord().extElement;
    }

    inline Type PointerType::elementType() const {
        return types->get(elementTypeIndex());
    }

    inline bool PointerType::operator==(PointerType other) const {
        return elementType() == other.elementType() && isOwn() == other.isOwn() && isUninit() == other.isUninit();
    }

    inline bool PointerType::contains(TypeIndex var) const {
        return elementTypeIndex() == var || elementType().contains(var);
    }

    inline u64 PointerType::hash() const {
        u64 h = intHash(u64(kind()) << 2 | u64(traits()));
        if (elementType().isVar())
            return mixHash(h, intHash(elementTypeIndex()));
        return mixHash(h, clover::hash(elementType()));
    }

    inline void PointerType::concretify() {
        elementType().concretify();
        firstWord().isConcrete = true;
    }

    inline UnifyResult PointerType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        if (other.is<TypeKind::Pointer>()) {
            if (!(traits() <= other.as<TypeKind::Pointer>().traits()))
                return UnifyFailure;
            auto otherElement = other.as<TypeKind::Pointer>().elementType();
            return elementType().unifyOnto(otherElement, constraints, mode | MustSubstitute);
        }

        // Slices and pointers have different representations, so we can't take
        // this path if we are required to substitute.
        if (other.is<TypeKind::Slice>() && elementType().is<TypeKind::Array>() && !(mode & MustSubstitute)) {
            if (!(traits() <= other.as<TypeKind::Slice>().traits()))
                return UnifyFailure;
            auto element = elementType().as<TypeKind::Array>().elementType();
            auto otherElement = other.as<TypeKind::Slice>().elementType();
            return element.unifyOnto(otherElement, constraints, mode)
                & otherElement.unifyOnto(element, constraints, mode);
        }
        return UnifyFailure;
    }

    // NamedType

    inline Symbol NamedType::name() const {
        return nthWord(1).name;
    }

    inline TypeIndex NamedType::innerTypeIndex() const {
        return firstWord().extElement;
    }

    inline Type NamedType::innerType() const {
        return types->get(innerTypeIndex());
    }

    inline bool NamedType::operator==(NamedType other) const {
        return name() == other.name() && innerTypeIndex() == other.innerTypeIndex();
    }

    inline bool NamedType::contains(TypeIndex var) const {
        return false;
    }

    inline u64 NamedType::hash() const {
        return mixHash(kindHash(), intHash(index));
    }

    inline void NamedType::concretify() {}

    inline UnifyResult NamedType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        if (other == *this)
            return UnifySuccess;
        if (isCase())
            return parentType().unifyOnto(other, constraints, mode);
        if (other.is<TypeKind::Pointer>() && innerType() == Void) {
            // Since instances of atoms are indistinguishable, we can construct
            // a pointer to an atom automatically.
            return unifyOnto(other.as<TypeKind::Pointer>().elementType(), constraints, mode)
                & other.as<TypeKind::Pointer>().elementType().unifyOnto(*this, constraints, mode);
        }
        return UnifyFailure;
    }

    inline bool NamedType::isCase() const {
        return firstWord().isCase;
    }

    inline Type NamedType::parentType() const {
        return types->get(parentTypeIndex());
    }

    inline TypeIndex NamedType::parentTypeIndex() const {
        assert(isCase());
        return nthWord(3).typeBound;
    }

    inline void NamedType::setParentType(TypeIndex i) {
        assert(isCase());
        nthWord(3).typeBound = i;
    }

    inline ScopeIndex NamedType::scope() const {
        return nthWord(2).scope;
    }

    inline void NamedType::setScope(ScopeIndex i) {
        assert(i != InvalidScope);
        nthWord(2).scope = i;
    }

    inline TypeIndex NamedType::typeTag() const {
        assert(isCase());
        return nthWord(4).tag;
    }

    inline void NamedType::setTypeTag(TypeIndex tag) {
        assert(isCase());
        nthWord(4).tag = tag;
    }

    // TupleType

    inline u32 TupleType::count() const {
        return firstWord().fieldCount;
    }

    inline Field TupleType::field(u32 i) const {
        assert(i < count());
        auto first = nthWord(1 + i * 2);
        if (first.hasName) {
            auto second = nthWord(2 + i * 2);
            if (first.isBitField)
                return Field(types, Symbol(second.name), first.fieldType, Bits(first.bitFieldSize));
            return Field(types, Symbol(second.name), first.fieldType);
        } else {
            if (first.isBitField)
                return Field(types, first.fieldType, Bits(first.bitFieldSize));
            return Field(types, first.fieldType);
        }
    }

    inline TypeIndex TupleType::fieldTypeIndex(u32 i) const {
        assert(i < count());
        return nthWord(1 + i * 2).fieldType;
    }

    inline Type TupleType::fieldType(u32 i) const {
        return types->get(fieldTypeIndex(i));
    }

    inline bool TupleType::fieldHasName(u32 i) const {
        return nthWord(1 + i * 2).hasName;
    }

    inline Symbol TupleType::fieldName(u32 i) const {
        assert(nthWord(1 + i * 2).hasName);
        return nthWord(2 + i * 2).name;
    }

    inline bool TupleType::operator==(TupleType other) const {
        if (count() != other.count())
            return false;
        for (u32 i = 0; i < count(); i ++) {
            // We use all 32 bits of the first field word, so we can compare them directly.
            auto ourFirst = nthWord(1 + i * 2), theirFirst = other.nthWord(1 + i * 2);
            if (ourFirst.bits != theirFirst.bits)
                return false;
            if (ourFirst.hasName && nthWord(2 + i * 2).name != other.nthWord(2 + i * 2).name)
                return false;
        }
        return true;
    }

    inline bool TupleType::contains(TypeIndex var) const {
        for (u32 i = 0; i < count(); i ++)
            if (fieldTypeIndex(i) == var || fieldType(i).contains(var))
                return true;
        return false;
    }

    inline u64 TupleType::hash() const {
        u64 h = kindHash();
        h *= count() ^ 9916916121878267039ull;
        for (u32 i = 0; i < count(); i ++)
            h = mixHash(mixHash(h, clover::hash(fieldType(i))), intHash(nthWord(2 + i * 2).name));
        return h;
    }

    inline void TupleType::concretify() {
        for (u32 i = 0; i < count(); i ++)
            fieldType(i).concretify();
        firstWord().isConcrete = true;
    }

    inline UnifyResult TupleType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        UnifyResult result = UnifySuccess;
        if (other.is<TypeKind::Tuple>()) {
            const auto& tup = other.as<TypeKind::Tuple>();
            if (tup.count() != count())
                return UnifyFailure;
            for (u32 i = 0; i < count(); i ++) {
                result &= fieldType(i).unifyOnto(tup.fieldType(i), constraints, mode);
                if (result == UnifyFailure)
                    return result;
            }
            return result;
        }
        return UnifyFailure;
    }

    // TupleBuilder

    inline Type TupleBuilder::build(TypeSystem* types) {
        return types->encode<TypeKind::Tuple>(*this);
    }

    // FunctionType

    inline u32 FunctionType::parameterCount() const {
        return firstWord().fieldCount;
    }

    inline TypeIndex FunctionType::returnTypeIndex() const {
        return nthWord(1).fieldType;
    }

    inline Type FunctionType::returnType() const {
        return types->get(returnTypeIndex());
    }

    inline Field FunctionType::parameter(u32 i) const {
        assert(i < parameterCount());
        auto first = nthWord(2 + i * 2);
        assert(!first.isBitField); // Bit fields are illegal in function types.
        return first.hasName ? Field(types, Symbol(nthWord(3 + i * 2).name), first.fieldType) : Field(types, first.fieldType);
    }

    inline TypeIndex FunctionType::parameterTypeIndex(u32 i) const {
        assert(i < parameterCount());
        return nthWord(2 + i * 2).fieldType;
    }

    inline Type FunctionType::parameterType(u32 i) const {
        return types->get(parameterTypeIndex(i));
    }

    inline bool FunctionType::parameterHasName(u32 i) const {
        assert(i < parameterCount());
        return nthWord(2 + i * 2).hasName;
    }

    inline Symbol FunctionType::parameterName(u32 i) const {
        assert(i < parameterCount());
        assert(nthWord(2 + i * 2).hasName);
        return nthWord(3 + i * 2).name;
    }

    inline bool FunctionType::operator==(FunctionType other) const {
        if (parameterCount() != other.parameterCount())
            return false;
        if (returnType() != other.returnType())
            return false;
        for (u32 i = 0; i < parameterCount(); i ++) {
            if (parameterType(i) != other.parameterType(i))
                return false;
            auto ourFirst = nthWord(2 + i * 2), theirFirst = other.nthWord(2 + i * 2);
            if (ourFirst.hasName && nthWord(3 + i * 2).name != other.nthWord(3 + i * 2).name)
                return false;
        }
        return true;
    }

    inline bool FunctionType::contains(TypeIndex var) const {
        if (returnTypeIndex() == var)
            return true;
        if (returnType().contains(var))
            return true;
        for (u32 i = 0; i < parameterCount(); i ++)
            if (parameterTypeIndex(i) == var || parameterType(i).contains(var))
                return true;
        return false;
    }

    inline u64 FunctionType::hash() const {
        u64 h = kindHash();
        h = mixHash(h, intHash(parameterCount()));
        h = mixHash(h, clover::hash(returnType()));
        for (u32 i = 0; i < parameterCount(); i ++)
            h = mixHash(mixHash(h, clover::hash(parameterType(i))), nthWord(3 + i * 2).name);
        return h;
    }

    inline void FunctionType::concretify() {
        returnType().concretify();
        for (u32 i = 0; i < parameterCount(); i ++)
            parameterType(i).concretify();
        firstWord().isConcrete = true;
    }

    inline UnifyResult FunctionType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        UnifyResult result = UnifySuccess;
        if (other.is<TypeKind::Function>()) {
            const auto& fun = other.as<TypeKind::Function>();
            if (fun.parameterCount() != parameterCount())
                return UnifyFailure;

            // Function types are invariant in return type and parameters when
            // it comes to function/function subtyping, since Clover does not
            // have (or more accurately does not want to use) a uniform value
            // representation.

            if (!returnType().unifyOnto(fun.returnType(), constraints, mode)
                || !fun.returnType().unifyOnto(returnType(), constraints, mode))
                return UnifyFailure;
            for (u32 i = 0; i < parameterCount(); i ++) {
                result &= parameterType(i).unifyOnto(fun.parameterType(i), constraints, mode);
                result &= fun.parameterType(i).unifyOnto(parameterType(i), constraints, mode);
                if (!result)
                    return result;
            }
            return result;
        }
        return UnifyFailure;
    }

    // FunctionBuilder

    inline Type FunctionBuilder::build(TypeSystem* types) {
        return types->encode<TypeKind::Function>(*this);
    }

    // StructType

    inline Symbol StructType::name() const {
        return nthWord(1).name;
    }

    inline u32 StructType::count() const {
        return firstWord().fieldCount;
    }

    inline Field StructType::field(u32 i) const {
        assert(i < count());
        auto first = nthWord(3 + i * 2);
        assert(first.hasName);
        auto second = nthWord(4 + i * 2);
        if (first.isBitField)
            return Field(types, Symbol(second.name), first.fieldType, Bits(first.bitFieldSize));
        return Field(types, Symbol(second.name), first.fieldType);
    }

    inline TypeIndex StructType::fieldTypeIndex(u32 i) const {
        assert(i < count());
        return nthWord(3 + i * 2).fieldType;
    }

    inline Type StructType::fieldType(u32 i) const {
        return types->get(fieldTypeIndex(i));
    }

    inline Symbol StructType::fieldName(u32 i) const {
        assert(i < count());
        return nthWord(4 + i * 2).name;
    }

    inline bool StructType::operator==(StructType other) const {
        return index == other.index;
    }

    inline bool StructType::contains(TypeIndex var) const {
        for (u32 i = 0; i < count(); i ++)
            if (fieldType(i).contains(var))
                return true;
        return false;
    }

    inline u64 StructType::hash() const {
        return mixHash(kindHash(), (index ^ 17402556438599366131ull) * 10595215790981386093ull);
    }

    inline void StructType::concretify() {}

    inline UnifyResult StructType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        if (other == *this)
            return UnifySuccess;
        if (isCase())
            return parentType().unifyOnto(other, constraints, mode);
        return UnifyFailure;
    }

    inline bool StructType::isCase() const {
        return firstWord().isCase;
    }

    inline Type StructType::parentType() const {
        return types->get(parentTypeIndex());
    }

    inline TypeIndex StructType::parentTypeIndex() const {
        assert(isCase());
        return nthWord(3 + count() * 2).typeBound;
    }

    inline void StructType::setParentType(TypeIndex i) {
        assert(isCase());
        nthWord(3 + count() * 2).typeBound = i;
    }

    inline ScopeIndex StructType::scope() const {
        return nthWord(2).scope;
    }

    inline void StructType::setScope(ScopeIndex i) {
        assert(i != InvalidScope);
        nthWord(2).scope = i;
    }

    inline TypeIndex StructType::typeTag() const {
        assert(isCase());
        return nthWord(4 + count() * 2).tag;
    }

    inline void StructType::setTypeTag(TypeIndex tag) {
        assert(isCase());
        nthWord(4 + count() * 2).tag = tag;
    }

    // StructBuilder

    inline Type StructBuilder::build(TypeSystem* types) {
        return types->encode<TypeKind::Struct>(*this);
    }

    // ConstraintList

    inline vec<ConstraintIndex>& ConstraintList::refinementList(Constraints& constraints) {
        assert(hasRefinementList);
        auto handle = data()[0];
        return constraints.refinementLists[handle.index];
    }

    inline vec<ConstraintIndex>& ConstraintList::ensureRefinementList(Constraints& constraints) {
        if (!hasRefinementList) {
            hasRefinementList = 1;
            assert(sz == 0); // This should be true, but mostly we assert it for convenience.
            this->add({ .index = constraints.refinementLists.claim(), .kind = Constraint::Order });
        }

        auto handle = data()[0];
        return constraints.refinementLists[handle.index];
    }

    inline void ConstraintList::dropRefinementList(Constraints& constraints) {
        auto handle = data()[0];
        constraints.refinementLists.release(handle.index);
        memory::move(data(), begin(), end() - begin());
        hasRefinementList = 0;
        sz --;
    }

    // Constraints

    inline Constraints::~Constraints() {
        if (depth > 0) for (u32 i = 0; i < constrainedTypes.size(); i ++)
            types->constraintNodes[constrainedTypes[i]] = outerIndices[i];
    }

    inline ConstraintIndex Constraints::index(Type type) {
        type = clover::expand(type);
        auto packed = types->constraintNodes[type.index];
        if (packed.index() == InvalidConstraint || packed.depth() < depth) {
            if (depth > 0)
                outerIndices.push(packed);
            packed = types->constraintNodes[type.index] = PackedConstraintNode(this, constraints.size());
            constraints.push({});
            constrainedTypes.push(type.index);
            return constraints.size() - 1;
        }

        return packed.index();
    }

    inline ConstraintList& Constraints::list(Type type) {
        return constraints[index(type)];
    }

    inline void printTypeConstraints(TypeSystem*, Constraints*);

    inline bool Constraints::constrainType(Type subType, Type superType) {
        if UNLIKELY(config::verboseUnify >= 2)
            println("[TYPE]\tAdded subtyping constraint ", subType, " <: ", superType);
        subType = clover::expand(subType);
        superType = clover::expand(superType);
        if (subType.index == superType.index)
            return false;
        auto superIndex = index(superType);
        auto subIndex = index(subType);
        return list(superType).add({ .index = subIndex, .kind = Constraint::Subtype });
    }

    inline bool Constraints::constrainSubstitute(Type subType, Type superType) {
        if UNLIKELY(config::verboseUnify >= 2)
            println("[TYPE]\tAdded substitutability constraint ", subType, " =: ", superType);
        subType = clover::expand(subType);
        superType = clover::expand(superType);
        if (subType.index == superType.index)
            return false;
        auto superIndex = index(superType);
        auto subIndex = index(subType);
        return list(superType).add({ .index = subIndex, .kind = Constraint::Substitute });
    }

    inline bool Constraints::constrainOrder(Type beforeType, Type afterType) {
        beforeType = clover::expand(beforeType);
        afterType = clover::expand(afterType);
        assert(afterType.isVar() && afterType.asVar().hasOwner());
        if (beforeType.index == afterType.index)
            return false;
        if (!beforeType.isVar()) {
            beforeType.forEachVar([&, this](Type type) {
                this->constrainOrder(type, afterType);
            });
            list(afterType).setNeedsRefinement();
            return true;
        }
        if UNLIKELY(config::verboseUnify >= 2)
            println("[TYPE]\tAdded ordering constraint ", beforeType, " ~> ", afterType);
        list(afterType).setNeedsRefinement();
        auto beforeIndex = index(beforeType);
        return list(afterType).add({ .index = beforeIndex, .kind = Constraint::Order });
    }

    inline void Constraints::clear() {
        constraints.clear();

        if (depth > 0) for (auto [i, t] : enumerate(constrainedTypes))
            types->constraintNodes[t] = outerIndices[i];
        else for (TypeIndex idx : constrainedTypes)
            types->constraintNodes[idx] = {};
        constrainedTypes.clear();
    }

    // VarType

    inline Type VarType::lowerBound() const {
        return types->get(firstWord().typeBound);
    }

    inline Type VarType::upperBound() const {
        return types->get(nthWord(1).typeBound);
    }

    inline UnifyResult VarType::setLowerBound(TypeIndex index) {
        assert(!isEqual());
        if (firstWord().typeBound != index) {
            if (nthWord(1).typeBound == index)
                makeEqual(types->get(index));
            else
                firstWord().typeBound = index;
        }
        return UnifySuccess;
    }

    inline UnifyResult VarType::setUpperBound(TypeIndex index) {
        assert(!isEqual());
        if (nthWord(1).typeBound != index) {
            if (firstWord().typeBound == index)
                makeEqual(types->get(index));
            else
                nthWord(1).typeBound = index;
        }
        return UnifySuccess;
    }

    inline bool VarType::isEqual() const {
        return nthWord(1).markedEqual;
    }

    inline Type VarType::equalType() const {
        assert(isEqual());
        Type expanded = expand(types->get(firstWord().typeBound));
        if (expanded.index != firstWord().typeBound)
            firstWord().typeBound = expanded.index;
        return expanded;
    }

    inline void VarType::replaceEqualType(Type t) {
        assert(isEqual());
        firstWord().typeBound = t.index;
    }

    inline void VarType::makeEqual(Type t) {
        assert(!isEqual());
        if (config::verboseUnify >= 2)
            println("[TYPE]\tMade type variable ", *this, " equal to ", t);
        nthWord(1).markedEqual = true;
        firstWord().typeBound = nthWord(1).typeBound = t.index;
    }

    inline bool canResolveTypeFromLowerBound(TypeSystem* sys, Type lb) {
        // This method is pretty important to the efficiency of type inference,
        // but implicitly relies on other behavior in the type system. It's
        // important to keep up-to-date. Essentially, this method needs to be
        // able to tell us whether, if we know the lower bound of a type
        // variable, we know the final value of that type variable (i.e. we
        // don't need to wait to refine its upper bound). This is important
        // since we infer types bottom-up, so if knowing the lower bound of a
        // var is enough to know its final inferred type, we can make it equal
        // to that type and remove it from the constraint graph. So, we want to
        // return true here for any lower bound type that, no matter the
        // inferred upper bound, gives us the final inference of a variable.

        lb = expand(lb);
        switch (lb.kind()) {
            case TypeKind::Bottom:
                // If we essentially know nothing about the lower bound, we
                // can't use it to resolve the type.
                return false;
            case TypeKind::Numeric:
                // Numeric types form our main exception to being able to infer
                // from the lower bound alone, since integer types widen up to
                // the upper bound to avoid overflow.
                return lb.as<TypeKind::Numeric>().isFloat() || lb.as<TypeKind::Numeric>().bitCount() == 64;
            default:
                // All other types do not widen up to their upper bound.
                return true;
        }
    }

    inline Type canonicalTypeInBounds(TypeSystem* sys, Type lb, Type ub) {
        if (lb == Bottom && ub != Any) {
            return ub;
        }
        if (lb.is<TypeKind::Numeric>()) {
            const auto& num = lb.as<TypeKind::Numeric>();
            u32 numBits = lb.as<TypeKind::Numeric>().bitCount();
            u32 maxBits = 64;
            bool isSigned = num.isSigned();
            if (num.isFloat() && num.bitCount() <= 32)
                maxBits = 32;
            if (!num.isFloat() && ub.is<TypeKind::Numeric>() && !ub.as<TypeKind::Numeric>().isFloat()) {
                maxBits = ub.as<TypeKind::Numeric>().bitCount();
                isSigned = ub.as<TypeKind::Numeric>().isSigned();
            }
            if (!num.isFloat() && ub == Any)
                maxBits = 64;
            if (numBits < 8 && maxBits >= 8)
                numBits = 8;
            if (numBits < 16 && maxBits >= 16)
                numBits = 16;
            if (numBits < 32 && maxBits >= 32)
                numBits = 32;
            if (numBits < 64 && maxBits >= 64)
                numBits = 64;

            static const TypeIndex signedTypes[9] = {
                I8, I8, I16, I16, I32, I32, I32, I32, I64
            };
            static const TypeIndex unsignedTypes[9] = {
                U8, U8, U16, U16, U32, U32, U32, U32, U64
            };
            if (num.isFloat())
                return lb = numBits == 64 ? sys->get(F64) : sys->get(F32);
            if (isSigned)
                return lb = sys->get(signedTypes[numBits / 8]);
            return lb = sys->get(unsignedTypes[numBits / 8]);
        }
        return lb;
    }

    inline void VarType::concretify() {
        // If this type is already equal to another type, we ensure that
        // type is concrete.

        if (isEqual()) {
            Type t = equalType();
            t.concretify();
            return;
        }

        // Otherwise, concretify our bounds, and pick a "sensible" type.
        // Generally, this "sensible" type is our concrete lower bound. But
        // numeric types are handled differently. For integers, we pick the
        // integer type with the largest width that is still a subtype of the
        // concrete upper bound, and such that the width is a valid "canonical"
        // width for a numeric type - 8, 16, 32, or 64 for an integer, and 32
        // or 64 for a float.

        Type lb = lowerBound(), ub = upperBound();
        lb = canonicalTypeInBounds(types, lb, ub);

        if (!lb.unifyOnto(ub, nullptr, Query))
            unreachable("Failed to concretify variable ", *this, ": canonicalized lower bound ", lb, " is no longer a subtype of concrete upper bound ", ub);
        makeEqual(lb);
    }

    inline u32 VarType::varNumber() const {
        assert(config::readableTypeVars);
        return nthWord(hasOwner() ? 3 : 2).bits;
    }

    inline bool VarType::hasOwner() const {
        return nthWord(1).hasOwner;
    }

    inline NodeIndex VarType::owner() const {
        return nthWord(2).owner;
    }

    inline bool VarType::operator==(VarType other) const {
        return index == other.index;
    }

    inline u64 VarType::hash() const {
        return mixHash(intHash(index), kindHash()); // We don't need to add any entropy here.
    }

    inline UnifyResult VarType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        unreachable("Should not be reached directly.");
    }

    // RangeType

    inline TypeIndex RangeType::lowerBoundIndex() const {
        return firstWord().typeBound;
    }

    inline TypeIndex RangeType::upperBoundIndex() const {
        return nthWord(1).typeBound;
    }

    inline Type RangeType::lowerBound() const {
        return types->get(lowerBoundIndex());
    }

    inline Type RangeType::upperBound() const {
        return types->get(upperBoundIndex());
    }

    inline bool RangeType::operator==(RangeType other) const {
        return lowerBoundIndex() == other.lowerBoundIndex()
            && upperBoundIndex() == other.upperBoundIndex();
    }

    inline bool RangeType::contains(TypeIndex type) const {
        // Ranges shouldn't have variables in them.
        assert(lowerBound().isConcrete());
        assert(upperBound().isConcrete());
        return false;
    }

    inline u64 RangeType::hash() const {
        return mixHash(mixHash(kindHash(), clover::hash(lowerBound())), clover::hash(upperBound()));
    }

    inline void RangeType::concretify() {
        assert(lowerBound().isConcrete());
        assert(upperBound().isConcrete());
    }

    inline UnifyResult RangeType::unifyOnto(Type other, Constraints*, UnifyMode) {
        unreachable("Should not be reached directly.");
    }

    // Unions

    inline bool UnionType::operator==(UnionType other) const {
        return index == other.index;
    }

    inline u64 UnionType::hash() const {
        return mixHash(kindHash(), intHash(index));
    }

    inline Symbol UnionType::name() const {
        return nthWord(1).name;
    }

    inline u32 UnionType::count() const {
        return firstWord().fieldCount;
    }

    inline Type UnionType::caseType(u32 i) const {
        return types->get(caseTypeIndex(i));
    }

    inline TypeIndex UnionType::caseTypeIndex(u32 i) const {
        return nthWord(3 + i).typeBound;
    }

    inline bool UnionType::contains(TypeIndex var) const {
        for (u32 i = 0; i < count(); i ++)
            if (caseType(i).contains(var))
                return true;
        return false;
    }

    inline void UnionType::concretify() {}

    inline UnifyResult UnionType::unifyOnto(Type other, Constraints* constraints, UnifyMode mode) {
        if (other == *this)
            return UnifySuccess;
        if (isCase())
            return parentType().unifyOnto(other, constraints, mode);
        return UnifyFailure;
    }

    inline bool UnionType::isCase() const {
        return firstWord().isCase;
    }

    inline Type UnionType::parentType() const {
        return types->get(parentTypeIndex());
    }

    inline TypeIndex UnionType::parentTypeIndex() const {
        assert(isCase());
        return nthWord(3 + count()).typeBound;
    }

    inline void UnionType::setParentType(TypeIndex i) {
        assert(isCase());
        nthWord(3 + count()).typeBound = i;
    }

    inline ScopeIndex UnionType::scope() const {
        return nthWord(2).scope;
    }

    inline void UnionType::setScope(ScopeIndex i) {
        assert(i != InvalidScope);
        nthWord(2).scope = i;
    }

    // UnionBuilder

    inline Type UnionBuilder::build(TypeSystem* types) {
        return types->encode<TypeKind::Union>(*this);
    }

    // Type formatting

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const PrimitiveType& type) {
        switch (type.kind()) {
            case TypeKind::Bottom:
                return format(io, "bottom");
            case TypeKind::Void:
                return format(io, "void");
            case TypeKind::Bool:
                return format(io, "bool");
            case TypeKind::Char:
                return format(io, "char");
            case TypeKind::Any:
                return format(io, "any");
            default:
                unreachable("We shouldn't reach here with a non-primitive typekind.");
        }
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const NumericType& type) {
        return format(io, type.isFloat() ? "f" : (type.isSigned() ? "i" : "u"), (u32)type.bitCount());
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const PointerType& type) {
        if (type.isOwn())
            io = format(io, "own ");
        if (type.isUninit())
            io = format(io, "uninit ");
        return format(io, type.elementType(), '*');
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const SliceType& type) {
        if (type.isOwn())
            io = format(io, "own ");
        if (type.isUninit())
            io = format(io, "uninit ");
        return format(io, type.elementType(), "[]");
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const ArrayType& type) {
        return format(io, type.elementType(), '[', type.length(), ']');
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const TupleType& type) {
        if (type.count() == 0)
            return format(io, "()");
        io = format(io, '(');
        bool first = true;
        for (u32 i = 0; i < type.count(); i ++) {
            if (!first)
                io = format(io, ", ");
            first = false;
            auto field = type.field(i);
            io = format(io, field.type());
            if (field.isBitField)
                io = format(io, '#', field.bitFieldSize);
            if (field.hasName)
                io = format(io, ' ', type.types->symbols->get(field.name.symbol));
        }
        return format(io, ')');
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const FunctionType& type) {
        io = format(io, type.returnType(), '(');
        bool first = true;
        for (u32 i = 0; i < type.parameterCount(); i ++) {
            if (!first)
                io = format(io, ", ");
            first = false;
            io = format(io, type.parameterType(i));
            if (type.parameterHasName(i))
                io = format(io, ' ', type.types->symbols->get(type.parameterName(i).symbol));
        }
        return format(io, ')');
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const RangeType& type) {
        return format(io, '{', type.lowerBound(), ':', type.upperBound(), '}');
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const VarType& type) {
        if UNLIKELY(config::readableTypeVars) {
            io = format(io, '\'');
            u32 number = type.varNumber();
            u32 p = 1;
            while (p * 26 <= number)
                p *= 26;
            while (p > 0) {
                u32 n = number / p;
                u32 c = n % 26;
                if (p > 1)
                    c --;
                p /= 26;
                io = format(io, i8('a' + c));
            }
            return io;
        }
        return format(io, '@', type.index);
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const NamedType& type) {
        return format(io, type.types->symbols->get(type.name().symbol), '(', type.innerType(), ')');
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const StructType& type) {
        io = format(io, type.types->symbols->get(type.name().symbol), '(');
        bool first = true;
        for (u32 i = 0; i < type.count(); i ++) {
            if (!first)
                io = format(io, ", ");
            first = false;
            auto field = type.field(i);
            io = format(io, field.type());
            if (field.isBitField)
                io = format(io, '#', field.bitFieldSize);
            if (field.hasName)
                io = format(io, ' ', type.types->symbols->get(field.name.symbol));
        }
        return format(io, ')');
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const UnionType& type) {
        io = format(io, type.types->symbols->get(type.name().symbol), '(');
        bool first = true;
        for (u32 i = 0; i < type.count(); i ++) {
            if (!first)
                io = format(io, ", ");
            first = false;
            io = format(io, "case ", type.caseType(i));
        }
        return format(io, ')');
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Type& type) {
        switch (type.kind()) {
            case TypeKind::Bottom:
                return format(io, type.as<TypeKind::Bottom>());
            case TypeKind::Void:
                return format(io, type.as<TypeKind::Void>());
            case TypeKind::Bool:
                return format(io, type.as<TypeKind::Bool>());
            case TypeKind::Char:
                return format(io, type.as<TypeKind::Char>());
            case TypeKind::Any:
                return format(io, type.as<TypeKind::Any>());
            case TypeKind::Numeric:
                return format(io, type.as<TypeKind::Numeric>());
            case TypeKind::Pointer:
                return format(io, type.as<TypeKind::Pointer>());
            case TypeKind::Slice:
                return format(io, type.as<TypeKind::Slice>());
            case TypeKind::Array:
                return format(io, type.as<TypeKind::Array>());
            case TypeKind::Tuple:
                return format(io, type.as<TypeKind::Tuple>());
            case TypeKind::Function:
                return format(io, type.as<TypeKind::Function>());
            case TypeKind::Var:
                return format(io, type.asVar());
            case TypeKind::Range:
                return format(io, type.asRange());
            case TypeKind::Named:
                return format(io, type.as<TypeKind::Named>());
            case TypeKind::Struct:
                return format(io, type.as<TypeKind::Struct>());
            case TypeKind::Union:
                return format(io, type.as<TypeKind::Union>());
        }
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO formatTypeVariableState(IO io, TypeSystem* sys) {
        for (TypeIndex i : indices(sys->typeList)) if (sys->get(i).isVar()) {
            auto var = sys->get(i).asVar();
            io = format(io, " - ", var);
            if (var.isEqual())
                io = format(io, " = ", var.equalType());
            else
                io = format(io, " = [", var.lowerBound(), ", ", var.upperBound(), "]");
            io = format(io, '\n');
        }
        return io;
    }

    inline void printTypeVariableState(TypeSystem* sys) {
        formatTypeVariableState(io_stdout, sys);
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO formatTypeConstraints(IO io, TypeSystem* sys, Constraints* constraints) {
        for (ConstraintIndex i : indices(constraints->constraints)) {
            if (constraints->constraints[i].isForwarded())
                continue;
            if (!constraints->constraints[i].size())
                continue;
            for (Constraint edge : constraints->constraints[i]) {
                const i8* str = " <: ";
                if (edge.kind == Constraint::Order) str = " ~> ";
                if (edge.kind == Constraint::Substitute) str = " =: ";
                io = format(io, "[TYPE]\t    - ", sys->get(constraints->constrainedTypes[constraints->expand(edge.index)]), str, sys->get(constraints->constrainedTypes[i]), '\n');
            }
        }
        return io;
    }

    inline void printTypeConstraints(TypeSystem* sys, Constraints* constraints) {
        formatTypeConstraints(io_stdout, sys, constraints);
    }
}

#endif