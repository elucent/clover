#ifndef JASMINE_TYPE_H
#define JASMINE_TYPE_H

#include "util/vec.h"
#include "util/maybe.h"
#include "asm/arch.h"

namespace jasmine {
    using TypeIndex = i32;

    #define FOR_EACH_TYPE_KIND(macro) \
        macro(I8, i8, -1) \
        macro(I16, i16, -2) \
        macro(I32, i32, -3) \
        macro(I64, i64, -4) \
        macro(U8, u8, -5) \
        macro(U16, u16, -6) \
        macro(U32, u32, -7) \
        macro(U64, u64, -8) \
        macro(PTR, ptr, -9) \
        macro(REF, ref, -10) \
        macro(F32, f32, -11) \
        macro(F64, f64, -12) \
        macro(VOID, void, -13) \
        macro(UNDEFINED, undefined, -14) \
        macro(BOOL, bool, -15)

    enum TypeKind : i8 {
        #define DEFINE_ENUM(upper, lower, id) upper = id,
        FOR_EACH_TYPE_KIND(DEFINE_ENUM)
        #undef DEFINE_ENUM
        INVALID = -16, EXT = -128
    };

    struct TypeContext;
    struct Function;

    inline bool isInt(TypeIndex type);
    inline bool isUnsigned(TypeIndex type);
    inline bool isSigned(TypeIndex type);
    inline bool isFloat(TypeIndex type);
    inline bool isGPType(Function& fn, TypeIndex type);
    inline bool isFPType(TypeIndex type);
    inline bool isCompound(TypeIndex type);
    inline bool isPointer(TypeIndex type);
    inline bool isStruct(const TypeContext& ctx, TypeIndex type);
    inline bool isVector(const TypeContext& ctx, TypeIndex type);
    inline bool isArray(const TypeContext& ctx, TypeIndex type);
    inline bool isFunction(const TypeContext& ctx, TypeIndex type);
    inline bool isStruct(const Function& fn, TypeIndex type);
    inline bool isVector(const Function& fn, TypeIndex type);
    inline bool isArray(const Function& fn, TypeIndex type);
    inline bool isFunction(const Function& fn, TypeIndex type);

    struct CompoundType {
        enum Kind : u32 {
            STRUCT, ARRAY, VECTOR, FUNCTION, UNION
        };

        union {
            struct { Kind typeKind : 3; u32 hasPointerFields : 1; u32 fieldCount : 28; };
            struct { Kind : 3; u32 : 1; bool isInline : 1; u32 typeLength : 27; };
            struct { Kind : 3; u32 : 1; u32 argumentCount : 28; };
        };

        TypeIndex members[0];

        inline Kind kind() const {
            return typeKind;
        }

        inline TypeIndex returnType() const {
            assert(typeKind == FUNCTION);
            return members[0];
        }

        inline const_slice<TypeIndex> arguments() const {
            assert(typeKind == FUNCTION);
            return { members + 1, argumentCount };
        }

        inline const_slice<TypeIndex> fields() const {
            assert(typeKind == STRUCT);
            return { members, fieldCount };
        }

        inline const_slice<TypeIndex> cases() const {
            assert(typeKind == UNION);
            return { members, fieldCount };
        }

        inline TypeIndex elementType() const {
            assert(typeKind == ARRAY || typeKind == VECTOR);
            return members[0];
        }

        inline u32 length() const {
            assert(typeKind == ARRAY || typeKind == VECTOR);
            return isInline ? typeLength : (u32)members[1];
        }

        inline bool containsPointers() const {
            return hasPointerFields;
        }
    };

    union TypeWord {
        CompoundType type;
        TypeIndex index;
        u32 extendedLength;
    };

    struct Module;
    struct TypeContext;

    struct IncompleteType {
        CompoundType::Kind typeKind;

        inline CompoundType::Kind kind() const { return typeKind; };

        inline TypeIndex returnType() const;
        inline const_slice<TypeIndex> arguments() const;
        inline const_slice<TypeIndex> fields() const;
        inline const_slice<TypeIndex> cases() const;
        inline TypeIndex elementType() const;
        inline u32 length() const;
        template<u32 N>
        inline void encode(const TypeContext& context, vec<TypeWord, N>& storage) const;
    };

    inline const CompoundType& getType(const TypeContext& context, TypeIndex type);

    struct VectorBuilder : public IncompleteType {
        TypeIndex element;
        u32 arrayLength;

        inline VectorBuilder(TypeIndex element_in, u32 length_in):
            IncompleteType { CompoundType::VECTOR }, element(element_in), arrayLength(length_in) {}

        inline TypeIndex elementType() const {
            return element;
        }

        inline u32 length() const {
            return arrayLength;
        }

        template<u32 N>
        inline void encode(const TypeContext& context, vec<TypeWord, N>& storage) const {
            bool canInlineLength = arrayLength <= 0x0fffffff;
            CompoundType header;
            header.typeKind = CompoundType::VECTOR;
            assert(!isCompound(element));
            header.hasPointerFields = element == PTR;
            header.isInline = canInlineLength;
            header.typeLength = canInlineLength ? arrayLength : 0;
            storage.push({ .type = header });
            storage.push({ .index = element });
            if (!canInlineLength)
                storage.push({ .extendedLength = arrayLength });
        }

        inline TypeIndex build(TypeContext& context) const;
    };

    struct ArrayBuilder : public IncompleteType {
        TypeIndex element;
        u32 arrayLength;

        inline ArrayBuilder(TypeIndex element_in, u32 length_in):
            IncompleteType { CompoundType::ARRAY }, element(element_in), arrayLength(length_in) {}

        inline TypeIndex elementType() const {
            return element;
        }

        inline u32 length() const {
            return arrayLength;
        }

        template<u32 N>
        inline void encode(const TypeContext& context, vec<TypeWord, N>& storage) const {
            bool canInlineLength = arrayLength <= 0x0fffffff;
            CompoundType header;
            header.typeKind = CompoundType::ARRAY;
            header.hasPointerFields = element == PTR || (isCompound(element) && getType(context, element).containsPointers());
            header.isInline = canInlineLength;
            header.typeLength = canInlineLength ? arrayLength : 0;
            storage.push({ .type = header });
            storage.push({ .index = element });
            if (!canInlineLength)
                storage.push({ .extendedLength = arrayLength });
        }

        inline TypeIndex build(TypeContext& context) const;
    };

    struct StructBuilder : public IncompleteType {
        vec<TypeIndex, 8> members;

        template<typename... Args>
        inline StructBuilder(Args... args):
            IncompleteType { CompoundType::STRUCT } {
            members.push(args...);
        }

        inline void addField(TypeIndex field) {
            members.push(field);
        }

        inline const_slice<TypeIndex> fields() const {
            return members;
        }

        template<u32 N>
        inline void encode(const TypeContext& context, vec<TypeWord, N>& storage) const {
            CompoundType header;
            header.typeKind = CompoundType::STRUCT;
            header.fieldCount = members.size();

            bool hasPointers = false;
            for (TypeIndex field : members) if (field == PTR || (isCompound(field) && getType(context, field).containsPointers())) {
                hasPointers = true;
                break;
            }
            header.hasPointerFields = hasPointers;

            storage.push({ .type = header });
            for (TypeIndex field : members)
                storage.push({ .index = field });
        }

        inline TypeIndex build(TypeContext& context) const;
    };

    struct UnionBuilder : public IncompleteType {
        vec<TypeIndex, 8> members;

        template<typename... Args>
        inline UnionBuilder(Args... args):
            IncompleteType { CompoundType::UNION } {
            members.push(args...);
        }

        inline void addCase(TypeIndex type) {
            members.push(type);
        }

        inline const_slice<TypeIndex> cases() const {
            return members;
        }

        template<u32 N>
        inline void encode(const TypeContext& context, vec<TypeWord, N>& storage) const {
            CompoundType header;
            header.typeKind = CompoundType::UNION;
            header.fieldCount = members.size();

            bool hasPointers = false;
            for (TypeIndex caseType : members) if (caseType == PTR || (isCompound(caseType) && getType(context, caseType).containsPointers())) {
                hasPointers = true;
                break;
            }
            header.hasPointerFields = hasPointers;

            storage.push({ .type = header });
            for (TypeIndex caseType : members)
                storage.push({ .index = caseType });
        }

        inline TypeIndex build(TypeContext& context) const;
    };

    struct FunctionBuilder : public IncompleteType {
        TypeIndex ret;
        vec<TypeIndex, 8> members;

        template<typename... Args>
        inline FunctionBuilder(TypeIndex returnType, Args... args):
            IncompleteType { CompoundType::FUNCTION }, ret(returnType) {
            members.push(args...);
        }

        inline void addArgument(TypeIndex argument) {
            members.push(argument);
        }

        inline TypeIndex returnType() const {
            return ret;
        }

        inline const_slice<TypeIndex> arguments() const {
            return members;
        }

        template<u32 N>
        inline void encode(const TypeContext& context, vec<TypeWord, N>& storage) const {
            CompoundType header;
            header.typeKind = CompoundType::FUNCTION;
            header.argumentCount = members.size();
            header.hasPointerFields = false;
            storage.push({ .type = header });
            storage.push({ .index = ret });
            for (TypeIndex field : members)
                storage.push({ .index = field });
        }

        inline TypeIndex build(TypeContext& context) const;
    };

    inline TypeIndex IncompleteType::returnType() const {
        if (kind() == CompoundType::FUNCTION)
            return ((const FunctionBuilder*)this)->returnType();
        unreachable("Expected function type.");
    }

    inline const_slice<TypeIndex> IncompleteType::arguments() const {
        if (kind() == CompoundType::FUNCTION)
            return ((const FunctionBuilder*)this)->arguments();
        unreachable("Expected function type.");
    }

    inline const_slice<TypeIndex> IncompleteType::fields() const {
        if (kind() == CompoundType::STRUCT)
            return ((const StructBuilder*)this)->fields();
        unreachable("Expected struct type.");
    }

    inline const_slice<TypeIndex> IncompleteType::cases() const {
        if (kind() == CompoundType::UNION)
            return ((const UnionBuilder*)this)->cases();
        unreachable("Expected union type.");
    }

    inline TypeIndex IncompleteType::elementType() const {
        if (kind() == CompoundType::ARRAY)
            return ((const ArrayBuilder*)this)->elementType();
        if (kind() == CompoundType::VECTOR)
            return ((const VectorBuilder*)this)->elementType();
        unreachable("Expected array or vector type.");
    }

    inline u32 IncompleteType::length() const {
        if (kind() == CompoundType::ARRAY)
            return ((const ArrayBuilder*)this)->length();
        if (kind() == CompoundType::VECTOR)
            return ((const VectorBuilder*)this)->length();
        unreachable("Expected array or vector type.");
    }

    template<u32 N>
    inline void IncompleteType::encode(const TypeContext& context, vec<TypeWord, N>& storage) const {
        switch (kind()) {
            case CompoundType::ARRAY: return ((const ArrayBuilder*)this)->encode<N>(context, storage);
            case CompoundType::STRUCT: return ((const StructBuilder*)this)->encode<N>(context, storage);
            case CompoundType::VECTOR: return ((const VectorBuilder*)this)->encode<N>(context, storage);
            case CompoundType::FUNCTION: return ((const FunctionBuilder*)this)->encode<N>(context, storage);
            case CompoundType::UNION: return ((const UnionBuilder*)this)->encode<N>(context, storage);
            default: unreachable("Unexpected type kind.");
        }
    }

    template<typename TypeLike>
    inline TypeIndex typeIndex(TypeLike t);

    template<>
    inline TypeIndex typeIndex(TypeIndex i) {
        return i;
    }

    template<>
    inline TypeIndex typeIndex(TypeKind i) {
        assert(i != TypeKind::EXT);
        return TypeIndex(i);
    }

    struct TypeLogger {
        const TypeContext& context;
        TypeIndex type;

        inline TypeLogger(const TypeContext& context_in, TypeIndex type_in):
            context(context_in), type(type_in) {}

        template<typename ModuleLike>
        inline TypeLogger(const ModuleLike& module_in, TypeIndex type_in):
            context(module_in.typeContext()), type(type_in) {}
    };

    struct TypeKey {
        const TypeContext* context;
        TypeIndex index;
        const IncompleteType* wip;

        inline TypeKey(const TypeContext& context_in, TypeIndex index_in):
            context(&context_in), index(index_in) {}

        inline TypeKey(const IncompleteType& wip_in):
            index(TypeKind::INVALID), wip(&wip_in) {}

        inline CompoundType::Kind kind() const;
        inline TypeIndex returnType() const;
        inline const_slice<TypeIndex> arguments() const;
        inline const_slice<TypeIndex> fields() const;
        inline const_slice<TypeIndex> cases() const;
        inline TypeIndex elementType() const;
        inline u32 length() const;
    };

    inline bool operator==(const TypeKey& a, const TypeKey& b);
    inline u64 hash(const TypeKey& t);

    struct TypeContext {
        map<TypeKey, TypeIndex> typeTable;
        vec<TypeIndex, 8> types;
        vec<TypeWord, 256> storage;

        inline const CompoundType& operator[](TypeIndex type) const {
            return storage[types[type]].type;
        }

        inline TypeIndex operator[](TypeKey key) {
            auto it = typeTable.find(key);
            if (it != typeTable.end())
                return it->value;
            else {
                TypeIndex result = types.size();
                types.push(storage.size());
                key.wip->encode(*this, storage);
                typeTable.put(TypeKey(*this, result), result);
                return result;
            }
        }

        inline TypeIndex arrayType(TypeIndex element, u32 length) {
            return (*this)[ArrayBuilder(element, length)];
        }

        inline TypeIndex vectorType(TypeIndex element, u32 length) {
            return (*this)[VectorBuilder(element, length)];
        }

        template<typename... Args>
        inline TypeIndex unionType(Args... args) {
            return (*this)[UnionBuilder(args...)];
        }

        template<typename... Args>
        inline UnionBuilder unionBuilder(Args... args) const {
            return UnionBuilder(args...);
        }

        template<typename... Args>
        inline TypeIndex structType(Args... args) {
            return (*this)[StructBuilder(args...)];
        }

        template<typename... Args>
        inline StructBuilder structBuilder(Args... args) const {
            return StructBuilder(args...);
        }

        template<typename... Args>
        inline TypeIndex functionType(TypeIndex returnType, Args... args) {
            return (*this)[FunctionBuilder(returnType, args...)];
        }

        template<typename... Args>
        inline FunctionBuilder functionBuilder(TypeIndex returnType, Args... args) const {
            return FunctionBuilder(returnType, args...);
        }
    };

    inline const CompoundType& getType(const TypeContext& context, TypeIndex type) {
        return context[type];
    }

    inline TypeIndex StructBuilder::build(TypeContext& context) const {
        return context[*this];
    }

    inline TypeIndex UnionBuilder::build(TypeContext& context) const {
        return context[*this];
    }

    inline TypeIndex FunctionBuilder::build(TypeContext& context) const {
        return context[*this];
    }

    inline TypeIndex ArrayBuilder::build(TypeContext& context) const {
        return context[*this];
    }

    inline TypeIndex VectorBuilder::build(TypeContext& context) const {
        return context[*this];
    }

    // TypeKey method implementations.

    inline CompoundType::Kind TypeKey::kind() const {
        return index == TypeKind::INVALID ? wip->kind() : (*context)[index].kind();
    }

    inline TypeIndex TypeKey::returnType() const {
        return index == TypeKind::INVALID ? wip->returnType() : (*context)[index].returnType();
    }

    inline const_slice<TypeIndex> TypeKey::arguments() const {
        return index == TypeKind::INVALID ? wip->arguments() : (*context)[index].arguments();
    }

    inline const_slice<TypeIndex> TypeKey::fields() const {
        return index == TypeKind::INVALID ? wip->fields() : (*context)[index].fields();
    }

    inline const_slice<TypeIndex> TypeKey::cases() const {
        return index == TypeKind::INVALID ? wip->cases() : (*context)[index].cases();
    }

    inline TypeIndex TypeKey::elementType() const {
        return index == TypeKind::INVALID ? wip->elementType() : (*context)[index].elementType();
    }

    inline u32 TypeKey::length() const {
        return index == TypeKind::INVALID ? wip->length() : (*context)[index].length();
    }

    inline bool operator==(const TypeKey& a, const TypeKey& b) {
        if (a.kind() != b.kind())
            return false;
        switch (a.kind()) {
            case CompoundType::VECTOR:
            case CompoundType::ARRAY:
                return a.elementType() == b.elementType() && a.length() == b.length();
            case CompoundType::STRUCT:
                if (a.fields().size() != b.fields().size())
                    return false;
                for (auto [a, b] : zip(a.fields(), b.fields()))
                    if (a != b) return false;
                return true;
            case CompoundType::UNION:
                if (a.cases().size() != b.cases().size())
                    return false;
                for (auto [a, b] : zip(a.cases(), b.cases()))
                    if (a != b) return false;
                return true;
            case CompoundType::FUNCTION:
                if (a.returnType() != b.returnType())
                    return false;
                if (a.arguments().size() != b.arguments().size())
                    return false;
                for (auto [a, b] : zip(a.arguments(), b.arguments()))
                    if (a != b) return false;
                return true;
            default:
                unreachable("Unknown compound type kind.");
        }
    }

    inline u64 hash(const TypeKey& t) {
        switch (t.kind()) {
            case CompoundType::VECTOR:
            case CompoundType::ARRAY:
                return ::hash(t.elementType()) * 6057883947731412179ull + ::hash(t.length());
            case CompoundType::STRUCT: {
                u64 h = 3358053817928655161ull;
                for (auto f : t.fields()) {
                    h += ::hash(f);
                    h *= 2036822541477012631ull;
                }
                return h;
            }
            case CompoundType::UNION: {
                u64 h = 18142656606673910327ull;
                for (auto f : t.cases()) {
                    h += ::hash(f);
                    h *= 15150522336657798631ull;
                }
                return h;
            }
            case CompoundType::FUNCTION: {
                u64 h = ::hash(t.returnType()) * 14356112910883734283ull;
                for (auto f : t.arguments()) {
                    h += ::hash(f);
                    h *= 5606565733596020539ull;
                }
                return h;
            }
            default:
                unreachable("Unknown compound type kind.");
        }
    }

    inline bool isInt(TypeIndex type) {
        return type <= I8 && type >= REF;
    }

    inline bool isUnsigned(TypeIndex type) {
        return type <= U8 && type >= U64;
    }

    inline bool isSigned(TypeIndex type) {
        return isInt(type) && !isUnsigned(type);
    }

    inline bool isFloat(TypeIndex type) {
        return type == F32 || type == F64;
    }

    inline bool isGPType(Function& fn, TypeIndex type) {
        return isInt(type) || type == BOOL || isFunction(fn, type);
    }

    inline bool isFPType(TypeIndex type) {
        return isFloat(type);
    }

    inline bool isCompound(TypeIndex type) {
        return type >= 0;
    }

    inline bool isPointer(TypeIndex type) {
        return type == PTR || type == REF;
    }

    inline bool isStruct(const TypeContext& ctx, TypeIndex type) {
        return type >= 0 && ctx[type].kind() == CompoundType::STRUCT;
    }

    inline bool isVector(const TypeContext& ctx, TypeIndex type) {
        return type >= 0 && ctx[type].kind() == CompoundType::VECTOR;
    }

    inline bool isArray(const TypeContext& ctx, TypeIndex type) {
        return type >= 0 && ctx[type].kind() == CompoundType::ARRAY;
    }

    inline bool isFunction(const TypeContext& ctx, TypeIndex type) {
        return type >= 0 && ctx[type].kind() == CompoundType::FUNCTION;
    }

    inline bool isUnion(const TypeContext& ctx, TypeIndex type) {
        return type >= 0 && ctx[type].kind() == CompoundType::UNION;
    }

    inline bool isScalar(Function& fn, TypeIndex type) {
        return !isCompound(type) || isFunction(fn, type);
    }

    template<TypeKind Kind>
    inline bool fits(i64 value) { return false; }

    template<>
    inline bool fits<TypeKind::BOOL>(i64 value) { return value == 0 || value == 1; }

    template<>
    inline bool fits<TypeKind::I8>(i64 value) { return value >= -0x80 && value <= 0x7f; }

    template<>
    inline bool fits<TypeKind::I16>(i64 value) { return value >= -0x8000 && value <= 0x7fff; }

    template<>
    inline bool fits<TypeKind::I32>(i64 value) { return value >= -0x80000000ll && value <= 0x7fffffffll; }

    template<>
    inline bool fits<TypeKind::U8>(i64 value) { return (value & 0xff) == value; }

    template<>
    inline bool fits<TypeKind::U16>(i64 value) { return (value & 0xffff) == value; }

    template<>
    inline bool fits<TypeKind::U32>(i64 value) { return (value & 0xffffffff) == value; }

    template<>
    inline bool fits<TypeKind::I64>(i64 value) { return true; }

    template<>
    inline bool fits<TypeKind::U64>(i64 value) { return true; }

    template<>
    inline bool fits<TypeKind::PTR>(i64 value) { return true; }

    template<>
    inline bool fits<TypeKind::REF>(i64 value) { return true; }

    inline bool fits(i64 value, TypeIndex type) {
        switch (type) {
            case TypeKind::I8:
                return fits<TypeKind::I8>(value);
            case TypeKind::I16:
                return fits<TypeKind::I16>(value);
            case TypeKind::I32:
                return fits<TypeKind::I32>(value);
            case TypeKind::I64:
                return fits<TypeKind::I64>(value);
            case TypeKind::U8:
                return fits<TypeKind::U8>(value);
            case TypeKind::U16:
                return fits<TypeKind::U16>(value);
            case TypeKind::U32:
                return fits<TypeKind::U32>(value);
            case TypeKind::U64:
                return fits<TypeKind::U64>(value);
            case TypeKind::PTR:
                return fits<TypeKind::PTR>(value);
            case TypeKind::REF:
                return fits<TypeKind::REF>(value);
            case TypeKind::BOOL:
                return fits<TypeKind::BOOL>(value);
            default:
                return false;
        }
    }

    struct Function;

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const TypeLogger& n) {
        if (n.type < 0) {
            switch ((TypeKind)n.type) {
                case I8: return format(io, "i8");
                case I16: return format(io, "i16");
                case I32: return format(io, "i32");
                case I64: return format(io, "i64");
                case U8: return format(io, "u8");
                case U16: return format(io, "u16");
                case U32: return format(io, "u32");
                case U64: return format(io, "u64");
                case PTR: return format(io, "ptr");
                case REF: return format(io, "ref");
                case F32: return format(io, "f32");
                case F64: return format(io, "f64");
                case VOID: return format(io, "void");
                case BOOL: return format(io, "bool");
                default:
                    unreachable("Unexpected type kind.");
            }
        }
        const CompoundType& type = n.context[n.type];
        bool first = true;
        switch (type.kind()) {
            case CompoundType::ARRAY:
                return format(io, TypeLogger(n.context, type.elementType()), '[', type.length(), ']');
            case CompoundType::VECTOR:
                return format(io, '<', TypeLogger(n.context, type.elementType()), 'x', type.length(), '>');
            case CompoundType::STRUCT:
                io = format(io, '{');
                for (auto t : type.fields()) {
                    if (!first) io = format(io, ", ");
                    first = false;
                    io = format(io, TypeLogger(n.context, t));
                }
                return format(io, '}');
            case CompoundType::UNION:
                for (auto t : type.cases()) {
                    if (!first) io = format(io, "|");
                    first = false;
                    io = format(io, TypeLogger(n.context, t));
                }
                return io;
            case CompoundType::FUNCTION:
                io = format(io, TypeLogger(n.context, type.returnType()));
                io = format(io, '(');
                for (auto t : type.arguments()) {
                    if (!first) io = format(io, ", ");
                    first = false;
                    io = format(io, TypeLogger(n.context, t));
                }
                return format(io, ')');
        }
    }
}

#endif