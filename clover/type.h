#ifndef BASIL_CLOVER_TYPE_H
#define BASIL_CLOVER_TYPE_H

#include "core/def.h"
#include "core/util.h"
#include "lib/slice.h"
#include "lib/tuple.h"
#include "lib/malloc.h"
#include "lib/hash.h"
#include "clover/clover.h"
#include "clover/env.h"

struct Type;

u64 hash(Type* type);
constexpr u64(*type_hash)(Type*) = hash;
bool operator==(const Type& a, const Type& b);

struct TypeKey {
    u64 h;
    Type* type;

    inline TypeKey(Type* t): h(hash(t)), type(t) {}

    inline bool operator==(const TypeKey& other) const {
        return *type == *other.type;
    }
};

inline u64 hash(TypeKey key) {
    return key.h;
}

struct TypeContext {
    arena typespace;
    map<TypeKey, Type*> typemap;
    EnvContext* envctx;
    Interner* interner;

    inline TypeContext(EnvContext* envctx_in, Interner* interner_in): typespace(256), envctx(envctx_in), interner(interner_in) {}

    template<typename T, typename... Args>
    inline Type* def(Args... args) {
        T local(args...);
        auto it = typemap.find({&local});
        if (it == typemap.end()) {
            T* ptr = (T*)typespace.alloc(sizeof(T));
            mcpy(ptr, &local, sizeof(T));
            ptr->init_env(this);
            typemap.put({ptr}, ptr);
            return ptr;
        }
        else return it->value;
    }

    inline Type* defvar(Module* mod, Env* env, i32 nick = -1);
};

enum TypeKind : i8 {
    T_UNIT, T_NUMERIC, T_PTR, T_ARRAY, T_SLICE, T_FUN, T_VAR,
    T_CHAR, T_STRING, T_BOOL, T_VOID, T_NAMED, T_STRUCT, T_UNION,
    T_ANY_NUMERIC, T_ANY,
    T_TYPE, T_ERROR
};

struct Type {
    Env* env = nullptr;
    i32 mangled = -1, caseid = -1;
    TypeKind kind;
    bool referenced_by_name = false, ctor_called = false, gen_placement_new = false, is_case = false, is_prototype = false;

    inline Type(TypeKind kind_in): kind(kind_in) {}

    inline void init_env(TypeContext* typectx) {}
};

extern Type *VOID, *UNIT, *I8, *I16, *I32, *I64, *INT, *IPTR, *ICONST8, *ICONST16, *ICONST32, *ICONST64, *F32, *F64, *FLOAT, *BOOL, *CHAR, *STRING, *TYPE, *ERROR,
    *ANY, *ANY_NUMERIC, *ANY_ARRAY, *ANY_PTR, *ANY_SLICE, *ANY_FUNCTION;

struct TypeTuple {
    slice<Type*> types;
    u64 h;

    TypeTuple(slice<Type*> types_in);
};

inline u64 hash(const TypeTuple& tup) {
    return tup.h;
}

inline bool operator==(const TypeTuple& a, const TypeTuple& b) {
    if (a.types.n != b.types.n) return false;
    for (iptr i = 0; i < a.types.n; i ++) if (a.types[i] != b.types[i]) return false;
    return true;
}

struct NumericType : Type {
    bool floating, literal;
    i32 bytes;

    inline NumericType(bool floating_in, bool literal_in, i32 bytes_in): Type(T_NUMERIC), floating(floating_in), literal(literal_in), bytes(bytes_in) {}

    inline void init_env(TypeContext* typectx) {}
};

inline Type* simplify(Type* t) {
    if (t == ICONST8) return INT;
    if (t == ICONST16) return INT;
    if (t == ICONST32) return INT;
    if (t == ICONST64) return INT;
    return t;
}

struct PtrType : Type {
    Type* target;

    inline PtrType(Type* target_in): Type(T_PTR), target(simplify(target_in)) {}
    
    void init_env(TypeContext* typectx);
};

struct ArrayType : Type {
    i32 size;
    Type* element;

    inline ArrayType(Type* element_in, i32 size_in): Type(T_ARRAY), size(size_in), element(element_in) {}
    inline ArrayType(Type* element_in): Type(T_ARRAY), size(-1), element(element_in) {}
    
    void init_env(TypeContext* typectx);
};

struct SliceType : Type {
    Type* element;

    inline SliceType(Type* element_in): Type(T_SLICE), element(simplify(element_in)) {}

    void init_env(TypeContext* typectx);
};

struct FunType : Type {
    slice<Type*> arg;
    Type* ret;
    bool nary;
    u64 hash;

    inline FunType(Type* ret_in): Type(T_FUN), ret(simplify(ret_in)), nary(true) {
        hash = 4318138567807710449ul * ::hash(ret);
        hash ^= 10126270993744429001ul;
    }

    inline FunType(slice<Type*> arg_in, Type* ret_in): Type(T_FUN), arg(arg_in), ret(simplify(ret_in)), nary(false) {
        hash = 4318138567807710449ul * ::hash(ret);
        if (arg.n == 0) unreachable("Somehow function type has no arguments.");
        for (Type*& t : arg) t = simplify(t), hash *= 10126270993744429001ul, hash ^= ::hash(t);
    }

    void init_env(TypeContext* typectx);
};

struct NamedType : Type {
    i32 name;
    Type* inner;

    inline NamedType(i32 name_in, Env* env_in, Type* inner_in): Type(T_NAMED), name(name_in), inner(simplify(inner_in)) { env = env_in; }

    inline void init_env(TypeContext* typectx) {}
};

struct StructType : Type {
    i32 name;
    slice<pair<i32, Type*>> fields;

    inline StructType(i32 name_in, Env* env_in, slice<pair<i32, Type*>> fields_in): Type(T_STRUCT), name(name_in), fields(fields_in) { 
        env = env_in; 
        for (auto& p : fields) p.second = simplify(p.second);
    }

    inline void init_env(TypeContext* typectx) {}
};

struct UnionType : Type {
    i32 name;
    slice<pair<i32, Type*>> fields;

    inline UnionType(i32 name_in, Env* env_in, slice<pair<i32, Type*>> fields_in): Type(T_UNION), name(name_in), fields(fields_in) { 
        env = env_in;
        i32 i = 0;
        for (auto& p : fields) p.second = simplify(p.second), p.second->caseid = i ++;
    }

    inline void init_env(TypeContext* typectx) {}
};

struct VarType : Type {
    i32 id, nick;
    Type** binding;

    inline VarType(Module* mod, Env* env_in, i32 nick_in = -1): Type(T_VAR), id(env_in->tvar ++), nick(nick_in), binding((Type**)mod->typectx->typespace.alloc(sizeof(Type*))) {
        *binding = VOID;
        env = env_in;
    }

    inline void init_env(TypeContext* typectx) {}
};

inline Type* TypeContext::defvar(Module* mod, Env* env, i32 nick) {
    VarType* var = (VarType*)typespace.alloc(sizeof(VarType));
    new(var) VarType(mod, env, nick);
    return var;
}

Type* unify(Type* a, Type* b);
bool is_subtype(Type* src, Type* dest);
bool is_subtype_generic(Type* src, Type* dest);
i32 size(Type* type);
void format(stream& io, Module* mod, Type* type);

inline bool is_named(Type* type) {
    return type->kind >= T_NAMED;
}

inline Env* type_env(Type* type) {
    return type->env;
}

inline bool isconcrete(Type* type) {
    switch (type->kind) {
        case T_NUMERIC:
        case T_UNIT:
        case T_STRING:
        case T_CHAR:
        case T_BOOL:
        case T_VOID:
        case T_TYPE:
        case T_NAMED:
        case T_STRUCT:
        case T_UNION:
            return true;
        case T_VAR:
        case T_ANY:
        case T_ANY_NUMERIC:
        case T_ERROR:
            return false;
        case T_PTR:
            return isconcrete(((PtrType*)type)->target);
        case T_SLICE:
            return isconcrete(((SliceType*)type)->element);
        case T_ARRAY:
            return isconcrete(((ArrayType*)type)->element) && ((ArrayType*)type)->size >= 0;
        case T_FUN:
            if (!isconcrete(((FunType*)type)->ret)) return false;
            for (Type* t : ((FunType*)type)->arg) if (!isconcrete(t)) return false;
            return true;
        default:
            unreachable("Unexpected typekind!");
            return false;
    }   
}

inline Type* shallowconcrete(Type* type) {
    if (!type) return ERROR;
    switch (type->kind) {
        case T_NUMERIC:
        case T_UNIT:
        case T_STRING:
        case T_CHAR:
        case T_BOOL:
        case T_VOID:
        case T_ERROR:
        case T_TYPE:
        case T_NAMED:
        case T_STRUCT:
        case T_UNION:
        case T_PTR:
        case T_SLICE:
        case T_ARRAY:
        case T_FUN:
            return type;
        case T_VAR:
            type = shallowconcrete(*((VarType*)type)->binding);
            if (type == VOID || type == ERROR) return ERROR;
            else return type;
        case T_ANY:
        case T_ANY_NUMERIC:
            return ERROR;
        default:
            unreachable("Unexpected type kind!");
            return ERROR;
    }   
}

inline Type* fullconcrete(TypeContext& ctx, Type* type) {
    if (!type) return ERROR;
    switch (type->kind) {
        case T_NUMERIC:
            return type;
        case T_UNIT:
        case T_STRING:
        case T_CHAR:
        case T_BOOL:
        case T_VOID:
        case T_ERROR:
        case T_TYPE:
        case T_NAMED:
        case T_STRUCT:
        case T_UNION:
            return type;
        case T_VAR:
            type = fullconcrete(ctx, *((VarType*)type)->binding);
            if (type == VOID || type == ERROR) return ERROR;
            else return type;
        case T_PTR:
            type = fullconcrete(ctx, ((PtrType*)type)->target);
            if (type == VOID || type == ERROR) return ERROR;
            else return ctx.def<PtrType>(type);
        case T_SLICE:
            type = fullconcrete(ctx, ((PtrType*)type)->target);
            if (type == VOID || type == ERROR) return ERROR;
            else return ctx.def<SliceType>(type);
        case T_ARRAY: {
            if (((ArrayType*)type)->size < 0) return ERROR;
            Type* element = fullconcrete(ctx, ((ArrayType*)type)->element);
            if (element == VOID || element == ERROR) return ERROR;
            else return ctx.def<ArrayType>(element, ((ArrayType*)type)->size);
        }
        case T_FUN: {
            if (((FunType*)type)->nary) return ERROR;
            slice<Type*> concreted = { new(ctx.typespace) Type*[((FunType*)type)->arg.n], ((FunType*)type)->arg.n};
            for (iptr i = 0; i < concreted.n; i ++) {
                concreted[i] = fullconcrete(ctx, ((FunType*)type)->arg[i]);
                if (concreted[i] == VOID || concreted[i] == ERROR) return ERROR;
            }
            return ctx.def<FunType>(concreted, fullconcrete(ctx, ((FunType*)type)->ret));
        }
        case T_ANY:
        case T_ANY_NUMERIC:
            return ERROR;
        default:
            unreachable("Unexpected type kind!");
            return ERROR;
    }   
}

inline Type* fullsimplify(TypeContext& ctx, Type* type) {
    if (!type) return ERROR;
    switch (type->kind) {
        case T_NUMERIC:
            return simplify(type);
        case T_UNIT:
        case T_STRING:
        case T_CHAR:
        case T_BOOL:
        case T_VOID:
        case T_ERROR:
        case T_TYPE:
        case T_NAMED:
        case T_STRUCT:
        case T_UNION:
            return type;
        case T_VAR:
            type = fullsimplify(ctx, *((VarType*)type)->binding);
            if (type == VOID || type == ERROR) return ERROR;
            else return type;
        case T_PTR:
            type = fullsimplify(ctx, ((PtrType*)type)->target);
            if (type == VOID || type == ERROR) return ERROR;
            else return ctx.def<PtrType>(type);
        case T_SLICE:
            type = fullsimplify(ctx, ((PtrType*)type)->target);
            if (type == VOID || type == ERROR) return ERROR;
            else return ctx.def<SliceType>(type);
        case T_ARRAY: {
            if (((ArrayType*)type)->size < 0) return ERROR;
            Type* element = fullsimplify(ctx, ((ArrayType*)type)->element);
            if (element == VOID || element == ERROR) return ERROR;
            else return ctx.def<ArrayType>(element, ((ArrayType*)type)->size);
        }
        case T_FUN: {
            if (((FunType*)type)->nary) return ERROR;
            slice<Type*> concreted = { new(ctx.typespace) Type*[((FunType*)type)->arg.n], ((FunType*)type)->arg.n};
            for (iptr i = 0; i < concreted.n; i ++) {
                concreted[i] = fullsimplify(ctx, ((FunType*)type)->arg[i]);
                if (concreted[i] == VOID || concreted[i] == ERROR) return ERROR;
            }
            return ctx.def<FunType>(concreted, fullsimplify(ctx, ((FunType*)type)->ret));
        }
        case T_ANY:
        case T_ANY_NUMERIC:
        default:
            return ERROR;
    }
}

inline void unbind(Type* type) {
    if (!type) return;
    switch (type->kind) {
        case T_NUMERIC:
        case T_UNIT:
        case T_STRING:
        case T_CHAR:
        case T_BOOL:
        case T_VOID:
        case T_ERROR:
        case T_TYPE:
        case T_NAMED:
        case T_STRUCT:
        case T_UNION:
        case T_ANY:
        case T_ANY_NUMERIC:
            return;
        case T_VAR:
            *((VarType*)type)->binding = VOID;
            return;
        case T_PTR:
            return unbind(((PtrType*)type)->target);
        case T_SLICE:
            return unbind(((SliceType*)type)->element);
        case T_ARRAY:
            return unbind(((ArrayType*)type)->element);
        case T_FUN: {
            unbind(((FunType*)type)->ret);
            if (!((FunType*)type)->nary) for (Type* t : ((FunType*)type)->arg) unbind(t);
            return;
        }
    }   
}

#endif