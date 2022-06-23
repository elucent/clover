#ifndef BASIL_CLOVER_ENV_H
#define BASIL_CLOVER_ENV_H

#include "lib/malloc.h"
#include "lib/hash.h"
#include "lib/vec.h"
#include "clover/clover.h"

enum EntryKind : i8 {
    E_GLOBAL, E_VAR, E_FUN, E_GENFUN, E_MOD, E_TYPE, E_GENTYPE, E_ALIAS, E_CASE
};

enum EnvKind : i8 {
    ENV_ROOT, ENV_GLOBAL, ENV_FUN, ENV_MOD, ENV_TYPE, ENV_LOCAL
};

inline bool is_type(EntryKind kind) {
    return kind >= E_TYPE;
}

struct AST;
struct Type;

struct Entry {
    AST* ast;
    Type* type;
    EntryKind kind;
};

inline Entry e_var(Type* type, AST* decl) {
    return { decl, type, E_VAR };
}

inline Entry e_fun(Type* type, AST* decl) {
    return { decl, type, E_FUN };
}

inline Entry e_genfun(AST* decl) {
    return { decl, nullptr, E_GENFUN };
}

inline Entry e_type(Type* type, AST* decl) {
    return { decl, type, E_TYPE };
}

inline Entry e_gentype(AST* decl) {
    return { decl, nullptr, E_GENTYPE };
}

inline Entry e_case(Type* type, AST* decl) {
    return { decl, type, E_CASE };
}

inline Entry e_alias(Type* type, AST* decl) {
    return { decl, type, E_ALIAS };
}

inline Entry e_mod(AST* module) {
    return { module, nullptr, E_MOD };
}

inline Entry e_global(AST* decl) {
    return { decl, nullptr, E_GLOBAL };
}

struct Env {
    map<i32, Entry, 8, arena> entries;
    Env* parent;
    vec<Env*, 8, arena> siblings;
    i32 name, anon = 0, tvar = 0, fqname = -1;
    u64 hash;
    AST* decl = nullptr;
    EnvKind kind;

    inline Entry* lookup(i32 name) {
        auto it = entries.find(name);
        if (it != entries.end()) return &it->value;
        if (siblings.size()) for (Env* env : siblings) {
            auto sit = env->entries.find(name);
            if (sit != env->entries.end()) return &sit->value;
        }
        if (parent) return parent->lookup(name);
        return nullptr;
    }

    inline Env* find(i32 name) {
        auto it = entries.find(name);
        if (it != entries.end()) return this;
        if (siblings.size()) for (Env* env : siblings) {
            auto sit = env->entries.find(name);
            if (sit != env->entries.end()) return env;
        }
        if (parent) return parent->find(name);
        return nullptr;
    }

    inline bool def(i32 name, Entry e) {
        auto it = entries.find(name);
        if (it == entries.end()) {
            entries.put(name, e);
            return true;
        }
        return false;
    }

    void format(stream& io, Module* mod, i32 indent);
};

struct FunDecl;

struct EnvContext {
    arena envspace;
    Env* root;
    map<i32, vec<pair<Type*, Env*>, 16, arena>, 256, arena> methods;
    map<i32, FunDecl*, 256, arena> generic_methods;

    inline EnvContext() {
        methods.alloc = &envspace;
        generic_methods.alloc = &envspace;
    }

    void add_method(i32 name, Type* type, Env* decl);
    void add_generic_method(i32 name, FunDecl* decl);
    pair<Type*, Env*>* find_method(i32 name, Type* type);

    inline Env* create(EnvKind kind, Env* parent, i32 name) {
        Env* env = new(envspace) Env;
        env->kind = kind;
        env->entries.alloc = &envspace;
        env->parent = parent;
        env->siblings.alloc = &envspace;
        env->name = name;
        env->hash = env->name;
        if (parent) env->hash ^= parent->hash * 7039452210068278519ul;
        return env;
    }

    void create_root_env(Interner& syms, TypeContext& types);
};

#endif