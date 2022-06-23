#include "clover/ast.h"
#include "clover/env.h"
#include "clover/lex.h"
#include "clover/type.h"
#include "lib/vec.h"

void space(stream& io, i32 indent) {
    while (indent --) write(io, ' ');
}

static const i8* ENV_LABELS[] = {
    "ROOT ", "GLOBAL ", "FUN ", "MOD ", "TYPE ", "LOCAL "
};

void Env::format(stream& io, Module* mod, i32 indent) {
    space(io, indent);
    write(io, ENV_LABELS[kind], mod->interner->str(name), ":\n");
    for (auto& p : entries) {
        switch (p.value.kind) {
        case E_VAR:
            space(io, indent);
            write(io, " - VAR ", mod->interner->str(p.key), '\n');
            break;
        case E_GENFUN:
        case E_FUN:
            if (p.value.ast) ((FunDecl*)p.value.ast)->env->format(io, mod, indent + 3);
            else space(io, indent), write(io, " - FUN ", mod->interner->str(p.key), '\n');
            break;
        case E_GENTYPE:
        case E_TYPE:
            if (p.value.ast) ((TypeDecl*)p.value.ast)->env->format(io, mod, indent + 3);
            else if (Env* tenv = type_env(p.value.type)) tenv->format(io, mod, indent + 3);
            break;
        case E_CASE:
            ((CaseDecl*)p.value.ast)->env->format(io, mod, indent + 3);
            break;
        case E_GLOBAL:
            ((ASTProgram*)p.value.ast)->env->format(io, mod, indent + 3);
            break;
        case E_ALIAS:
            space(io, indent);
            write(io, " - ALIAS ", mod->interner->str(p.key), '\n');
            break;
        case E_MOD:
            ((ModuleDecl*)p.value.ast)->env->format(io, mod, indent + 3);
            break;
        }
    }
}

void EnvContext::create_root_env(Interner& syms, TypeContext& types) {
    Env* env = create(ENV_ROOT, nullptr, syms.intern("root"));

    env->def(syms.intern("i8"), e_type(I8 = types.def<NumericType>(false, false, 1), nullptr));
    env->def(syms.intern("i16"), e_type(I16 = types.def<NumericType>(false, false, 2), nullptr));
    env->def(syms.intern("i32"), e_type(I32 = types.def<NumericType>(false, false, 4), nullptr));
    env->def(syms.intern("i64"), e_type(I64 = types.def<NumericType>(false, false, 8), nullptr));
    env->def(syms.intern("int"), e_type(INT = types.def<NumericType>(false, false, sizeof(iptr)), nullptr));
    env->def(syms.intern("iptr"), e_type(IPTR = types.def<NumericType>(false, false, sizeof(iptr)), nullptr));
    ICONST = types.def<NumericType>(false, true, 8);
    
    env->def(syms.intern("f32"), e_type(F32 = types.def<NumericType>(true, false, 4), nullptr));
    env->def(syms.intern("f64"), e_type(F64 = types.def<NumericType>(true, false, 8), nullptr));
    env->def(syms.intern("float"), e_type(FLOAT = types.def<NumericType>(true, false, 8), nullptr));
    FCONST = types.def<NumericType>(true, true, 8);
    
    env->def(syms.intern("void"), e_type(VOID = types.def<Type>(T_VOID), nullptr));
    env->def(syms.intern("unit"), e_type(UNIT = types.def<Type>(T_UNIT), nullptr));
    env->def(syms.intern("char"), e_type(CHAR = types.def<Type>(T_CHAR), nullptr));
    env->def(syms.intern("string"), e_type(STRING = types.def<Type>(T_STRING), nullptr));
    env->def(syms.intern("bool"), e_type(BOOL = types.def<Type>(T_BOOL), nullptr));
    ERROR = types.def<Type>(T_ERROR);
    TYPE = types.def<Type>(T_TYPE);

    I8->env = create(ENV_TYPE, env, syms.intern("i8"));
    I16->env = create(ENV_TYPE, env, syms.intern("i16"));
    I32->env = create(ENV_TYPE, env, syms.intern("i32"));
    I64->env = create(ENV_TYPE, env, syms.intern("i64"));
    INT->env = create(ENV_TYPE, env, syms.intern("int"));
    IPTR->env = create(ENV_TYPE, env, syms.intern("iptr"));
    ICONST->env = INT->env;
    F32->env = create(ENV_TYPE, env, syms.intern("f32"));
    F64->env = create(ENV_TYPE, env, syms.intern("f64"));
    FLOAT->env = create(ENV_TYPE, env, syms.intern("float"));
    FCONST->env = FLOAT->env;
    VOID->env = create(ENV_TYPE, env, syms.intern("void"));
    UNIT->env = create(ENV_TYPE, env, syms.intern("unit"));
    CHAR->env = create(ENV_TYPE, env, syms.intern("char"));
    STRING->env = create(ENV_TYPE, env, syms.intern("string"));
    BOOL->env = create(ENV_TYPE, env, syms.intern("bool"));

    root = env;

    auto def_print = [&](Type* base) {
        slice<Type*> args = { new(types.typespace) Type*[1], 1 };
        args[0] = base;  
        Type* ft = types.def<FunType>(args, VOID);
        base->env->def(syms.intern("print"), e_fun(ft, nullptr));
        add_method(syms.intern("print"), base, base->env);
    };

    def_print(INT);
    def_print(FLOAT);
    def_print(BOOL);
    def_print(UNIT);
    def_print(STRING);
    def_print(CHAR);

    auto def_pow = [&](Type* base) {
        slice<Type*> args = { new(types.typespace) Type*[2], 2 };
        args[0] = base;  
        args[1] = base;  
        Type* ft = types.def<FunType>(args, base);
        base->env->def(syms.intern("$pow"), e_fun(ft, nullptr));
        add_method(syms.intern("$pow"), base, base->env);
    };

    def_pow(I8);
    def_pow(I16);
    def_pow(I32);
    def_pow(I64);
    def_pow(F32);
    def_pow(F64);
}

void EnvContext::add_method(i32 name, Type* type, Env* env) {
    auto it = methods.find(name);
    if (it == methods.end()) {
        vec<pair<Type*, Env*>, 16, arena> v;
        v.alloc = &envspace;
        v.push({type, env});
        methods.put(name, v);
    }
    else it->value.push({type, env});
}

void EnvContext::add_generic_method(i32 name, FunDecl* decl) {
    auto it = generic_methods.find(name);
    if (it == generic_methods.end()) generic_methods.put(name, decl);
}

pair<Type*, Env*>* EnvContext::find_method(i32 name, Type* type) {
    type = concrete(type);
    auto it = methods.find(name);
    if (it != methods.end()) {
        for (auto& p : it->value) if (type == p.first) return &p;
        for (auto& p : it->value) if (is_subtype(type, p.first)) return &p;
    }
    return nullptr;
}