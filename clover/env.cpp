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
        case E_CONST:
            space(io, indent);
            write(io, " - CONST ", mod->interner->str(p.key), '\n');
            break;
        case E_GENFUN:
        case E_FUN:
            if (p.value.ast && ((FunDecl*)p.value.ast)->env != this) 
                ((FunDecl*)p.value.ast)->env->format(io, mod, indent + 3);
            else space(io, indent), write(io, " - FUN ", mod->interner->str(p.key), '\n');
            break;
        case E_GENTYPE:
        case E_TYPE:
            if (p.value.type && p.value.type->kind == T_VAR) {
                space(io, indent);
                write(io, " - TYPE ", mod->interner->str(p.key), '\n');
            }
            else if (p.value.ast && ((TypeDecl*)p.value.ast)->env != this) ((TypeDecl*)p.value.ast)->env->format(io, mod, indent + 3);
            else if (Env* tenv = type_env(p.value.type)) if (tenv != this) tenv->format(io, mod, indent + 3);
            break;
        case E_CASE:
            if (((CaseDecl*)p.value.ast)->env != this) 
                ((CaseDecl*)p.value.ast)->env->format(io, mod, indent + 3);
            break;
        case E_GLOBAL:
            if (((ASTProgram*)p.value.ast)->env != this) 
                ((ASTProgram*)p.value.ast)->env->format(io, mod, indent + 3);
            break;
        case E_ALIAS:
            space(io, indent);
            write(io, " - ALIAS ", mod->interner->str(p.key), '\n');
            break;
        case E_MOD:
            if (((ModuleDecl*)p.value.ast)->env != this) 
                ((ModuleDecl*)p.value.ast)->env->format(io, mod, indent + 3);
            break;
        }
    }
    for (auto loc : locals) loc->format(io, mod, indent + 3);
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
    ANY = types.def<Type>(T_ANY);

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

    ANY_PTR = types.def<PtrType>(ANY);
    ANY_SLICE = types.def<SliceType>(ANY);
    ANY_ARRAY = types.def<ArrayType>(ANY);
    ANY_FUNCTION = types.def<FunType>(ANY);
    ANY_NUMERIC = types.def<Type>(T_ANY_NUMERIC);

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
        base->env->def(syms.intern("pow"), e_fun(ft, nullptr));
        add_method(syms.intern("pow"), base, base->env);
    };

    def_pow(I8);
    def_pow(I16);
    def_pow(I32);
    def_pow(I64);
    def_pow(F32);
    def_pow(F64);
}

void EnvContext::add_method(i32 name, Type* type, Env* env) {
    if (in_prototype) return;
    auto it = nonconcrete_methods.find(name);
    if (it == nonconcrete_methods.end()) {
        vec<pair<Type*, Env*>, 16, arena> v;
        v.alloc = &envspace;
        v.push({type, env});
        nonconcrete_methods.put(name, v);
    }
    else it->value.push({type, env});
}

void EnvContext::finalize_methods(Module* mod) {
    for (auto& e : nonconcrete_methods) {
        auto it = methods.find(e.key);
        if (it == methods.end()) {
            vec<pair<Type*, Env*>, 16, arena> v;
            v.alloc = &envspace;
            for (const auto& p : e.value) 
                v.push({fullsimplify(*mod->typectx, p.first), p.second});
            methods.put(e.key, v);
        }
        else for (const auto& p : e.value)
            it->value.push({fullsimplify(*mod->typectx, p.first), p.second});
    }
    nonconcrete_methods.clear();
}

i32 anon_sym(Module* mod, i32 n) {
    i8 buf[16];
    i32 len = 0;

    i32 id = n;
    buf[len ++] = '_';
    if (!id) buf[len ++] = '0';
    else {
        u32 olen = len;
        u32 c = 0, d = 0;
        u64 p = 1;
        while (p <= id) p *= 10, ++ c;
        d = c;
        while (c >= 1) {
            buf[-- c + olen] = "0123456789"[id % 10];
            id /= 10;
        }
        len += d;
    }
    slice<i8> sl = slice<i8>{new(mod->envctx->envspace) i8[len], len};
    mcpy(sl.ptr, buf, len);
    i32 name = mod->interner->intern(sl);
    return name;
}

AST* EnvContext::create_method(i32 name, Type* type, Module* mod, vec<pair<Type*, Env*>*, 64, arena> methods) {
    for (const auto& m : methods) {
        if (!m->second) return nullptr;
        auto e = m->second->lookup(name);
        if (!e || e->type->kind != T_FUN) return nullptr;
    }
    Entry* e0 = methods[0]->second->lookup(name);
    FunType* ft0 = (FunType*)e0->type;
    slice<Type*> args0 = ft0->arg;
    Type* ret = ft0->ret;
    for (i32 i = 1; i < methods.size(); i ++) {
        Entry* en = methods[i]->second->lookup(name);
        FunType* ftn = (FunType*)en->type;
        slice<Type*> argsn = ftn->arg;
        Type* retn = ftn->ret;
        if (ret != retn) return nullptr;
        if (argsn.n != args0.n) return nullptr;
        for (i32 j = 1; j < args0.n; j ++) {
            if (args0[j] != argsn[j]) return nullptr;
        }
    }

    slice<AST*> params = { new(mod->parser->astspace) AST*[args0.size() - 1], args0.size() - 1 };
    for (i32 i = 1; i < args0.n; i ++) {
        Type* argt = args0[i];
        i32 argname = anon_sym(mod, i);
        AST* type = new(mod->parser->astspace) Var({}, argt->env->name);
        type->kind = AST_TYPENAME, type->type = argt;
        params[i - 1] = new(mod->parser->astspace) VarDecl({}, type, new(mod->parser->astspace) Var({}, argname), nullptr);
    }
    AST* dotted_name = new(mod->parser->astspace) Binary(AST_DOT, {},
        new(mod->parser->astspace) Var({}, type->env->name),
        new(mod->parser->astspace) Var({}, name)
    );
    AST* sig = new(mod->parser->astspace) Apply({}, dotted_name, params);

    slice<AST*> cases = { new(mod->parser->astspace) AST*[methods.size()], methods.size() };
    i32 casename = mod->interner->intern("_p");
    for (i32 i = 0; i < methods.size(); i ++) {
        AST* d = methods[i]->second->decl;
        Type* ct = ((UnionType*)type)->fields[i].second;
        AST* caset = new(mod->parser->astspace) Var({}, ct->env->name);
        caset->kind = AST_TYPENAME, caset->type = ct;
        AST* decl = new(mod->parser->astspace) VarDecl({}, caset, new(mod->parser->astspace) Var({}, casename), nullptr);
        
        slice<AST*> bodyterms = { new(mod->parser->astspace) AST*[1], 1 };
        slice<AST*> callargs = { new(mod->parser->astspace) AST*[args0.n], args0.n };
        callargs[0] = new(mod->parser->astspace) Var({}, casename);
        for (i32 i = 1; i < args0.n; i ++) callargs[i] = new(mod->parser->astspace) Var({}, ((Var*)((VarDecl*)params[i])->name)->name);
        bodyterms[0] = new(mod->parser->astspace) Apply({}, new(mod->parser->astspace) Var({}, name), callargs);
        bodyterms[0] = new(mod->parser->astspace) Unary(AST_RETURN, {}, bodyterms[0]);
        AST* body = new(mod->parser->astspace) List(AST_DO, {}, bodyterms);
        
        cases[i] = new(mod->parser->astspace) CaseDecl({}, decl, body); 
    }
    AST* body = new(mod->parser->astspace) Binary(AST_MATCH, {}, new(mod->parser->astspace) Var({}, mod->interner->intern("this")), new(mod->parser->astspace) List(AST_DO, {}, cases));
    slice<AST*> body_block = { new(mod->parser->astspace) AST*[1], 1 };
    body_block[0] = body;
    AST* rettype = new(mod->parser->astspace) Var({}, ret->env->name);
    rettype->kind = AST_TYPENAME, rettype->type = ret;
    FunDecl* fun = new(mod->parser->astspace) FunDecl({}, rettype, sig, new(mod->parser->astspace) List(AST_DO, {}, body_block));

    compute_envs(mod, type->env, fun);
    // print("auto generated "), format(stdout, mod, fun), print('\n');
    detect_types(mod, type->env, fun);
    infer(mod, type->env, fun);
    typecheck(mod, type->env, fun);
    return fun;
}

pair<Type*, Env*>* EnvContext::find_method(i32 name, Type* type, Module* mod) {
    auto it = methods.find(name);
    if (it != methods.end()) {
        for (auto& p : it->value) if (type == p.first) return &p;
        for (auto& p : it->value) if (is_subtype(type, p.first)) return &p;
    }
    it = nonconcrete_methods.find(name);
    if (it != nonconcrete_methods.end()) {
        for (auto& p : it->value) if (type == p.first) return &p;
        for (auto& p : it->value) if (is_subtype(type, p.first)) return &p;
    }

    // Consider pointer matching method:
    if (type->kind == T_PTR) {
        return find_method(name, ((PtrType*)type)->target, mod);
    }
    // Consider generic matching method:
    if (type->kind == T_UNION) {
        vec<pair<Type*, Env*>*, 64, arena> methods;
        methods.alloc = &envspace;

        for (const auto& field : ((UnionType*)type)->fields) {
            auto method = find_method(name, field.second, mod);
            if (method) methods.push(method);
            else break;
        }

        if (methods.size() == ((UnionType*)type)->fields.n) {
            AST* auto_method = create_method(name, type, mod, methods);
            if (!auto_method) return nullptr;

            add_method(name, type, ((FunDecl*)auto_method)->env);
            mod->automethods.push(auto_method);
            return find_method(name, type, mod);
        }
    }
    return nullptr;
}