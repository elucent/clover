#include "clover/type.h"
#include "clover/lex.h"
#include "clover/ast.h"

TypeTuple::TypeTuple(slice<Type*> types_in): types(types_in) {
    h = 3958494954898279007ul;
    for (Type* type : types) {
        h *= 11366591388032581423ul;
        h ^= ::hash(type);
    }
}

void PtrType::init_env(TypeContext* typectx) {
    if (!isconcrete(target)) return;

    Env* tenv = type_env(target);
    const auto& tname = typectx->interner->str(tenv->name);
    i8* ptr_name = (i8*)typectx->envctx->envspace.alloc(tname.n + 3);
    mcpy(ptr_name, tname.ptr, tname.n);
    ptr_name[tname.n] = '_';
    ptr_name[tname.n + 1] = '_';
    ptr_name[tname.n + 2] = 'P';
    env = typectx->envctx->create(ENV_TYPE, tenv->parent, typectx->interner->intern({ ptr_name, tname.n + 3 }));
}

void ArrayType::init_env(TypeContext* typectx) {
    if (!isconcrete(element)) return;

    Env* tenv = type_env(element);
    const auto& tname = typectx->interner->str(tenv->name);
    i8* arr_name = (i8*)typectx->envctx->envspace.alloc(tname.n + 25); // 20 bytes should be enough for any 32-bit size
    i32 arr_name_size = tname.n + 3;
    mcpy(arr_name, tname.ptr, tname.n);

    arr_name[tname.n] = '_';
    arr_name[tname.n + 1] = '_';
    arr_name[tname.n + 2] = 'A';
    arr_name[tname.n + 3] = '_';
    arr_name[tname.n + 4] = '_';
    if (size < 0) {
        arr_name[arr_name_size ++] = '!';
    }
    else if (!size) {
        arr_name[arr_name_size ++] = '0';
    }
    else {
        u64 u = size;
        i32 c = 0, d = 0;
        u64 p = 1;
        while (p <= u) p *= 10, ++ c;
        d = c;
        while (c) {
            arr_name[arr_name_size ++] = "0123456789"[u % 10];
            u /= 10;
            c --;
        }
    }
    
    env = typectx->envctx->create(ENV_TYPE, tenv->parent, typectx->interner->intern({ arr_name, arr_name_size }));
    typectx->def<SliceType>(element); // Always define the associated slice.
}

void SliceType::init_env(TypeContext* typectx) {
    if (!isconcrete(element)) return;

    Env* tenv = type_env(element);
    const auto& tname = typectx->interner->str(tenv->name);
    i8* slice_name = (i8*)typectx->envctx->envspace.alloc(tname.n + 3);
    mcpy(slice_name, tname.ptr, tname.n);
    slice_name[tname.n] = '_';
    slice_name[tname.n + 1] = '_';
    slice_name[tname.n + 2] = 'S';
    env = typectx->envctx->create(ENV_TYPE, tenv->parent, typectx->interner->intern({ slice_name, tname.n + 3 }));
    slice<Type*> args = {(Type**)typectx->typespace.alloc(sizeof(Type*) * 1), 1};
    args[0] = this;
    typectx->envctx->add_method(typectx->interner->intern("iter"), this, env);
    typectx->envctx->add_method(typectx->interner->intern("empty"), this, env);
    typectx->envctx->add_method(typectx->interner->intern("read"), this, env);
    typectx->envctx->add_method(typectx->interner->intern("next"), this, env);
    env->def(typectx->interner->intern("iter"), e_fun(typectx->def<FunType>(args, this), nullptr));
    env->def(typectx->interner->intern("empty"), e_fun(typectx->def<FunType>(args, BOOL), nullptr));
    env->def(typectx->interner->intern("read"), e_fun(typectx->def<FunType>(args, this->element), nullptr));
    env->def(typectx->interner->intern("next"), e_fun(typectx->def<FunType>(args, this), nullptr));
}

void FunType::init_env(TypeContext* typectx) {
    if (!isconcrete(ret)) return;
    for (Type* t : arg) if (!isconcrete(t)) return;

    i32 name_size = typectx->interner->str(type_env(ret)->name).n + 5;
    for (Type* t : arg) {
        if (t != arg[0]) name_size += 2;
        name_size += typectx->interner->str(type_env(t)->name).n;
    }
    i8* fn_name = (i8*)typectx->envctx->envspace.alloc(name_size);
    i8* writer = fn_name;

    const auto& retname = typectx->interner->str(type_env(ret)->name);
    mcpy(writer, retname.ptr, retname.n), writer += retname.n;
    *writer ++ = '_';
    *writer ++ = '_';
    *writer ++ = 'F';
    *writer ++ = '_';
    *writer ++ = '_';
    if (nary) *writer ++ = '!';
    else for (Type* t : arg) {
        const auto& argname = typectx->interner->str(type_env(t)->name);
        if (t != arg[0]) {
            *writer ++ = '_';
            *writer ++ = '_';
        }
        mcpy(writer, argname.ptr, argname.n), writer += argname.n;
    }

    env = typectx->envctx->create(ENV_TYPE, type_env(arg[0])->parent, typectx->interner->intern({ fn_name, name_size }));
}

u64 hash(Type* type) {
    switch (type->kind) {
        case T_FUN: return ((FunType*)type)->hash;
        case T_TYPE: return 7157652590940299477ul;
        case T_ERROR: return 6483207495553446109ul;
        case T_VOID: return 5338990909533516041ul;
        case T_UNIT: return 9711907605905811091ul;    
        case T_CHAR: return 14175939696698843893ul;
        case T_STRING: return 15858331530218197541ul;
        case T_BOOL: return 7099446600187717403ul;
        case T_PTR: return 11401374774137664493ul ^ hash(((PtrType*)type)->target);
        case T_SLICE: return 6319538190306112643ul ^ hash(((SliceType*)type)->element);
        case T_ARRAY: return 13106823537169913923ul * hash(((ArrayType*)type)->element) ^ 4561913271040760149ul * ((ArrayType*)type)->size;
        case T_NUMERIC: return 6107764339085278949ul ^ 6417851031936212059ul * uptr(type); // We consider addresses unique, since numeric types are not user-defined.
        case T_NAMED: return ((NamedType*)type)->env->hash;
        case T_STRUCT: return ((StructType*)type)->env->hash;
        case T_UNION: return ((UnionType*)type)->env->hash;
        case T_VAR: return ((VarType*)type)->env->hash * 12050882363664274373ul ^ ((VarType*)type)->id * 9020503123748990177ul;
        case T_ANY_NUMERIC: return 7594156975545589679ul;
        case T_ANY: return 7751476419087332773ul;
        default:
            unreachable("Unhashable type!");
            return false;
    }
}

bool operator==(const Type& a, const Type& b) {
    if (a.kind != b.kind) return false;
    
    switch (a.kind) {
        case T_VOID:
        case T_UNIT:  
        case T_CHAR:
        case T_STRING:
        case T_BOOL:
        case T_TYPE:
        case T_ERROR:
        case T_ANY:
        case T_ANY_NUMERIC:
            return true; // Kind comparison only.
        case T_PTR: 
            return *((const PtrType&)a).target == *((const PtrType&)b).target;
        case T_SLICE: 
            return *((const SliceType&)a).element == *((const SliceType&)b).element;
        case T_ARRAY: 
            return ((const ArrayType&)a).size == ((const ArrayType&)b).size
                && *((const ArrayType&)a).element == *((const ArrayType&)b).element;
        case T_NUMERIC: 
            return &a == &b;
        case T_FUN:
            if (((const FunType&)a).hash != ((const FunType&)b).hash) return false;
            if (((const FunType&)a).arg.n != ((const FunType&)b).arg.n) return false;
            if (((const FunType&)a).nary != ((const FunType&)b).nary) return false;
            if (!(*((const FunType&)a).ret == *((const FunType&)b).ret)) return false;
            for (iptr i = 0; i < ((const FunType&)a).arg.n; i ++) {
                if (!(*((const FunType&)a).arg[i] == *((const FunType&)b).arg[i])) return false;
            }
            return true;
        case T_NAMED: 
            return ((const NamedType&)a).env->parent == ((const NamedType&)b).env->parent
                && ((const NamedType&)a).name == ((const NamedType&)b).name;
        case T_STRUCT: 
            return ((const StructType&)a).env->parent == ((const StructType&)b).env->parent
                && ((const StructType&)a).name == ((const StructType&)b).name;
        case T_UNION: 
            return ((const UnionType&)a).env->parent == ((const UnionType&)b).env->parent
                && ((const UnionType&)a).name == ((const UnionType&)b).name;
        case T_VAR: 
            return ((const VarType&)a).env == ((const VarType&)b).env
                && ((const VarType&)a).id == ((const VarType&)b).id;
        default:
            unreachable("Incomparable type!");
            return false;
    }
}

void format(stream& io, Module* mod, Type* type) {
    if (!type) return write(io, "<null type!>");
    bool first = true;
    switch (type->kind) {
        case T_VOID: write(io, "void"); break;
        case T_UNIT: write(io, "unit"); break;
        case T_CHAR: write(io, "char"); break;
        case T_STRING: write(io, "string"); break;
        case T_BOOL: write(io, "bool"); break;
        case T_TYPE: write(io, "type"); break;
        case T_ERROR: write(io, "error"); break;
        case T_PTR: write(io, '*'); format(io, mod, ((PtrType*)type)->target); break;
        case T_NUMERIC:
            write(io, ((NumericType*)type)->floating ? 'f' : 'i');
            if (((NumericType*)type)->literal) write(io, "const", ((NumericType*)type)->bytes * 8);
            else write(io, ((NumericType*)type)->bytes * 8);
            break;
        case T_ARRAY: format(io, mod, ((ArrayType*)type)->element); write(io, '[', ((ArrayType*)type)->size, ']'); break;
        case T_SLICE: format(io, mod, ((SliceType*)type)->element); write(io, "[]"); break;
        case T_FUN: 
            format(io, mod, ((FunType*)type)->ret); 
            if (((FunType*)type)->nary) {
                write(io, "(...)");
                break;
            }
            write(io, '(');
            for (auto arg : ((FunType*)type)->arg) {
                if (!first) write(io, ", ");
                first = false;
                format(io, mod, arg);
            } 
            write(io, ')'); break;
        case T_NAMED: write(io, mod->interner->str(((NamedType*)type)->name)); break;
        case T_STRUCT:
            write(io, mod->interner->str(((StructType*)type)->name));
            break;
        case T_UNION:
            write(io, mod->interner->str(((UnionType*)type)->name));
            break;
        case T_VAR:
            if (((VarType*)type)->nick != -1) write(io, mod->interner->str(((VarType*)type)->nick), '?');
            else write(io, ((VarType*)type)->id, '?');
            if (*((VarType*)type)->binding != VOID) {
                write(io, '(');
                format(io, mod, *((VarType*)type)->binding);
                write(io, ')');
            }
            break;
        case T_ANY: write(io, "any?"); break;
        case T_ANY_NUMERIC: write(io, "numeric?"); break;
    }
}

Type* attempt_union(Type* a, Type* b) {
    Type* ac = a, *bc = b;
    while (ac->kind == T_VAR) ac = *((VarType*)ac)->binding;
    while (bc->kind == T_VAR) bc = *((VarType*)bc)->binding;
    if (ac->is_case && bc->is_case && ac->env->parent == bc->env->parent && ac->env->parent->decl) {
        if (is_subtype(a, bc->env->decl->type)) return a;
    }
    return nullptr;
}

Type* unify(Type* a, Type* b) {
    if (!a || !b) return nullptr;
    if (a == b) return a;
    else if (is_subtype(b, a)) return a;
    else if (is_subtype(a, b)) return b;
    
    if (a->kind == T_VAR) {
        if (Type* t = attempt_union(a, b)) return t;
    }
    else if (b->kind == T_VAR) if (Type* t = attempt_union(b, a)) return t;

    return nullptr;
}

bool more_generic(Type* a, Type* b) {
    if (b->kind == T_ANY 
        || b->kind == T_ANY_NUMERIC
        || b->kind == T_VOID
        || (isconcrete(a) && a != VOID && b->kind == T_VAR && (!isconcrete(*((VarType*)b)->binding) || *((VarType*)b)->binding == VOID))) return false;
    return true;
}

bool occurs(Type* t, Type* var) {
    if (t == var) return true;
    else switch (t->kind) {
        case T_BOOL:
        case T_STRING:
        case T_CHAR:
        case T_UNIT:
        case T_TYPE:
        case T_ERROR:
        case T_VOID:
        case T_ANY:
        case T_ANY_NUMERIC:
        case T_NUMERIC: // These types have no members/parameters.
            return false;
        case T_PTR: return occurs(((PtrType*)t)->target, var);
        case T_ARRAY: return occurs(((ArrayType*)t)->element, var);
        case T_SLICE: return occurs(((SliceType*)t)->element, var);
        case T_FUN: {
            if (occurs(((FunType*)t)->ret, var)) return true;
            else for (Type* a : ((FunType*)t)->arg) if (occurs(a, var)) return true;
            return false;
        }
        case T_NAMED: 
        case T_STRUCT:
        case T_UNION:
            return false;
        case T_VAR: 
            if (((VarType*)t)->id == ((VarType*)var)->id && ((VarType*)t)->env == ((VarType*)var)->env) return true;
            return occurs(*((VarType*)t)->binding, var);
        default: return false;
    }
}

bool is_subtype_generic(Type* src, Type* dest) {
    if (!src || !dest) return false;
    if (dest == ANY) return true;
    if (src->kind == T_NUMERIC && dest == ANY_NUMERIC) return true;
    if (src == dest) return true;
    if (dest->kind == T_ERROR) return true; // Error types are infectious; everything else can convert to them.
    if (src->kind != T_VAR && dest->kind == T_VAR) return is_subtype(dest, src);
    Type* odest = dest; // Remember original dest so we can combine var constraints.
    while (dest->kind == T_VAR) dest = *((VarType*)dest)->binding; // Make dest concrete.
    switch (src->kind) {
        case T_BOOL: // These primitives cannot be converted implicitly to any other type.
        case T_STRING:
        case T_CHAR:
        case T_UNIT:
        case T_TYPE:
        case T_ERROR:
            return false;
        case T_ANY:
        case T_VOID: // Void has no values, so it can unify with any other type.
            return true; 
        case T_NUMERIC: // Numeric types have no child types, and only have nontrivial subtyping relations.
            if (dest->kind != T_NUMERIC) return false;
            return ((((NumericType*)src)->literal && ((NumericType*)src)->bytes <= ((NumericType*)dest)->bytes)
                || (((NumericType*)dest)->literal && ((NumericType*)src)->bytes >= ((NumericType*)dest)->bytes)) && ((NumericType*)src)->floating == ((NumericType*)dest)->floating;
        case T_PTR:
            if (dest->kind != T_PTR) return false;
            return is_subtype_generic(((PtrType*)src)->target, ((PtrType*)dest)->target);
        case T_ARRAY:
            if (dest->kind != T_ARRAY) return false;
            if (((ArrayType*)src)->size != ((ArrayType*)dest)->size && ((ArrayType*)src)->size >= 0) return false;
            return is_subtype_generic(((ArrayType*)src)->element, ((ArrayType*)dest)->element);
        case T_SLICE:
            if (dest->kind != T_SLICE) return false;
            return is_subtype_generic(((SliceType*)src)->element, ((SliceType*)dest)->element);
        case T_FUN: // Functions have no subtyping rules.
            if (dest->kind != T_FUN) return false;
            if (!is_subtype_generic(((FunType*)dest)->ret, ((FunType*)src)->ret)) return false;
            if (((FunType*)dest)->nary) return true;
            if (((FunType*)src)->arg.n != ((FunType*)dest)->arg.n) return false;
            for (i32 i = 0; i < ((FunType*)src)->arg.n; i ++) {
                if (!is_subtype_generic(((FunType*)src)->arg[i], ((FunType*)dest)->arg[i]))
                    return false;
            }
            return true;
        case T_NAMED: // Named types have no special subtyping rules.
        case T_STRUCT:
        case T_UNION:
            return false;
        case T_VAR:
        // $2($1(i8)) iconst16
        // $1(i8) iconst16
        // i8 iconst16
            if (is_subtype(*((VarType*)src)->binding, dest)) {
                if (more_generic(*((VarType*)src)->binding, odest) && !occurs(odest, src)) *((VarType*)src)->binding = odest;
                return true;
            }
            else if (((VarType*)src)->binding[0]->is_case && dest->is_case              // Cases of the same union can unify to the union.
                     && ((VarType*)src)->binding[0]->env->parent == dest->env->parent 
                     && dest->env->parent->decl && dest->env->parent->decl->type) {
                return is_subtype_generic(src, dest->env->parent->decl->type);
            }
            else if (odest->kind == T_VAR) return is_subtype(odest, src);
            return false;
        case T_ANY_NUMERIC:
            return dest->kind == T_ANY_NUMERIC || dest->kind == T_NUMERIC;
        default:
            return false;
    }
    return false;
}

Type* deepbinding(Type* t) {
    if (t->kind == T_VAR) return deepbinding(*((VarType*)t)->binding);
    return t;
}

bool is_subtype(Type* src, Type* dest) {
    if (!src || !dest) return false;
    if (is_subtype_generic(src, dest)) return true;
    while (dest->kind == T_VAR) dest = *((VarType*)dest)->binding; // Make dest concrete.
    if (dest->kind == T_UNION) { // All types can coerce to unions that contain them.
        Type* inner = src;
        while (src->kind == T_VAR) src = *((VarType*)src)->binding;
        if (src->is_case && src->env->parent == dest->env) return true;
    }
    switch (src->kind) {
        case T_BOOL: // These primitives cannot be converted implicitly to any other type.
        case T_STRING:
        case T_CHAR:
        case T_UNIT:
        case T_TYPE:
        case T_ERROR:
        case T_ANY_NUMERIC: // These only have generic subtyping rules.
            return false;
        case T_ANY:
        case T_VOID: // Void has no values, so it can unify with any other type.
            return true; 
        case T_NUMERIC:
            if (dest->kind != T_NUMERIC) return false;
            if (((NumericType*)src)->literal && !((NumericType*)dest)->literal && !((NumericType*)dest)->floating) return false; // No non-generic subtyping for constants.
            if (((NumericType*)src)->floating)
                return ((NumericType*)dest)->floating 
                    && (((NumericType*)src)->bytes <= ((NumericType*)dest)->bytes);
            else return ((NumericType*)src)->bytes <= ((NumericType*)dest)->bytes || ((NumericType*)dest)->floating;
        case T_PTR:
            if (dest->kind != T_PTR) return false;
            if (deepbinding(((PtrType*)dest)->target)->kind != T_UNION) return false; // Allow only union member coercions.
            return is_subtype(((PtrType*)src)->target, ((PtrType*)dest)->target);
        case T_SLICE: // Slices and functions have no non-generic coercions.
        case T_FUN:
            return false;
        case T_ARRAY:
            return dest->kind == T_SLICE && is_subtype_generic(((ArrayType*)src)->element, ((SliceType*)dest)->element);
        case T_NAMED: // Named types have no special subtyping rules.
        case T_STRUCT:
        case T_UNION:
            return false;
        case T_VAR: // Type variables' coercion rules are handled in generic subtyping.
            return false;
    }
    return false;
}

Type *VOID, *UNIT, *I8, *I16, *I32, *I64, *INT, *IPTR, *ICONST8, *ICONST16, *ICONST32, *ICONST64, *F32, *F64, *FLOAT, *BOOL, *CHAR, *STRING, *TYPE, *ERROR, *ANY, *ANY_NUMERIC, *ANY_ARRAY, *ANY_PTR, *ANY_SLICE, *ANY_FUNCTION;