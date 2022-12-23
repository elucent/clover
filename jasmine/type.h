#ifndef BASIL_JASMINE_TYPE_H
#define BASIL_JASMINE_TYPE_H

#include "lib/vec.h"
#include "lib/hash.h"
#include "lib/buffer.h"
#include "jasmine/tab.h"

MODULE(jasmine)

struct JasmineModule;

constexpr typeidx
    T_I8 = -1,
    T_I16 = -2,
    T_I32 = -3,
    T_I64 = -4,
    T_IWORD = -5,
    T_PTR = -6,
    T_F32 = -7,
    T_F64 = -8,
    T_U8 = -9,
    T_U16 = -10,
    T_U32 = -11,
    T_U64 = -12,
    T_UWORD = -13,
    T_REF = -14,
    T_VOID = -15;

enum TypeKind : u8 {
    TK_ARR = 0, TK_TUP = 1, TK_FUN = 2, TK_VEC = 3
};

enum TypeFlags : u8 {
    NO_TYPE_FLAGS = 0, TF_HAS_FLOAT = 1, TF_HAS_INT = 2
};

inline TypeFlags& operator|=(TypeFlags& a, TypeFlags b) {
    a = TypeFlags(a | b);
    return a;
}

using TypeVec = vec<typeidx, 4, arena>;

struct Type {
    union {
        struct { typeidx elt; u64 nelts; };
        struct { u64 len; };
        struct { typeidx ret; u64 nargs; };
        struct { typeidx velt; u64 nvelts; };
    };

    TypeVec members;
    u64 h;
    TypeKind kind;
    TypeFlags flags = NO_TYPE_FLAGS;
    u16 align;
    u32 size = 0;

    bool operator==(const Type& other) const;
};

using TypeTableVec = vec<Type, 16>;
using TypeTableMap = map<Type, typeidx, 16>;

struct Target;

struct TypeTable {
    JasmineModule* obj;
    TypeTableVec types;
    TypeTableMap table;

    TypeTable(JasmineModule* obj_in);

    u32 size() const;
    void write(bytebuf<>& buf) const;
    void read(bytebuf<>& buf);
    void format(fd io) const;

    inline u32 conservative_sizeof(typeidx i) {
        if (i < 0) switch (i8(i)) {
            case T_VOID: 
                return 0;
            case T_U8:
            case T_I8: 
                return 1;
            case T_U16:
            case T_I16: 
                return 2;
            case T_U32:
            case T_F32:
            case T_I32: 
                return 4;
            case T_U64:
            case T_F64:
            case T_I64:
            case T_PTR:
            case T_REF:
            case T_IWORD:
            case T_UWORD:
                return 8;
            default:
                fatal("Can't take size of type.");
                return 0;
        }
        compute_conservative_sizeof(i);
        return types[i].size;
    }

    inline u32 conservative_alignof(typeidx i) {
        if (i < 0) switch (i8(i)) {
            case T_VOID: 
                return 0;
            case T_U8:
            case T_I8: 
                return 1;
            case T_U16:
            case T_I16: 
                return 2;
            case T_U32:
            case T_F32:
            case T_I32: 
                return 4;
            case T_U64:
            case T_F64:
            case T_I64:
            case T_PTR:
            case T_REF:
            case T_IWORD:
            case T_UWORD:
                return 8;
            default:
                fatal("Can't take align of type.");
                return 0;
        }
        compute_conservative_sizeof(i);
        return types[i].align;
    }

    inline void compute_conservative_sizeof(typeidx i) {
        Type& t = types[i];
        if (!t.size) switch (t.kind) {
            case TK_ARR: 
                t.size = t.nelts * conservative_sizeof(t.elt);
                t.align = conservative_alignof(t.elt);
                break;
            case TK_FUN:
                t.size = 0;
                t.align = 0;
                break;
            case TK_TUP: {
                u32 size = 0, align = 0;
                for (u32 i = 0; i < t.len; i ++) {
                    u32 a = conservative_alignof(t.members[i]);
                    while (size & a - 1) size ++;
                    align = a > align ? a : align;
                    size += conservative_sizeof(t.members[i]);
                }
                t.size = size;
                t.align = align;
                break;
            }
            case TK_VEC:
                t.size = t.nvelts * conservative_sizeof(t.velt); 
                t.align = conservative_alignof(t.nvelts * t.velt);
                break;
        }
    }

    template<typename Target>
    u32 native_sizeof(typeidx i) const {
        if (i < 0) return Target::primsize(i);
        else return types[i].size;
    }

    template<typename Target>
    u32 native_alignof(typeidx i) const {
        if (i < 0) return Target::primsize(i);
        else return types[i].align;
    }
    
    template<typename Target>
    void compute_native_sizes() {
        for (u32 i = 0; i < types.size(); i ++) {
            Type& t = types[i];
            switch (t.kind) {
                case TK_ARR: 
                    t.size = t.nelts * native_sizeof<Target>(t.elt);
                    t.align = native_alignof<Target>(t.elt);
                    break;
                case TK_FUN:
                    t.size = 0;
                    t.align = 0;
                    break;
                case TK_TUP: {
                    u32 size = 0, align = 0;
                    for (u32 i = 0; i < t.len; i ++) {
                        u32 a = native_alignof<Target>(t.members[i]);
                        while (size & a - 1) size ++;
                        align = a > align ? a : align;
                        size += native_sizeof<Target>(t.members[i]);
                    }
                    t.size = size;
                    t.align = align;
                    break;
                }
                case TK_VEC:
                    t.size = t.nvelts * native_sizeof<Target>(t.velt); 
                    t.align = native_alignof<Target>(t.nvelts * t.velt);
                    break;
            }
        }
    }
};

void format_type(const TypeTable& table, typeidx id, fd io);

inline u64 hash(const Type& t) {
    return t.h;
}

inline u64 try_insert(TypeTable& tab, const Type& t) {
    auto it = tab.table.find(t);
    if (it == tab.table.end()) {
        u64 i = tab.types.size();
        tab.types.push(t);
        tab.table.put(t, i);
        return i;
    }
    else return it->value;
}

inline TypeFlags tflags(TypeTable& tab, typeidx elt) {
    if (elt < 0) 
        if (elt == T_F32 || elt == T_F64) return TF_HAS_FLOAT;
        else return TF_HAS_INT;
    else return tab.types[elt].flags;
}

template<typename... Args>
TypeVec tvec(const Args&... args) {
    TypeVec v;
    vec_fill<typeidx>(v, args...);
    return v;
}

inline typeidx t_array(TypeTable& tab, typeidx elt, u32 len) {
    Type t;
    t.kind = TK_ARR;
    t.elt = elt;
    t.nelts = len;
    t.flags = tflags(tab, elt);   
    t.h = 6797967129370127093ul
        ^ t.elt * 5905731549298544443ul
        ^ t.nelts * 4976826848160358721ul;
    return try_insert(tab, t);
}

inline typeidx t_tuple(TypeTable& tab, const TypeVec& elts) {
    Type t;
    t.kind = TK_TUP;
    t.len = elts.size();
    t.members = elts;
    t.h = 5149859601856040891ul 
        ^ t.len * 9423393794836913413ul;
    for (typeidx u : elts) {
        t.h *= 4774041982370940107ul, t.h ^= 7518032075298356371ul * u;
        t.flags |= tflags(tab, u);
    }
    return try_insert(tab, t);
}

template<typename... Args>
inline typeidx t_tuple(TypeTable& tab, const Args&... args) {
    return t_tuple(tab, tvec(args...));
}

inline typeidx t_fun(TypeTable& tab, typeidx ret, const TypeVec& args) {
    Type t;
    t.kind = TK_FUN;
    t.ret = ret;
    t.nargs = args.size();
    t.members = args;
    t.h = 10992678914156401739ul 
        ^ t.ret * 6239922470233447ul
        ^ t.nargs * 9861267878043082691ul;
    for (typeidx u : args) t.h *= 1404551109701180041ul, t.h ^= 10559527727229159863ul * u;
    return try_insert(tab, t);
}

template<typename... Args>
inline typeidx t_fun(TypeTable& tab, typeidx ret, const Args&... args) {
    return t_fun(tab, ret, tvec(args...));
}

inline typeidx t_vec(TypeTable& tab, typeidx elt, u32 len) {
    Type t;
    t.kind = TK_VEC;
    t.velt = elt;
    t.flags = tflags(tab, elt);
    t.nvelts = len;
    t.h = 3377704524053732311ul
        ^ t.velt * 747909389925611777ul
        ^ t.nvelts * 7299539809287019081ul;
    return try_insert(tab, t);
}

ENDMODULE()

#endif