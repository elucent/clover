#ifndef BASIL_CLOVER_AST_H
#define BASIL_CLOVER_AST_H

#include "clover/lex.h"
#include "lib/tuple.h"
#include "clover/type.h"

struct Env;

enum ASTKind : u8 {
    AST_NONE, 
    // terminals
    AST_FIRST_EXPR, AST_ICONST = AST_FIRST_EXPR, AST_FCONST, AST_BOOL, AST_STRCONST, AST_CHCONST, AST_UNIT, AST_ERROR,
    AST_VAR, 
    // exprs
    AST_ADD, AST_SUB, AST_STAR, AST_DIV, AST_MOD, AST_EXP, AST_NEG, AST_PLUS,
    AST_BITAND, AST_BITOR, AST_BITXOR, AST_BITNOT, AST_BITLEFT, AST_BITRIGHT,
    AST_AND, AST_OR, AST_XOR, AST_NOT,
    AST_EQUAL, AST_INEQUAL, AST_LESS, AST_LEQUAL, AST_GREATER, AST_GEQUAL, AST_IS, 
    AST_DEREF, AST_ADDROF, AST_DOT, AST_APPLY, AST_INDEX, AST_SLICE, AST_CTOR, AST_GENCTOR,
    AST_NEW, AST_NEWARRAY, AST_DEL, AST_POSTINCR, AST_POSTDECR, AST_INCR, AST_DECR,
    AST_ARRAY, AST_SET,
    AST_ASSIGN,
    AST_ADDEQ, AST_SUBEQ, AST_STAREQ, AST_DIVEQ, AST_MODEQ, AST_EXPEQ,
    AST_BITANDEQ, AST_BITOREQ, AST_BITXOREQ, AST_BITLEFTEQ, AST_BITRIGHTEQ,
    AST_PAREN,
    AST_CONV, AST_SIZEOF,
    // statements
    AST_LAST_EXPR, AST_IF = AST_LAST_EXPR, AST_WHILE, AST_UNTIL, AST_FOR, AST_WITH, AST_USE, AST_MATCH,
    AST_RETURN, AST_DEFER, AST_DO, AST_BREAK, AST_CONTINUE,
    // types
    AST_FIRST_TYPE, AST_TYPENAME = AST_FIRST_TYPE, AST_PTRTYPE, AST_ARRAYTYPE, AST_SLICETYPE, AST_FUNTYPE, AST_TYPEDOT, AST_TYPEINST, AST_TYPECONST, AST_TYPEVAR, AST_LAST_TYPE,
    // decls
    AST_FIRST_DECL = AST_LAST_TYPE, AST_VARDECL = AST_FIRST_DECL, AST_TYPEDECL, AST_FUNDECL, AST_MODULEDECL, AST_ALIASDECL, AST_CASEDECL, AST_CONSTDECL, AST_PTRDECL, // ptrdecl is needed for the case 'T* ptr', which is ambiguous with multiplication until typechecking
    // misc
    AST_LAST_DECL, AST_MODULENAME = AST_LAST_DECL, AST_MODULEDOT, AST_DOTINSERT,
    // structure
    AST_PROGRAM, AST_ARGS,
};

struct AST {
    ASTKind kind;
    SourcePos pos;
    Type* type;   // General-purpose type information. For expressions, this is the type of the resulting value. For
                  // type nodes, this is the type represented by that expression. For declarations, this is the type 
                  // of the declared entity.

    inline AST(ASTKind kind_in, SourcePos pos_in): kind(kind_in), pos(pos_in), type(nullptr) {}

    virtual AST* clone(arena& alloc) = 0;
};

struct Fold {
    enum Kind : i8 {
        NONE, ICONST, FCONST, BOOL, CHCONST
    };

    union {
        i64 iconst;
        double fconst;
        rune chconst;
        bool bconst;
    };

    Kind kind;

    inline Fold(): kind(NONE) {}
    inline static Fold from_int(i64 iconst_in) {
        Fold f;
        f.kind = ICONST;
        f.iconst = iconst_in;
        return f;
    }
    
    inline static Fold from_float(double fconst_in) {
        Fold f;
        f.kind = FCONST;
        f.fconst = fconst_in;
        return f;
    }
    
    inline static Fold from_char(rune chconst_in) {
        Fold f;
        f.kind = CHCONST;
        f.chconst = chconst_in;
        return f;
    }
    
    inline static Fold from_bool(bool bconst_in) {
        Fold f;
        f.kind = BOOL;
        f.bconst = bconst_in;
        return f;
    }
    
    inline operator bool() const {
        return kind != NONE;
    }

    inline Type* type() const {
        switch (kind) {
            case NONE: return ERROR;
            case ICONST: return ::ICONST;
            case FCONST: return ::FCONST;
            case BOOL: return ::BOOL;
            case CHCONST: return ::CHAR;
        }
    }
};

inline AST* try_clone(AST* ast, arena& alloc) {
    if (ast) return ast->clone(alloc);
    else return nullptr;
}

struct Statement : public AST {
    inline Statement(ASTKind kind, SourcePos pos): AST(kind, pos) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Statement(kind, pos);
    }
};

struct Expr : public Statement {
    inline Expr(ASTKind kind, SourcePos pos): Statement(kind, pos) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Expr(kind, pos);
    }
};

struct Decl : public Statement {
    inline Decl(ASTKind kind, SourcePos pos): Statement(kind, pos) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Decl(kind, pos);
    }
};

struct VarDecl : public Decl {
    AST* ann; // Null for type inference.
    AST* name;
    AST* init;
    Symbol basename;
    inline VarDecl(SourcePos pos, AST* ann_in, AST* name_in, AST* init_in): Decl(AST_VARDECL, pos), ann(ann_in), name(name_in), init(init_in) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) VarDecl(pos, try_clone(ann, alloc), try_clone(name, alloc), try_clone(init, alloc));
    }
};

struct ConstDecl : public Decl {
    AST* name;
    AST* init;
    Symbol basename;
    Fold fold;
    inline ConstDecl(SourcePos pos, AST* name_in, AST* init_in): Decl(AST_CONSTDECL, pos), name(name_in), init(init_in) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) ConstDecl(pos, try_clone(name, alloc), try_clone(init, alloc));
    }
};

struct FunDecl : public Decl {
    AST* returned;
    AST* proto;
    AST* body;
    Env* env;
    Type* method_type;
    Symbol basename;
    bool emitted;
    map<TypeTuple, FunDecl*, 8, arena> insts;
    bool generic = false, isinst = false;
    inline FunDecl(SourcePos pos, AST* returned_in, AST* proto_in): FunDecl(pos, returned_in, proto_in, nullptr) {}
    inline FunDecl(SourcePos pos, AST* returned_in, AST* proto_in, AST* body_in): 
        Decl(AST_FUNDECL, pos), returned(returned_in), proto(proto_in), body(body_in), env(nullptr), method_type(nullptr), emitted(false) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) FunDecl(pos, try_clone(returned, alloc), try_clone(proto, alloc), try_clone(body, alloc));
    }
};

struct ModuleDecl : public Decl {
    AST* name;
    AST* body;
    Env* env;
    Symbol basename;
    inline ModuleDecl(SourcePos pos, AST* name_in, AST* body_in): Decl(AST_MODULEDECL, pos), name(name_in), body(body_in), env(nullptr) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) ModuleDecl(pos, try_clone(name, alloc), try_clone(body, alloc));
    }
};

struct CaseDecl : public Decl {
    AST* pattern;
    AST* body;
    Env* env;
    Symbol basename;
    bool reachable;
    inline CaseDecl(SourcePos pos, AST* pattern_in, AST* body_in): Decl(AST_CASEDECL, pos), pattern(pattern_in), body(body_in), env(nullptr), reachable(true) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) CaseDecl(pos, try_clone(pattern, alloc), try_clone(body, alloc));
    }
};

struct TypeDecl : public Decl {
    AST* name;
    AST* body;
    Env* env;
    Symbol basename;
    map<TypeTuple, TypeDecl*, 8, arena> insts;
    TypeDecl* prototype;
    bool generic = false, is_prototype = false;
    inline TypeDecl(SourcePos pos, AST* name_in, AST* body_in): Decl(AST_TYPEDECL, pos), name(name_in), body(body_in), prototype(nullptr) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) TypeDecl(pos, try_clone(name, alloc), try_clone(body, alloc));
    }
};

struct AliasDecl : public Decl {
    AST* name;
    AST* body;
    Symbol basename;
    inline AliasDecl(SourcePos pos, AST* name_in, AST* body_in): Decl(AST_ALIASDECL, pos), name(name_in), body(body_in) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) TypeDecl(pos, try_clone(name, alloc), try_clone(body, alloc));
    }
};

struct Unary : public Expr {
    AST* child;
    inline Unary(ASTKind kind, SourcePos pos, AST* child_in): Expr(kind, pos), child(child_in) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Unary(kind, pos, try_clone(child, alloc));
    }
};

struct Sizeof : public Unary {
    Fold fold;
    inline Sizeof(SourcePos pos, AST* child): Unary(AST_SIZEOF, pos, child) {}

    inline AST* clone(arena& alloc) override {
        Sizeof* s = new(alloc) Sizeof(pos, try_clone(child, alloc));
        s->fold = fold;
        return s;
    }
};

struct Binary : public Expr {
    AST* left;
    AST* right;
    inline Binary(ASTKind kind, SourcePos pos, AST* left_in, AST* right_in): Expr(kind, pos), left(left_in), right(right_in) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Binary(kind, pos, try_clone(left, alloc), try_clone(right, alloc));
    }
};

struct Is : public Binary {
    Env* env = nullptr;
    bool external_env = false, reachable = true;
    inline Is(SourcePos pos, AST* left, AST* right): Binary(AST_IS, pos, left, right) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Is(pos, try_clone(left, alloc), try_clone(right, alloc));
    }
};

struct Slice : public Expr {
    AST* array;
    AST* low;
    AST* high;
    inline Slice(SourcePos pos, AST* array_in, AST* low_in, AST* high_in): Expr(AST_SLICE, pos), array(array_in), low(low_in), high(high_in) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Slice(pos, try_clone(array, alloc), try_clone(low, alloc), try_clone(high, alloc));
    }
};

struct IllFormed : public Expr {
    inline IllFormed(): Expr(AST_ERROR, {}) { type = ERROR; }

    inline AST* clone(arena& alloc) override {
        return new(alloc) IllFormed;
    }
};

struct Const : public Expr {
    inline Const(SourcePos pos, i64 i): Expr(AST_ICONST, pos), iconst(i) {}
    inline Const(SourcePos pos, double f): Expr(AST_FCONST, pos), fconst(f) {}
    inline Const(SourcePos pos, bool b): Expr(AST_BOOL, pos), bconst(b) {}
    inline Const(SourcePos pos, rune ch): Expr(AST_CHCONST, pos), chconst(ch) {}
    inline Const(SourcePos pos, i32 byteidx, i32 len): Expr(AST_STRCONST, pos), strconst{byteidx, len} {}
    inline Const(SourcePos pos): Expr(AST_UNIT, pos) {}

    union {
        i64 iconst;
        double fconst;
        bool bconst;
        rune chconst;
        struct { i32 byteidx, len; } strconst;
    };

    static_assert(sizeof(strconst) == sizeof(iconst), "Constant fields in AST node aren't sized the same.");

    inline AST* clone(arena& alloc) override {
        Const* c = new(alloc) Const(pos, iconst);
        c->kind = kind;
        return c;
    }
};

struct Apply : public Expr {
    AST* fn;
    slice<AST*> args;
    inline Apply(SourcePos pos, AST* fn_in, slice<AST*> args_in): Expr(AST_APPLY, pos), fn(fn_in), args(args_in) {}

    inline AST* clone(arena& alloc) override {
        AST** argspace = new(alloc) AST*[args.n];
        for (int i = 0; i < args.n; i ++) argspace[i] = try_clone(args[i], alloc);
        return new(alloc) Apply(pos, try_clone(fn, alloc), slice<AST*>(argspace, args.n));
    }
};

struct List : public Expr {
    slice<AST*> items;
    i32 cap;
    inline List(ASTKind kind, SourcePos pos, slice<AST*> items_in): Expr(kind, pos), items(items_in), cap(items_in.n) {}

    inline void push(arena& space, AST* ast) {
        if (items.n + 1 >= cap) {
            auto old = items;
            cap *= 2;
            items = { new(space) AST*[cap], old.n };
            for (u32 i = 0; i < old.n; i ++) items[i] = old[i];
        }
        items[items.n ++] = ast;
    }

    inline AST* clone(arena& alloc) override {
        AST** itemspace = new(alloc) AST*[items.n];
        for (int i = 0; i < items.n; i ++) itemspace[i] = try_clone(items[i], alloc);
        return new(alloc) List(kind, pos, slice<AST*>(itemspace, items.n));
    }
};

struct Var : public Expr {
    Symbol name;
    inline Var(SourcePos pos, Symbol name_in): Expr(AST_VAR, pos), name(name_in) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Var(pos, name);
    }
};

struct Loop : public Statement {
    AST* cond;
    AST* body;
    Env* env;
    inline Loop(ASTKind kind, SourcePos pos, AST* cond_in, AST* body_in): Statement(kind, pos), cond(cond_in), body(body_in), env(nullptr) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) Loop(kind, pos, try_clone(cond, alloc), try_clone(body, alloc));
    }
};

struct If : public Expr {
    AST* cond;
    AST* ifTrue;
    AST* ifFalse; // Null if else omitted.
    Env* env;
    inline If(SourcePos pos, AST* cond_in, AST* ifTrue_in, AST* ifFalse_in): Expr(AST_IF, pos), cond(cond_in), ifTrue(ifTrue_in), ifFalse(ifFalse_in) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) If(pos, try_clone(cond, alloc), try_clone(ifTrue, alloc), try_clone(ifFalse, alloc));
    }
};

struct For : public Statement {
    AST* binding;
    AST* items;
    AST* body;
    Env* env;
    bool transformed;
    inline For(SourcePos pos, AST* binding_in, AST* items_in, AST* body_in): Statement(AST_FOR, pos), binding(binding_in), items(items_in), body(body_in), transformed(false) {}

    inline AST* clone(arena& alloc) override {
        For* f = new(alloc) For(pos, try_clone(binding, alloc), try_clone(items, alloc), try_clone(body, alloc));
        f->transformed = transformed;
        return f;
    }
};

struct With : public Statement {
    AST* bound;
    AST* body;
    Env* env;
    inline With(SourcePos pos, AST* bound_in, AST* body_in): Statement(AST_WITH, pos), bound(bound_in), body(body_in), env(nullptr) {}

    inline AST* clone(arena& alloc) override {
        return new(alloc) With(pos, try_clone(bound, alloc), try_clone(body, alloc));
    }
};

struct ASTProgram : public AST {
    vec<Statement*, 128, arena> toplevel;
    vec<AST*, 128, arena> defers;
    Env* env;
    Env* deferred;
    inline ASTProgram(): AST(AST_PROGRAM, {}), env(nullptr), deferred(nullptr) {}
    
    inline AST* clone(arena& alloc) override {
        ASTProgram* new_prog = new(alloc) ASTProgram;
        new_prog->toplevel.alloc = new_prog->defers.alloc = &alloc;
        return new_prog;
    }
};

using Production = void(*)(Lexer&, Parser&, Module*, Token&);

struct Parser {
    arena astspace;
    vec<AST*, 128, arena> nodes;
    vec<Production, 128, arena> prods;
    vec<i32, 128, arena> counts;
    vec<i32, 128, arena> indents;
    vec<SourcePos, 128, arena> locs;
    vec<TokenKind, 128, arena> binops;
    ASTProgram* program;
    i32 nesting = 0, indent = 0;
    bool check_indent = false;

    Parser();

    inline Parser& consume_and_indent(Lexer& lexer) {
        if (lexer.tokens.peek().kind != TK_EOF) 
            if (lexer.tokens.read().kind == TK_NEWLINE) {
                check_indent = true; // Remember to check indent again once we receive more tokens.
            }
        return *this;
    }

    inline Parser& consume(Lexer& lexer) {
        if (lexer.tokens.peek().kind != TK_EOF) 
            lexer.tokens.read();
        return *this;
    }

    inline Parser& rec(Production prod) {
        prods.push(prod);
        return *this;
    }

    inline Parser& tailrec(Production prod) {
        prods.back() = prod;
        return *this;
    }

    inline Parser& leave() {
        prods.pop();
        return *this;
    }

    inline Parser& push(AST* ast) {
        nodes.push(ast);
        return *this;
    }

    inline Parser& rec(Production prod, AST* ast) {
        prods.push(prod);
        nodes.push(ast);
        return *this;
    }

    inline Parser& tailrec(Production prod, AST* ast) {
        prods.back() = prod;
        nodes.push(ast);
        return *this;
    }

    inline Parser& leave(AST* ast) {
        prods.pop();
        nodes.push(ast);
        return *this;
    }

    inline AST* back() {
        return nodes.back();
    }

    inline AST* pop() {
        return nodes.pop();
    }

    inline Parser& pop(i32 n) {
        while (n --) nodes.pop();
        return *this;
    }

    inline slice<AST*> take(i32 n) {
        AST** start = (AST**)astspace.alloc(n * sizeof(AST*));
        mcpy(start, nodes.end() - n, n * sizeof(AST*));
        nodes._size -= n;
        return {start, n};
    }

    inline Parser& begincount(i32 n = 1) {
        counts.push(n);
        return *this;
    }

    inline i32 count() {
        return counts.back();
    }

    inline Parser& inc() {
        counts.back() ++;
        return *this;
    }

    inline i32 endcount() {
        return counts.pop();
    }

    inline AST* lhs() {
        return nodes[nodes.size() - 2];
    }

    inline AST* rhs() {
        return nodes.back();
    }

    inline Parser& pushop(TokenKind op) {
        binops.push(op);
        return *this;
    }

    inline TokenKind popop() {
        return binops.pop();
    }

    inline Parser& poplr() {
        nodes[nodes.size() - 3] = nodes.back();
        nodes._size -= 2;
        return *this;
    }

    inline Parser& nest() {
        nesting ++;
        return *this;
    }

    inline Parser& unnest() {
        nesting --;
        return *this;
    }

    inline Parser& startpos(SourcePos pos) {
        locs.push(pos);
        return *this;
    }

    inline SourcePos endpos(SourcePos pos) {
        return locs.pop() + pos;
    }
};

/*
 * advance_parser(mod)
 *
 * Moves the parser forward, consuming all available tokens from the module, producing AST nodes.
 */
void advance_parser(Module* mod);

/*
 * format(io, mod, ast)
 *
 * Writes a textual representation of the provided AST to the provided IO stream.
 */
void format(stream& io, Module* mod, const AST* const& ast);

/*
 * compute_envs(mod, env, ast)
 *
 * Traverses the provided AST, creating local environments and defining symbols in the environment
 * tree.
 */
void compute_envs(Module* mod, Env* env, AST* ast);

/*
 * detect_types(mod, env, ast)
 *
 * Detects AST nodes that represent types instead of values and updates their node kinds
 * accordingly. Checks that positions where typenames are required actually contain types, and
 * errors otherwise.
 */
void detect_types(Module* mod, Env* env, AST* ast);

/*
 * infer(mod, env, ast)
 *
 * Traverses the provided AST and computes a type for every node. If a type cannot be computed for
 * a node without violating other expressions, an error is reported.
 */
void infer(Module* mod, Env* env, AST* ast);

/*
 * typecheck(mod, env, ast)
 *
 * Verifies that all inferred types across the AST are used correctly. If incorrect usage is encountered,
 * an error is reported. Otherwise, types are replaced with their concrete variants. Generic type and
 * function instantiation is done at this phase.
 */
void typecheck(Module* mod, Env* env, AST* ast);

/*
 * tryfold(mod, env, ast)
 *
 * Tries to constant-fold the provided AST. Returns an empty fold if the node cannot be constant-folded,
 * or a fold of a primitive value if a fold was possible.
 */
Fold tryfold(Module* mod, Env* env, AST* ast);

/*
 * CContext
 *
 * Contains contextual information for writing C source.
 */
struct CContext {
    stream& h, &c;      // Streams for the associated header and C source.
    i32 anon = 0;       // Index for anonymous variables.
    i32 h_indent = 0;   // Indentation levels for new lines.
    i32 c_indent = 0;
    arena textspace;    // Arena for storing string information produced in the C output process.
    Module* main;       // Pointer to the module containing the entry point, can be null if no entry point is needed.
    vec<pair<AST*, AST*>, 8, arena> patn_defs; // List of patterns to revisit in the body of a conditional expression.

    inline CContext(stream& h_in, stream& c_in, Module* main_in): h(h_in), c(c_in), main(main_in) { patn_defs.alloc = &main->parser->astspace; }
};

/*
 * emit_c_prelude(mod, env, cctx)
 *
 * Prints shared C header all source files share, and initializes some printing information.
 */
void emit_c_prelude(Module* mod, Env* env, CContext& ctx, const i8* cpath, const i8* hpath);

/*
 * emit_c_types(mod, env, cctx)
 *
 * Prints C type declarations for all Clover types to the output stream contained within the
 * provided C context.
 */
void emit_c_types(Module* mod, Env* env, CContext& ctx);

/*
 * emit_c(mod, env, ast, cctx)
 *
 * Compiles the provided typechecked AST to C source code, updating the provided C context and
 * printing the source to the stream contained within.
 */
void emit_c(Module* mod, Env* env, AST* ast, CContext& ctx);

#endif