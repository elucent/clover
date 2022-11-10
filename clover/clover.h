#ifndef BASIL_CLOVER_CLOVER_H
#define BASIL_CLOVER_CLOVER_H

#include "lib/malloc.h"
#include "lib/vec.h"
#include "lib/hash.h"

typedef i32 Symbol;

/*
 * ByteSource
 *
 * Essentially just a slice of chars, representing the raw text of a source file loaded
 * into memory.
 */
struct ByteSource {
    iptr length;
    i8* text;
};

struct Lexer;
struct Interner;
struct Parser;
struct TypeContext;
struct EnvContext;
struct Clover;
struct Type;

enum TopoMark {
    MARK_NONE, MARK_TEMP, MARK_PERM
};

struct AST;
struct Env;

struct Module {
    // Local to this module
    const i8* path;
    ByteSource bytes;
    Lexer* lexer;
    Parser* parser;

    // Defer metadata.
    vec<AST*, 8, arena> defers; // Expressions being deferred in the current block.
    vec<i32, 8, arena> ndefers; // Number of deferred expressions in the enclosing block.
    // Together, defers and ndefers form a sort of stack of stacks. defers[ndefers.back():] represents the defers
    // to be processed for the current block.

    // Definitions this module is responsible for generating.
    vec<pair<AST*, Env*>, 8, arena> toplevels; // Stores top-level definitions to be included in this module's output.
    vec<pair<AST*, Env*>, 8, arena> automethods; // Stores automatically-generated methods defined in this module.

    void add_top_decl(AST* ast, Env* env);
    void add_top_typedecl(AST* ast, Env* env);

    // Shared between modules
    Clover* cloverinst;
    Interner* interner;
    TypeContext* typectx;
    EnvContext* envctx;

    // C output paths
    i8* hpath;
    i8* cpath;
    set<Type*, 8, arena> cnew_types; // Types constructed using new.

    // Dependencies
    vec<Module*, 8, arena> deps;
    bool visited, dep_errored = false;

    // Name of this module.
    Symbol basename;

    // Mark status used for topological sort.
    TopoMark mark = MARK_NONE;
};

struct Clover {
    map<Symbol, Module*, 256, arena> modules;
    map<Type*, pair<Env*, Module*>, 256, arena> typedefs; // Tracks which modules define certain types.
    arena module_space;
    Module* main;
};

Module* compile_module(Clover* clover, Interner* interner, TypeContext* typectx, EnvContext* envctx, const_slice<i8> path);

i32 clover_main(i32 argc, i8** argv);

#endif