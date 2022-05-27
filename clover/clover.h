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

struct Module {
    // Local to this module
    const i8* path;
    ByteSource bytes;
    Lexer* lexer;
    Parser* parser;

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
    bool visited;

    Symbol basename;
};

struct Clover {
    map<Symbol, Module*, 256, arena> modules;
    arena module_space;
    Module* main;
};

Module* compile_module(Clover* clover, Interner* interner, TypeContext* typectx, EnvContext* envctx, const_slice<i8> path);

i32 clover_main(i32 argc, i8** argv);

#endif