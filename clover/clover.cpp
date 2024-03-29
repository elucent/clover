#include "clover/clover.h"
#include "clover/load.h"
#include "clover/lex.h"
#include "clover/ast.h"
#include "clover/env.h"
#include "clover/err.h"
#include "clover/type.h"
#include "core/sys.h"
#include "core/util.h"
#include "lib/io.h"
#include "jasmine/obj.h"
#include "jasmine/pass.h"

#define CLOVER_VERSION_MAJOR 0
#define CLOVER_VERSION_MINOR 1
#define CLOVER_VERSION_PATCH 0

#ifdef LIBCORE_LINUX
    #define CLOVER_VERSION_OS "linux"
#elif defined(LIBCORE_OSX)
    #define CLOVER_VERSION_OS "darwin"
#elif defined(LIBCORE_WINDOWS)
    #define CLOVER_VERSION_OS "windows"
#elif defined(LIBCORE_WASI)
    #define CLOVER_VERSION_OS ""
#endif

#ifdef LIBCORE_AMD64
    #define CLOVER_VERSION_ARCH "amd64"
#elif defined(LIBCORE_X86)
    #define CLOVER_VERSION_ARCH "x86"
#elif defined(LIBCORE_ARM64)
    #define CLOVER_VERSION_ARCH "arm64"
#elif defined(LIBCORE_WASM)
    #define CLOVER_VERSION_ARCH "wasm"
#endif

enum CompilerFamily {
    CC_CLANG,
    CC_GCC,
    CC_MSVC,
    CC_TCC
};

enum CompilerBackend {
    C_BACKEND, JASMINE_BACKEND
};

enum CompilerProduct {
    PROD_RUN,   // Run the program instead of producing a file.
    PROD_EXEC,  // Produce an executable.
    PROD_OBJ    // Produce a relocatable object.
};

CompilerBackend backend = C_BACKEND;
CompilerFamily family = CC_GCC;
CompilerProduct target = PROD_RUN;
const i8* cc = nullptr;
const i8* module_path = nullptr;
const i8* out_path = nullptr;
bool show_lex = false, show_ast = false, show_envs = false, show_types = false, cc_verbose = false;
bool verbose = false;

void Module::add_top_decl(AST* ast, Env* env) {
    if (ast->kind == AST_TYPEDECL) add_top_typedecl(ast, env);
    else toplevels.push({ ast, env });
}

void Module::add_top_typedecl(AST* ast, Env* env) {
    auto it = cloverinst->typedefs.find(ast->type);
    if (it == cloverinst->typedefs.end()) {
        toplevels.push({ ast, env });
        cloverinst->typedefs.put(ast->type, { env, this });
    }
}

void c_backend(Module* mod, const_slice<i8> basename) {
    i8* cdest = (i8*)mod->envctx->envspace.alloc(basename.n + 3);
    i8* hdest = (i8*)mod->envctx->envspace.alloc(basename.n + 3);
    mcpy(cdest, basename.ptr, basename.n + 1);
    mcpy(hdest, basename.ptr, basename.n + 1);
    cdest[basename.n + 1] = 'c';
    hdest[basename.n + 1] = 'h';
    cdest[basename.n + 2] = hdest[basename.n + 2] = '\0';
    fd cout = file_open({cdest, basename.n + 3}, FP_WRITE);
    fd hout = file_open({hdest, basename.n + 3}, FP_WRITE);
    mod->hpath = hdest;
    mod->cpath = cdest;

    CContext cctx(hout, cout, mod);
    emit_c_prelude(mod, mod->parser->program->env, cctx, cdest, hdest);
    emit_c_types(mod, mod->parser->program->env, cctx);
    emit_c(mod, mod->parser->program->env, mod->parser->program, cctx);

    file_close(cout);
    file_close(hout);
}

void jasmine_backend(Module* mod) {
    JasmineGenContext ctx{mod, new jasmine::JasmineModule(jasmine::Version(0, 0, 1), mod->interner->str(mod->basename)), {}};
    emit_jasmine_types(mod, mod->parser->program->env, ctx);
    emit_jasmine_prelude(mod, mod->parser->program->env, ctx);
    emit_jasmine(mod, mod->parser->program->env, mod->parser->program, ctx);

    jasmine::PassInfo info;
    ctx.mod->opt(info, jasmine::OPT_2);
    jasmine::Assembly as(&ctx.mod->strings);
    ctx.mod->compile(info, as);
    ctx.mod->format(file_stdout);
    // ctx.mod->dumpDOT(file_stdout, info);
}

Module* compile_module(Clover* clover, Interner* interner, TypeContext* typectx, EnvContext* envctx, const_slice<i8> path) {
    i64 pathlen = path.n;
    i64 dotidx = -1;
    for (i64 i = 0; i < pathlen; i ++) if (path[i] == '.') dotidx = i;
    if (dotidx < 0) fatal("Expected dot in source file name.");
    i8* pathstr = (i8*)envctx->envspace.alloc(dotidx + 1);
    mcpy(pathstr, path.ptr, dotidx);
    pathstr[dotidx] = '\0';
    i32 basename = interner->intern({ pathstr, dotidx });
    auto it = clover->modules.find(basename);
    if (it != clover->modules.end()) return it->value; // Don't reopen module.

    Module* mod = new(clover->module_space) Module();
    if (!clover->main) clover->main = mod; // First module imported is main module.
    pathstr = (i8*)envctx->envspace.alloc(path.n + 1);
    mcpy(pathstr, path.ptr, path.n);
    pathstr[path.n] = '\0';
    mod->path = pathstr;
    mod->basename = basename;
    mod->cnew_types.alloc = &typectx->typespace;
    mod->deps.alloc = &clover->module_space;

    iptr time = time_nanos();

    mod->bytes = read_bytes(path.ptr);
    if (!mod->bytes.text) fatal("Couldn't open source file.");

    UnicodeBuf ubuf;
    mod->lexer = new(clover->module_space) Lexer;
    mod->parser = new(clover->module_space) Parser;

    mod->interner = interner;
    mod->typectx = typectx;
    mod->envctx = envctx;
    mod->cloverinst = clover;

    mod->defers.alloc = &mod->parser->astspace;
    mod->ndefers.alloc = &mod->parser->astspace;
    mod->ndefers.push(0);
    mod->toplevels.alloc = &mod->parser->astspace;
    mod->automethods.alloc = &mod->parser->astspace;
    iptr timer;
    iptr decns = 0, lexns = 0, parsens = 0, envns = 0, typns = 0, chkns = 0, emitns = 0, cycles = 0;

    if (show_lex) {
        print(" === Clover Tokens === \n");
    }
    while (ubuf.byteidx < mod->bytes.length) { // while there are still bytes to read
        timer = time_nanos();
        advance_decoder(ubuf, mod);
        decns += time_nanos() - timer, timer = time_nanos();

        if (!get_error() || get_error() == LEX_ERROR) {
            advance_lexer(mod, ubuf);
            advance_interner(mod);
            lexns += time_nanos() - timer, timer = time_nanos();

            if (show_lex) {
                for (i64 i = mod->lexer->tokens.start; i != mod->lexer->tokens.end; i = (i + 1) & 16383) {
                    print(mod->lexer->tokens[i].str(mod), '(', i64(mod->lexer->tokens[i].kind), ") ");
                }
                print('\n');
            }
        }

        if (!get_error() || get_error() == PARSE_ERROR) {
            advance_parser(mod);
            parsens += time_nanos() - timer, timer = time_nanos();
        }
        cycles ++;
    }
    if (show_lex) 
        println();
    if (mod->lexer->scratch.kind == TK_STRCONST || mod->lexer->scratch.kind == TK_CHCONST)
        unexpected_eof_error(mod, mod->bytes.length, mod->lexer->line, 1, mod->lexer->column);
    Token eof = mod->lexer->tokens.elts[(mod->lexer->tokens.start - 1) & (mod->lexer->tokens.size() - 1)];
    eof.kind = TK_EOF;
    mod->lexer->tokens.push(eof);
    
    if (!get_error() || get_error() == PARSE_ERROR) advance_parser(mod);
    if (get_error()) return print_errors(io_stderr, verbose), nullptr; // Decoder, lexer, parser error.

    {
        bindlocal<arena> astspace(&mod->parser->astspace);
        for (AST* ast : mod->parser->nodes) mod->parser->program->toplevel.push((Statement*)ast);
        if (show_ast) {
            print(" === Clover AST === \n");
            format(io_stdout, mod, mod->parser->program);
            println();
        }
    }

    timer = time_nanos();
    compute_envs(mod, mod->envctx->root, mod->parser->program);
    envns = time_nanos() - timer, timer = time_nanos();

    if (get_error()) return print_errors(io_stderr, verbose), nullptr; // Typechecker env error.

    if (show_envs) {
        print(" === Clover Scope Tree === \n");
        mod->envctx->root->format(io_stdout, mod, 0);
        println();
    }

    timer = time_nanos();
    detect_types(mod, mod->envctx->root, mod->parser->program);
    typns = time_nanos() - timer, timer = time_nanos();
    if (get_error()) return print_errors(io_stderr, verbose), nullptr; // Typechecker decl error.
    
    if (show_types) {
        // print(" === Clover Detected Types === \n");
        // format(io_stdout, mod, mod->parser->program);
        // println();
    }

    timer = time_nanos();
    infer(mod, mod->envctx->root, mod->parser->program);

    if (get_error()) return print_errors(io_stderr, verbose), nullptr; // Typechecker checking error.
    
    if (show_types) {
        // print("\nInferred types:\n\n");
        // for (AST* ast : mod->parser->program->toplevel) {
        //     format(io_stdout, mod, ast);
        //     write(io_stdout, "\n");
        // }
        // print("\n\n");
    }

    typecheck(mod, mod->envctx->root, mod->parser->program);
    chkns = time_nanos() - timer, timer = time_nanos();

    if (get_error()) return print_errors(io_stderr, verbose), nullptr; // Typechecker checking error.
    
    if (show_types) {
        print(" === Clover Typed AST === \n");
        for (AST* ast : mod->parser->program->toplevel) {
            format(io_stdout, mod, ast);
            write(io_stdout, "\n");
        }
        println();
    }

    timer = time_nanos();
    // c_backend(mod, {path.ptr, dotidx});
    jasmine_backend(mod);
    emitns = time_nanos() - timer, timer = time_nanos();

    double total = decns + lexns + parsens + envns + typns + chkns + emitns;
    // print("decoding took ", decns / 1000000.0, " ms, avg ", (double)decns / cycles / UNICODE_BUFSIZE, " ns per char, ", decns / total * 100, " % total\n");
    // print("lexing took ", lexns / 1000000.0, " ms, avg ", (double)lexns / cycles / UNICODE_BUFSIZE, " ns per token, ", lexns / total * 100, " % total\n");
    // print("parsing took ", parsens / 1000000.0, " ms, avg ", (double)parsens / 1, " ns per node, ", parsens / total * 100, " % total\n");
    // print("scoping took ", envns / 1000000.0, " ms, ", envns / total * 100, " % total\n");
    // print("type declaration took ", typns / 1000000.0, " ms, ", typns / total * 100, " % total\n");
    // print("type checking took ", chkns / 1000000.0, " ms, ", chkns / total * 100, " % total\n");
    // print("code generation took ", emitns / 1000000.0, " ms, ", emitns / total * 100, " % total\n");
    // print("total took ", total / 1000000.0, " ms\n");

    return mod;
}

const i8* find_cc() {
    static i8 ccs[][16] = {
        "clang", "clang-15", "clang-14", "clang-13", "clang-12", "clang-11", "clang-10", "clang-9", "clang-8", "clang-7", "clang-6", "clang-5", "clang-4", "clang-3", "clang-2", "clang-1",
        "gcc", "gcc-12", "gcc-11", "gcc-10", "gcc-9", "gcc-8", "gcc-7", "gcc-6", "gcc-5", "gcc-4", "gcc-3", "gcc-2", "gcc-1",
        "cl", "cl.exe",
        "tcc"
    };
    i8* args[2];
    iptr n_ccs = sizeof(ccs) / 16;
    i8 buf[64];
#ifdef CLOVER_WINDOWS
    const i8* cmd = "where ";
#else
    const i8* cmd = "which ";
#endif
    iptr len = cidx(cmd, '\0');
    for (i8* cc_opt : slice<i8[16]>{ccs, n_ccs}) {
        args[0] = cc_opt;
        args[1] = nullptr;
        mset(buf, 0, 64);
        void* cur = buf;
        cur = mcpy(cur, cmd, cidx(cmd, '\0'));
        cur = mcpy(cur, cc_opt, cidx(cc_opt, '\0'));
        cur = mcpy(cur, " 1> /dev/null 2> /dev/null", 27);
        iptr result = sysrun(buf);
        if (!result) return cc_opt;
    }
    return nullptr;
}

bool startswith(const i8* str, const i8* prefix) {
    iptr n = cidx(prefix, '\0');
    for (iptr i = 0; i < n; i ++) {
        if (str[i] != prefix[i]) return false;
    }
    return true;
}

i32 help(const i8* cmd) {
    print("Usage: ", cmd, " [options...] file\n");
    print("\n");
    print("Options:\n");
    print("  -h, --help:       Prints usage information.\n");
    print("  -C, --c-target:   Compile using C backend.\n");
    print("  --cc=<path>:      Sets the C compiler to use for the C target.\n");
    print("  --cflags=<flags>: Defines custom flags to be passed to the C compiler for the C target.\n");
    print("  --cc-verbose:     Enables C compiler output when compiling using the C backend.\n");
    print("  --show-lex:       Prints the lexed tokens to standard output.\n");
    print("  --show-ast:       Prints the parsed AST to standard output.\n");
    print("  --show-envs:      Prints the environment tree to standard output.\n");
    print("  --show-types:     Prints the typechecker results to standard output.\n");
    print("  -v, --verbose:    Enables all verbose flags (--cc-verbose, --show-ast, --show-envs, --show-types).\n");
    print("  -m, --script:     Compile and run file as script (default).\n");
    print("  -p <path>:        Write generated source archive to <path>.\n");
    print("  -o <path>:        Write executable to <path>.\n");
    print("  -c <path>:        Write relocatable object file to <path>.\n");
    return 1;
}

i32 version() {
    print("Clover version ", CLOVER_VERSION_MAJOR, '.', CLOVER_VERSION_MINOR, '.', CLOVER_VERSION_PATCH, " for ", CLOVER_VERSION_ARCH, ' ', CLOVER_VERSION_OS, '\n');
    return 0;
}

void visit_module_cc(Module* mod, vec<i8>& cmdbuf) {
    for (Module* m : mod->deps) {
        if (!m->visited) m->visited = true, visit_module_cc(m, cmdbuf); 
    }
    auto ptr = mod->cpath;
    cmdbuf.push(' ');
    while (*ptr) cmdbuf.push(*ptr ++);
}

void add_flag_cc(const i8* flag, vec<i8>& cmdbuf) {
    cmdbuf.push(' ');
    while (*flag) cmdbuf.push(*flag ++);
}

void invoke_cc(Clover& clover, const i8* cc, vec<i8>& cmdbuf) {
    while (*cc) cmdbuf.push(*cc ++);
    add_flag_cc("-I.", cmdbuf); 
    add_flag_cc("-L./bin", cmdbuf);
    add_flag_cc("-nodefaultlibs", cmdbuf);
    add_flag_cc("-nostdlib", cmdbuf);
    add_flag_cc("-O3", cmdbuf);
    for (auto& m : clover.modules) m.value->visited = false; // Reset visited state.
    visit_module_cc(clover.main, cmdbuf);
    add_flag_cc("-lcclover", cmdbuf);
    if (!cc_verbose) add_flag_cc("1> /dev/null 2> /dev/null", cmdbuf);
    cmdbuf.push('\0');

    if (cc_verbose) print(const_slice<i8>{&cmdbuf[0], cmdbuf.size()});
    iptr result = sysrun(&cmdbuf[0]);
    if (result) fatal("C Compiler error.");
    if (target == PROD_RUN) {
        sysrun("./a.out");
        sysrun("rm -f ./a.out");
    }
    else if (target == PROD_EXEC) {
        i8 buf[65536];
        void* cur = mcpy(buf, "mv ./a.out ", 11);
        mcpy(cur, out_path, cidx(out_path, '\0') + 1);
        sysrun(buf);
    }
}

i32 clover_main(i32 argc, i8** argv) {
    if (argc == 1) return help(argv[0]);

    for (int i = 0; i < argc - 1; i ++) {
        i8* arg = argv[i + 1];
        if (!mcmp(arg, "-h", 3) || !mcmp(arg, "--help", 7)) {
            return help(argv[0]);
        }
        if (!mcmp(arg, "--version", 10)) {
            return version();
        }
        else if (!mcmp(arg, "-o", 3)) {
            i ++;
            target = PROD_EXEC;
            out_path = argv[i + 1];
        }
        else if (!mcmp(arg, "-c", 3)) {
            i ++;
            target = PROD_OBJ;
            out_path = argv[i + 1];
        }
        else if (!mcmp(arg, "-C", 3) || !mcmp(arg, "--c-target", 11)) {
            backend = C_BACKEND;
        }
        else if (!mcmp(arg, "--cc-verbose", 13)) {
            cc_verbose = true;
        }
        else if (!mcmp(arg, "--show-lex", 11)) {
            show_lex = true;
        }
        else if (!mcmp(arg, "--show-ast", 11)) {
            show_ast = true;
        }
        else if (!mcmp(arg, "--show-envs", 12)) {
            show_envs = true;
        }
        else if (!mcmp(arg, "--show-types", 13)) {
            show_types = true;
        }
        else if (!mcmp(arg, "--show-all", 10)) {
            cc_verbose = true;
            show_lex = true;
            show_ast = true;
            show_envs = true;
            show_types = true;
        }
        else if (!mcmp(arg, "-v", 3) || !mcmp(arg, "--verbose", 10)) {
            verbose = true;
        }
        else if (startswith(arg, "--cc=")) cc = arg + 5;
        else if (startswith(arg, "-")) fatal("Unknown command-line option.");
        else if (!module_path) module_path = arg;
        else fatal("Module path was already specified.");
    }

    if (!cc) cc = find_cc();
    if (!cc) fatal("Could not find a valid C compiler.");

    Interner interner;
    EnvContext envctx;
    TypeContext typectx(&envctx, &interner);
    envctx.create_root_env(interner, typectx);

    Clover clover;
    clover.modules.alloc = &envctx.envspace;
    clover.typedefs.alloc = &envctx.envspace;
    clover.main = nullptr;
    compile_module(&clover, &interner, &typectx, &envctx, { module_path, cidx(module_path, '\0') });

    // if (!get_error()) {
    //     allocator cmd_alloc;
    //     vec<i8> cmdbuf;
    //     cmdbuf.alloc = &cmd_alloc;
    //     invoke_cc(clover, cc, cmdbuf);
    // }

    return get_error();
}