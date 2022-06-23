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

enum CompilerFamily {
    CC_CLANG,
    CC_GCC,
    CC_MSVC,
    CC_TCC
};

enum CompilerBackend {
    C_BACKEND
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

Module* compile_module(Clover* clover, Interner* interner, TypeContext* typectx, EnvContext* envctx, const_slice<i8> path) {
    i64 pathlen = path.n;
    i64 dotidx = -1;
    for (i64 i = 0; i < pathlen; i ++) if (path[i] == '.') dotidx = i;
    if (dotidx < 0) fatal("Expected dot in source file name.");
    i8* pathstr = (i8*)envctx->envspace.alloc(dotidx + 1);
    mcpy(pathstr, path.ptr, dotidx);
    pathstr[dotidx] = '\0';
    i32 basename = interner->intern({{ pathstr, dotidx }});
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

    iptr time = nanotime();

    mod->bytes = read_bytes(path.ptr);
    if (!mod->bytes.text) fatal("Couldn't open source file.");

    UnicodeBuf ubuf;
    mod->lexer = new(clover->module_space) Lexer;
    mod->parser = new(clover->module_space) Parser;

    mod->interner = interner;
    mod->typectx = typectx;
    mod->envctx = envctx;
    mod->cloverinst = clover;
    iptr timer;
    iptr decns = 0, lexns = 0, parsens = 0, envns = 0, typns = 0, chkns = 0, cycles = 0;

    if (show_lex) print("\nLexed tokens:\n\n");
    while (ubuf.byteidx < mod->bytes.length) { // while there are still bytes to read
        timer = nanotime();
        advance_decoder(ubuf, mod);
        decns += nanotime() - timer, timer = nanotime();

        if (!get_error() || get_error() == LEX_ERROR) {
            advance_lexer(mod, ubuf);
            advance_interner(mod);
            lexns += nanotime() - timer, timer = nanotime();

            if (show_lex) {
                for (i64 i = mod->lexer->tokens.start; i != mod->lexer->tokens.end; i = (i + 1) & 16383) {
                    print(mod->lexer->tokens[i].str(mod), '(', i64(mod->lexer->tokens[i].kind), ") ");
                }
                print('\n');
            }
        }

        if (!get_error() || get_error() == PARSE_ERROR) {
            advance_parser(mod);
            parsens += nanotime() - timer, timer = nanotime();
        }
        cycles ++;
    }
    if (mod->lexer->scratch.kind == TK_STRCONST || mod->lexer->scratch.kind == TK_CHCONST)
        unexpected_eof_error(mod, mod->bytes.length, mod->lexer->line, 1, mod->lexer->column);
    Token eof = mod->lexer->tokens.elts[mod->lexer->tokens.start - 1 & mod->lexer->tokens.size() - 1];
    eof.kind = TK_EOF;
    mod->lexer->tokens.push(eof);
    
    if (!get_error() || get_error() == PARSE_ERROR) advance_parser(mod);
    if (get_error()) return print_errors(stderr, verbose), nullptr; // Decoder, lexer, parser error.

    {
        bindlocal<arena> astspace(&mod->parser->astspace);
        for (AST* ast : mod->parser->nodes) mod->parser->program->toplevel.push((Statement*)ast);
        if (show_ast) {
            print("\nParsed nodes:\n\n");
            format(stdout, mod, mod->parser->program);
        }
    }

    timer = nanotime();
    compute_envs(mod, mod->envctx->root, mod->parser->program);
    envns = nanotime() - timer, timer = nanotime();

    if (get_error()) return print_errors(stderr, verbose), nullptr; // Typechecker env error.

    if (show_envs) {
        print("\nEnv tree:\n\n");
        mod->envctx->root->format(stdout, mod, 0);
    }

    timer = nanotime();
    detect_types(mod, mod->envctx->root, mod->parser->program);
    typns = nanotime() - timer, timer = nanotime();

    if (get_error()) return print_errors(stderr, verbose), nullptr; // Typechecker decl error.
    
    if (show_types) {
        print("\nDetected types:\n\n");
        format(stdout, mod, mod->parser->program);
    }

    timer = nanotime();
    typecheck(mod, mod->envctx->root, mod->parser->program);
    chkns = nanotime() - timer, timer = nanotime();

    if (get_error()) return print_errors(stderr, verbose), nullptr; // Typechecker checking error.
    
    if (show_types) {
        print("\nChecked types:\n\n");
        for (AST* ast : mod->parser->program->toplevel) {
            format(stdout, mod, ast);
            write(stdout, " : ");
            format(stdout, mod, ast->type);
            write(stdout, "\n");
        }
        print("\n\n");
    }

    i8* cdest = (i8*)mod->envctx->envspace.alloc(dotidx + 3);
    i8* hdest = (i8*)mod->envctx->envspace.alloc(dotidx + 3);
    mcpy(cdest, path.ptr, dotidx + 1);
    mcpy(hdest, path.ptr, dotidx + 1);
    cdest[dotidx + 1] = 'c';
    hdest[dotidx + 1] = 'h';
    cdest[dotidx + 2] = hdest[dotidx + 2] = '\0';
    stream* cout = open(cdest, FP_WRITE);
    stream* hout = open(hdest, FP_WRITE);
    mod->hpath = hdest;
    mod->cpath = cdest;

    CContext cctx(*hout, *cout, mod);
    emit_c_prelude(mod, mod->parser->program->env, cctx, cdest, hdest);
    emit_c_types(mod, mod->parser->program->env, cctx);
    emit_c(mod, mod->parser->program->env, mod->parser->program, cctx);

    double total = decns + lexns + parsens + envns + typns + chkns;
    // print("decoding took ", decns / 1000000.0, " ms, avg ", (double)decns / cycles / UNICODE_BUFSIZE, " ns per char, ", decns / total * 100, " % total\n");
    // print("lexing took ", lexns / 1000000.0, " ms, avg ", (double)lexns / cycles / UNICODE_BUFSIZE, " ns per token, ", lexns / total * 100, " % total\n");
    // print("parsing took ", parsens / 1000000.0, " ms, avg ", (double)parsens / 1, " ns per node, ", parsens / total * 100, " % total\n");
    // print("scoping took ", envns / 1000000.0, " ms, ", envns / total * 100, " % total\n");
    // print("type declaration took ", typns / 1000000.0, " ms, ", typns / total * 100, " % total\n");
    // print("type checking took ", chkns / 1000000.0, " ms, ", chkns / total * 100, " % total\n");

    close(cout);
    close(hout);

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
    add_flag_cc("-Os", cmdbuf);
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
    clover.main = nullptr;
    compile_module(&clover, &interner, &typectx, &envctx, { module_path, cidx(module_path, '\0') });

    if (!get_error()) {
        allocator cmd_alloc;
        vec<i8> cmdbuf;
        cmdbuf.alloc = &cmd_alloc;
        invoke_cc(clover, cc, cmdbuf);
    }

    return get_error();
}