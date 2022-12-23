#include "basil/basil.h"
#include "core/sys.h"
#include "lib/io.h"
#include "lib/utf.h"
#include "basil/lex.h"
#include "basil/value.h"

MODULE(basil)

Basil* basilInstance;
Module* activeModule;
vec<i8> replSource;
Lexer replLexer;

i32 Options::flags;
Options::Mode Options::mode;
vec<const i8*> Options::sourceFiles;
vec<const i8*> Options::directSource;

void repl() {
    new (&replSource) vec<i8>;
    new (&replLexer) Lexer;

    vec<Token> tokens;
    Value root = createRoot();
    Value global = Object::Record(root.asObj()->asRecord());
    Module mod;
    activeModule = &mod;
    TokenView view{{}, tokens, 0};
    view.indents.push(0);
    while (true) {
        Value expr = parseRepl(view);
        Value result = eval(global, expr, false);
        println(result);
    }
}

void compileModule(const_slice<i8> path) {
    fd file = file_open(path, FP_READ);
    if (file < 0) fatal("Couldn't open file.");

    i8 buffer[65536];
    iptr length = 0, bytes;
    do {
        bytes = file_read(file, {buffer, 65536});
        length += bytes;
    } while (bytes > 0);

    file_close(file);
    file = file_open(path, FP_READ);

    i8* persistent_buf = new i8[length + 1];
    iptr actually_read = file_read(file, {persistent_buf, length});
    assert(actually_read == length);
    file_close(file);
    if (persistent_buf[length - 1] != '\n') persistent_buf[length ++] = '\n';

    Module mod;
    activeModule = &mod;
    mod.source = slice<i8>(persistent_buf, length);

    iptr ulength = utf8_length(persistent_buf, length);
    rune* runes = new rune[ulength];
    utf8_decode(persistent_buf, length, runes, ulength);

    Lexer lexer;
    vec<Token> tokens;
    lex(lexer, {runes, ulength}, tokens);

    Value program = parse(tokens);
    println("Program: ", program);

    Value root = createRoot();
    Value global = Object::Record(root.asObj()->asRecord());
    Value result = eval(global, program, false);
    println(result);
}

ENDMODULE()