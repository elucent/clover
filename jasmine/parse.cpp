#include "jasmine/parse.h"
#include "jasmine/data.h"
#include "jasmine/obj.h"
#include "jasmine/insn.h"
#include "lib/str.h"
#include "lib/utf.h"

// version 0.0.0
// meta "foo": "bar"
// $0 = const i64 1
// $1 = static i64 2
// $2 = func i64(i64, i64) {
// .0:
//   %2 = add i64 %0, %1
//   ret i64 %2   
// }

map<const_slice<i8>, Op, 128>* OPCODE_MAP;
map<const_slice<i8>, typeidx, 32>* TYPE_MAP;

struct ByteSource {
    iptr length;
    i8* text;
};

struct Buf {
    const_slice<rune> runes;
    i32 i;

    rune peek() const {
        return i >= runes.n ? rune(0) : runes[i];
    }

    rune read() {
        if (i >= runes.n) return 0;
        else return runes[i ++];
    }

    void skipws() {
        while (utf8_is_space_separator(peek()) && peek() != '\n') read();
    }

    void expect(rune r) {
        skipws();
        if (read() != r) fatal("Expected char.");
    }
};

i64 parse_integer(Buf& buf) {
    buf.skipws();
    i64 acc = 0;
    while (utf8_is_digit(buf.peek())) {
        acc *= 10;
        acc += utf8_digit_value(buf.read());
    }
    return acc;
}

double parse_float(Buf& buf) {
    buf.skipws();
    i64 acc = 0;
    while (utf8_is_digit(buf.peek())) {
        acc *= 10;
        acc += utf8_digit_value(buf.read());
    }
    if (buf.peek() != '.') return double(acc);
    else buf.read();
    i64 decimal = 0, div = 1;
    while (utf8_is_digit(buf.peek())) {
        decimal *= 10;
        div *= 10;
        decimal += utf8_digit_value(buf.read());
    }
    return acc + double(decimal) / div;
}

bool delimiter(rune r) {
    return r == '{' || r == '}' || r == '[' || r == ']' || r == '(' || r == ')' || r == ',' || utf8_is_space_separator(r);
}

const_slice<i8> parse_ident(Buf& buf) {
    buf.skipws();
    const rune* front = &buf.runes[buf.i];
    while (!delimiter(buf.peek())) buf.read();
    const rune* back = &buf.runes[buf.i];
    const_slice<rune> runes = { front, back };
    i32 n_bytes = utf8_bytes(runes.ptr, runes.n);
    slice<i8> bytes = { new i8[n_bytes], n_bytes };
    utf8_encode(runes.ptr, runes.n, bytes.ptr, bytes.n);
    return bytes;
}

const_slice<i8> parse_string(Buf& buf) {
    buf.expect('"');
    const rune* front = &buf.runes[buf.i];
    while (buf.peek() != '"') buf.read();
    const rune* back = &buf.runes[buf.i];
    const_slice<rune> runes = { front, back };
    i32 n_bytes = utf8_bytes(runes.ptr, runes.n);
    slice<i8> bytes = { new i8[n_bytes], n_bytes };
    utf8_encode(runes.ptr, runes.n, bytes.ptr, bytes.n);
    buf.expect('"');
    return bytes;
}

void parse_meta(JasmineModule* mod, Buf& buf) {
    buf.skipws();
    const_slice<i8> key = parse_string(buf);
    buf.expect(':');
    const_slice<i8> value = parse_string(buf);
    mod->meta.add(key, value);
    buf.expect('\n');
}

void parse_version(JasmineModule* mod, Buf& buf) {
    i64 major = parse_integer(buf);
    buf.expect('.');
    i64 minor = parse_integer(buf);
    buf.expect('.');
    i64 patch = parse_integer(buf);
    if (major < 0 || major > 255) fatal("Major version too large! Must be between 0 and 255.");
    if (minor < 0 || minor > 255) fatal("Minor version too large! Must be between 0 and 255.");
    if (patch < 0 || patch > 255) fatal("Patch version too large! Must be between 0 and 255.");
    mod->meta.ver = Version(major, minor, patch);
    buf.expect('\n');
}

void parse_setglobal(JasmineModule* mod, Buf& buf) {
    buf.read();
    while (!delimiter(buf.peek())) buf.read();
    buf.expect('=');
    buf.skipws();
}

pair<Param, Arg> parse_arg(Buf& buf) {
    buf.skipws();
    if (utf8_is_digit(buf.peek())) return pair<Param, Arg>{ P_IMM, Arg(parse_integer(buf)) };
    else if (buf.peek() == '%') return buf.read(), pair<Param, Arg>{ P_REG, Arg(parse_integer(buf)) };
    else if (buf.peek() == '.') return buf.read(), pair<Param, Arg>{ P_LABEL, Arg(parse_integer(buf)) };
    else if (buf.peek() == '$') return buf.read(), pair<Param, Arg>{ P_FUNC, Arg(parse_integer(buf)) };
    else return fatal("Unknown parameter."), pair<Param, Arg>{};
}

typeidx parse_type(JasmineModule* mod, Buf& buf);

typeidx parse_struct(JasmineModule* mod, Buf& buf) {
    buf.expect('{');
    TypeVec v;
    v.alloc = &mod->modspace;
    while (buf.peek() != '}') {
        if (v.size()) buf.expect(',');
        v.push(parse_type(mod, buf));
    }
    buf.expect('}');
    return t_tuple(mod->types, v);
}

typeidx parse_type(JasmineModule* mod, Buf& buf) {
    buf.skipws();
    typeidx id;
    if (buf.peek() == '{') id = parse_struct(mod, buf);
    else id = (*TYPE_MAP)[parse_ident(buf)];

    if (buf.peek() == '[') {
        buf.read();
        i64 size = parse_integer(buf);
        buf.expect(']');
        id = t_array(mod->types, id, size);
    }
    else if (buf.peek() == '(') {
        buf.read();
        TypeVec v;
        v.alloc = &mod->modspace;
        while (buf.peek() != ')') {
            if (v.size()) buf.expect(',');
            v.push(parse_type(mod, buf));
        }
        buf.expect(')');
        id = t_fun(mod->types, id, v);
    }
    return id;
}

void parse_setlocal(JasmineModule* mod, Buf& buf) {
    buf.read();
    while (!delimiter(buf.peek())) buf.read();
    buf.expect('=');
    buf.skipws();
}

void parse_insn(JasmineModule* mod, Function* func, Buf& buf) {
    buf.skipws();
    if (buf.peek() == '.') {
        buf.read();
        parse_integer(buf);
        jasm::label(func->label());
        buf.expect(':');
        buf.expect('\n');
        return;
    }
    else if (buf.peek() == '%') parse_setlocal(mod, buf);


    Insn insn;
    insn.op = (*OPCODE_MAP)[parse_ident(buf)];
    insn.pidx = func->args.size();
    Arity a = OP_ARITIES[insn.op];
    switch (a) {
        case NULLARY:
            insn.type = parse_type(mod, buf);
            buf.expect('\n');
            break;
        case UNARY: {
            insn.type = parse_type(mod, buf);
            insn.nparams = 1;
            auto arg = parse_arg(buf);   
            func->params.push(arg.first); 
            func->args.push(arg.second);
            buf.expect('\n');
            break;
        }
        case VUNARY: {
            insn.type = parse_type(mod, buf);
            insn.nparams = 1;
            auto arg = parse_arg(buf);   
            func->params.push(arg.first); 
            func->args.push(arg.second);
            while (buf.peek() != '\n') {
                buf.expect(',');
                auto arg = parse_arg(buf);   
                func->params.push(arg.first); 
                func->args.push(arg.second);
                insn.nparams ++;
                buf.skipws();
            }
            buf.expect('\n');
            break;
        }
        case BINARY: {
            insn.type = parse_type(mod, buf);
            insn.nparams = 2;
            auto lhs = parse_arg(buf);   
            func->params.push(lhs.first); 
            func->args.push(lhs.second);
            buf.expect(',');
            auto rhs = parse_arg(buf);   
            func->params.push(rhs.first); 
            func->args.push(rhs.second);
            buf.expect('\n');
            break;
        }
        case VBINARY: {
            insn.type = parse_type(mod, buf);
            insn.nparams = 2;
            auto lhs = parse_arg(buf);   
            func->params.push(lhs.first); 
            func->args.push(lhs.second);
            buf.expect('(');
            auto rhs = parse_arg(buf);   
            func->params.push(rhs.first); 
            func->args.push(rhs.second);
            while (buf.peek() != ')') {
                buf.expect(',');
                auto arg = parse_arg(buf);   
                func->params.push(arg.first); 
                func->args.push(arg.second);
                insn.nparams ++;
                buf.skipws();
            }
            buf.expect(')');
            buf.expect('\n');
            break;
        }
        case TERNARY: {
            insn.type = parse_type(mod, buf);
            insn.nparams = 3;
            auto lhs = parse_arg(buf);   
            func->params.push(lhs.first); 
            func->args.push(lhs.second);
            buf.expect(',');
            auto rhs = parse_arg(buf);   
            func->params.push(rhs.first); 
            func->args.push(rhs.second);
            buf.expect(',');
            auto third = parse_arg(buf);   
            func->params.push(third.first); 
            func->args.push(third.second);
            buf.expect('\n');
            break;
        }
    }
    func->insns.push(insn);
}

void parse_function(JasmineModule* mod, Buf& buf) {
    buf.skipws();
    typeidx ft = parse_type(mod, buf);
    const_slice<i8> ident = parse_ident(buf);
    buf.expect('{');
    buf.expect('\n');
    Function* func = new(mod->modspace) Function(mod);
    jasm::targetfn = func;
    func->name = mod->strings.intern(ident);
    while (buf.peek() != '}') parse_insn(mod, func, buf);
    buf.expect('}');
    buf.expect('\n');
    mod->funcs.push(func);
}

void parse_toplevel(JasmineModule* mod, Buf& buf) {
    buf.skipws();
    if (buf.peek() == '$') parse_setglobal(mod, buf);
    i8 kw[8];
    rune kwr[8];
    i32 i = 0;
    while (!utf8_is_space_separator(buf.peek()) && i < 8) kwr[i ++] = buf.read();
    const_slice<i8> keyword = { kw, (iptr)utf8_encode(kwr, i, kw, 8) };
    if (keyword == const_slice<i8>{"func", 4}) parse_function(mod, buf);
    // else if (keyword == const_slice{"static", 7}) parse_static(mod, buf);
    // else if (keyword == const_slice{"data", 7}) parse_data(mod, buf);
    else if (keyword == const_slice<i8>{"meta", 7}) parse_meta(mod, buf);
    else if (keyword == const_slice<i8>{"version", 7}) parse_version(mod, buf);
    else buf.read();
}

static ByteSource read_bytes(const i8* path) {
    fd file = fdopen(path, FP_READ);
    if (file < 0) fatal("Couldn't open file.");

    i8 buffer[65536];
    iptr length = 0, bytes;
    do {
        bytes = fdread(file, {buffer, 65536});
        length += bytes;
    } while (bytes > 0);

    fdclose(file);
    file = fdopen(path, FP_READ);

    i8* persistent_buf = new i8[length + 1];
    iptr actually_read = fdread(file, {persistent_buf, length});
    assert(actually_read == length);
    fdclose(file);
    if (persistent_buf[length - 1] != '\n') persistent_buf[length ++] = '\n';
    return ByteSource{length, persistent_buf};
}

JasmineObject* parse(const i8* path) {
    map<const_slice<i8>, Op, 128> opcodeMap;
    for (i32 i = 0; i < N_OPCODES; i ++) opcodeMap.put({ OP_NAMES[i], cidx(OP_NAMES[i], '\0') }, Op(i));
    OPCODE_MAP = &opcodeMap;

    map<const_slice<i8>, typeidx, 32> typeMap;
    typeMap[{"void", 4}] = T_VOID;
    typeMap[{"i8", 2}] = T_I8;
    typeMap[{"i16", 3}] = T_I16;
    typeMap[{"i32", 3}] = T_I32;
    typeMap[{"i64", 3}] = T_I64;
    typeMap[{"u8", 2}] = T_U8;
    typeMap[{"u16", 3}] = T_U16;
    typeMap[{"u32", 3}] = T_U32;
    typeMap[{"u64", 3}] = T_U64;
    typeMap[{"f32", 3}] = T_F32;
    typeMap[{"f64", 3}] = T_F64;
    typeMap[{"ptr", 3}] = T_PTR;
    typeMap[{"ref", 3}] = T_REF;
    typeMap[{"iword", 5}] = T_IWORD;
    typeMap[{"uword", 5}] = T_UWORD;
    TYPE_MAP = &typeMap;

    ByteSource bytes = read_bytes(path);
    i32 n = utf8_length(bytes.text, bytes.length);
    slice<rune> runes = {new rune[n], n};
    utf8_decode(bytes.text, bytes.length, runes.ptr, runes.n);
    Buf buf = {runes, 0};

    JasmineObject* obj = new JasmineObject();
    JasmineModule* mod = new JasmineModule(Version(0, 0, 0), {path, cidx(path, '\0')});
    while (buf.peek()) parse_toplevel(mod, buf);
    obj->modules.put(mod->strings.strings[mod->meta.modname], mod);
    obj->moduleseq.push(mod);
    return obj;
}