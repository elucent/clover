#include "basil/value.h"
#include "basil/lex.h"
#include "lib/str.h"

MODULE(basil)

extern vec<i8> replSource;
extern Lexer replLexer;

void replRead(vec<Token>& tokens) {
    vec<i8> bytes;
    i8 ch = read_byte(file_stdin);
    while (ch != '\n') {
        bytes.push(ch);
        ch = read_byte(file_stdin);
    }
    bytes.push(ch);
    for (i8 byte : bytes)
        replSource.push(byte);
    activeModule->source = { (i8*)replSource.data, replSource.size() };
    i32 n_runes = utf8_length((const i8*)bytes.data, bytes.size());
    rune* runes = new rune[n_runes];
    utf8_decode((const i8*)bytes.data, bytes.size(), runes, n_runes);
    lex(replLexer, {runes, n_runes}, tokens);
    delete[] runes;
}

Value parse(TokenView& view, vec<Value>& buf);

template<typename... Args>
void parseError(const Token& token, const Args&... args) {
    println("[PARSE ERROR] Line ", token.line + 1, ": ", args...);
    process_exit(42);
}

void consumeNewlines(TokenView& view) {
    while (view.peek().kind == TK_NEWLINE)
        view.read();
}

void consumeNewlinesWhileNested(TokenView& view) {
    while (view.depth && view.peek().kind == TK_NEWLINE)
        view.read();
}

Value parseEnclosed(TokenView& view, vec<Value>& buf, TokenKind end) {
    view.depth ++;
    view.read();

    // Count is the total number of expressions, localcount is the number of expressions since a separator (like ,)
    i32 count = 0, localcount = 0;
    bool hasComma = false;
    while (view && view.peek().kind != end) {
        buf.push(parse(view, buf));
        ++ count;
        ++ localcount;

        if (view.peek().kind == TK_COMMA) {
            hasComma = true;
            if (localcount == 0)
                parseError(view.peek(), "Expected at least one expression before comma.");

            count -= localcount - 1; // We are compressing <localcount> expressions into just one list.
            if (localcount > 1) {
                Value locallist = Value::Nil();
                for (i32 i = 0; i < localcount; i ++)
                    locallist = Object::List(buf.pop(), locallist);
                buf.push(locallist);
            }
            localcount = 0;
            view.read();
        }
    }

    if (hasComma && localcount > 1) {
        count -= localcount - 1;
        Value locallist = Value::Nil();
        for (i32 i = 0; i < localcount; i ++)
            locallist = Object::List(buf.pop(), locallist);
        buf.push(locallist);
    }

    if (view.peek().kind != end) parseError(view.peek(), "Expected closing punctuation.");
    view.read();

    Value list = Value::Nil();
    for (i32 i = 0; i < count; i ++)
        list = Object::List(buf.pop(), list);
    view.depth --;
    return list;
}

Value parsePrimary(TokenView& view, vec<Value>& buf) {
    consumeNewlinesWhileNested(view);
    if (!view) parseError(view.peek(), "Expected primary expression.");
    const Token& front = view.peek();
    SymbolTable& syms = basilInstance->symbols;
    slice<i8> source = activeModule->source;
    switch (front.kind) {
        case TK_ICONST:
            view.read();
            return Value::Int(toint({ source.ptr + front.start, front.end - front.start }));
        case TK_OPERATOR:
        case TK_SYMBOL:
            view.read();
            return Value::Sym(syms.intern({ source.ptr + front.start, front.end - front.start }));
        case TK_LPAREN:
            return parseEnclosed(view, buf, TK_RPAREN);
        case TK_LSQUARE:
            return Object::List(Value::Sym(syms.intern({"list", 4})), parseEnclosed(view, buf, TK_RSQUARE));
        case TK_LBRACE:
            return Object::List(Value::Sym(syms.intern({"record", 6})), parseEnclosed(view, buf, TK_RBRACE));
        default:
            parseError(front, "Expected primary expression.");
            return Value::Nil();
    }
}

Value parseSuffixedPrimary(TokenView& view, vec<Value>& buf) {
    Value first = parsePrimary(view, buf);
    consumeNewlinesWhileNested(view);
    Token front = view.peek();
    SymbolTable& syms = basilInstance->symbols;
    do switch (front.kind) {
        case TK_DOT:
            view.read();
            first = Object::ListOf(Value::Sym(syms.intern({".", 1})), first, parsePrimary(view, buf));
            consumeNewlinesWhileNested(view);
            front = view.peek();
            break;
        default:
            return first;
    } while (true);
}

Value parseOperator(const Token& op) {
    SymbolTable& syms = basilInstance->symbols;
    slice<i8> source = activeModule->source;
    return Value::Sym(syms.intern({ source.ptr + op.start, op.end - op.start }));
}

Value parseBinary(TokenView& view, vec<Value>& buf, Value lhs, const Token& op) {
    consumeNewlines(view);
    OperatorPrecedence opPrec = operators[activeModule->source[op.start]];
    Value rhs = parseSuffixedPrimary(view, buf);
    consumeNewlinesWhileNested(view);
    if (view.peek().kind != TK_OPERATOR)
        return Object::ListOf(parseOperator(op), lhs, rhs);
    
    const Token& nextOp = view.read();
    OperatorPrecedence nextOpPrec = operators[activeModule->source[nextOp.start]];
    if (nextOpPrec > opPrec)
        return Object::ListOf(parseOperator(op), lhs, parseBinary(view, buf, rhs, nextOp));
    else
        return parseBinary(view, buf, Object::ListOf(parseOperator(op), lhs, rhs), nextOp);
}

Value parse(TokenView& view, vec<Value>& buf) {
    Value primary = parseSuffixedPrimary(view, buf);
    consumeNewlinesWhileNested(view);
    if (view.peek().kind == TK_OPERATOR)
        primary = parseBinary(view, buf, primary, view.read());
    return primary;
}

Value parseLine(TokenView& view, vec<Value>& buf);

Value parseIndented(TokenView& view, vec<Value>& buf) {
    i32 count = 0;
    view.indents.push(view.peek().col);
    while (view.peek().col >= view.indents.back())
        buf.push(parseLine(view, buf)), ++ count;
    view.indents.pop();

    Value front = Value::Sym(basilInstance->symbols.intern({"seq", 3}));
    if (count == 1) return Object::ListOf(front, buf.pop());
    
    Value list = Value::Nil();
    for (i32 i = 0; i < count; i ++)
        list = Object::List(buf.pop(), list);
    return Object::List(front, list);
}

Value parseLine(TokenView& view, vec<Value>& buf) {
    i32 count = 0, localcount = 0;
    bool hasComma = false;
    while (view && view.peek().kind != TK_NEWLINE) {
        buf.push(parse(view, buf));
        ++ count;
        ++ localcount;

        if (view.peek().kind == TK_COMMA) {
            hasComma = true;
            if (localcount == 0)
                parseError(view.peek(), "Expected at least one expression before comma.");

            count -= localcount - 1; // We are compressing <localcount> expressions into just one list.
            if (localcount > 1) {
                Value locallist = Value::Nil();
                for (i32 i = 0; i < localcount; i ++)
                    locallist = Object::List(buf.pop(), locallist);
                buf.push(locallist);
            }
            localcount = 0;
            view.read();
        }
    }

    if (hasComma && localcount > 1) {
        count -= localcount - 1;
        Value locallist = Value::Nil();
        for (i32 i = 0; i < localcount; i ++)
            locallist = Object::List(buf.pop(), locallist);
        buf.push(locallist);
    }

    if (Options::mode == Options::REPL) {
        view.read();
        print(". ");
        flush(file_stdout);
        replRead(view.tokens);
    }
    while (view.peek().kind == TK_NEWLINE) view.read();

    if (view.peek().col > view.indents.back()) {
        buf.push(parseIndented(view, buf));
        count ++;
    }

    if (count == 1) return buf.pop();
    
    Value list = Value::Nil();
    for (i32 i = 0; i < count; i ++)
        list = Object::List(buf.pop(), list);
    return list;
}

Value parseRepl(TokenView& view) {
    vec<Value> buf;
    print("? ");
    flush(file_stdout);
    replRead(view.tokens);
    return parseLine(view, buf);
}

Value parse(vec<Token>& tokens) {
    vec<Value> buf;
    TokenView view{{}, tokens, 0};
    view.indents.push(0);
    vec<Value> values;
    while (view) values.push(parseLine(view, buf));
    return values.size() == 1 ? values.back() : Object::List(Value::Sym(basilInstance->symbols.intern({"seq", 3})), Object::ListOf(values));
}

void write_impl(fd io, Value value) {
    if (value.isInt())
        write(io, value.asInt());
    else if (value.isNum())
        write(io, value.asNum());
    else if (value.isSym())
        write(io, basilInstance->symbols.str(value.asSym()));
    else if (value.isNil())
        write(io, "()");
    else if (value.isObj()) {
        bool first = true;
        if (value.asObj()->isList()) {
            write(io, '(');
            while (value.isObj() && value.asObj()->isList()) {
                if (!first) write(io, ' ');
                first = false;
                write(io, value.asObj()->asList()->hd);
                value = value.asObj()->asList()->tl;
            }
            write(io, ')');
        }
        else if (value.asObj()->isRecord()) {
            write(io, '{');
            for (const auto& pair : value.asObj()->asRecord()->table) {
                if (!first) write(io, ' ');
                first = false;
                write(io, basilInstance->symbols.str(pair.key), "=");
                write(io, pair.value);
            }
            write(io, '}');
        }
        else if (value.asObj()->isFunction()) {
            if (value.asObj()->asFunction()->builtin)
                write(io, basilInstance->symbols.str(value.asObj()->asFunction()->builtin->name));
            else
                write(io, "#procedure", value.asObj()->asFunction()->env);
        }
        else if (value.asObj()->isData()) {
            write(io, '<', basilInstance->symbols.str(value.asObj()->asData()->name), ":");
            if (value.asObj()->asData()->traits.size() == 0)
                write(io, '?');
            else for (Value trait : value.asObj()->asData()->traits) 
                write(io, ' ', trait);
            write(io, '>');
        }
    }
}

template<typename... Args>
void evalError(const Args&... args) {
    println("[EVAL ERROR] ", args...);
    process_exit(43);
}

Value evalList(Value env, Value list, bool isRecord) {
    if (list.isNil()) 
        return Value::Nil();
    else
        return Object::List(eval(env, list.asObj()->asList()->hd, isRecord), evalList(env, list.asObj()->asList()->tl, isRecord));
}

Value car(Value list) {
    if (list.isNil())
        return Value::Nil();
    else
        return list.asObj()->asList()->hd;
}

Value cdr(Value list) {
    if (list.isNil())
        return Value::Nil();
    else
        return list.asObj()->asList()->tl;
}

Value cadr(Value list) {
    if (list.isNil())
        return Value::Nil();
    else
        return car(list.asObj()->asList()->tl);
}

Value caddr(Value list) {
    if (list.isNil())
        return Value::Nil();
    else
        return car(cdr(list.asObj()->asList()->tl));
}

List* asList(Value list) {
    if (list.isObj() && list.asObj()->isList())
        return list.asObj()->asList();
    else
        panic("Not a list!");
    return nullptr;
}

Record* asRecord(Value rec) {
    if (rec.isObj() && rec.asObj()->isRecord())
        return rec.asObj()->asRecord();
    else
        panic("Not a record!");
    return nullptr;
}

Data* asData(Value data) {
    if (data.isObj() && data.asObj()->isData())
        return data.asObj()->asData();
    else
        panic("Not a data variable!");
    return nullptr;
}

template<typename Func>
bool anyTrueTrait(Func func, Value data) {
    Data* dataPtr = data.asObj()->asData();
    for (Value trait : dataPtr->traits)
        if (func(trait)) return true;
    return false;
}

Value addTrait(Value data, Value trait) {
    Data* dataPtr = data.asObj()->asData();
    dataPtr->traits.push(trait);
    return data;
}

bool hasNoTraits(Value data) {
    return data.isObj() && data.asObj()->isData() && data.asObj()->asData()->traits.size() == 0;
}

#define SYM(sym) symbols.intern({#sym, cidx(#sym, '\0')})
#define ARG(sym) asRecord(env)->lookup(SYM(sym))
#define ENV(...) Object::RecordOf(__VA_ARGS__)
#define FUN(sym, name, env, ...) rootRecord->def(SYM(sym), Object::Function(env, &(BUILTIN_##name = Builtin { SYM(sym), __VA_ARGS__ })));
#define FFUN(sym, name, ...) rootRecord->def(SYM(sym), Object::Function(Value::Nil(), &(BUILTIN_##name = Builtin { SYM(sym), __VA_ARGS__ }), Function::FLAG_FEXPR));
#define EVAL(...) [](Value env, Value self, Value args, bool isRecord) -> Value { SymbolTable& symbols = basilInstance->symbols; __VA_ARGS__ }
#define GEN(...) [](Value env, Value self, Value args, bool isRecord, BytecodeBuffer& buf) -> Value { SymbolTable& symbols = basilInstance->symbols; __VA_ARGS__ }

static Builtin
    BUILTIN_seq,
    BUILTIN_lambda,
    BUILTIN_record,
    BUILTIN_isint,
    BUILTIN_list;

Value createRoot() {
    Value root = Object::Record();
    Record* rootRecord = root.asObj()->asRecord();
    SymbolTable& symbols = basilInstance->symbols;

    rootRecord->def(SYM(nil), Value::Nil());

    FFUN(lambda, lambda,
        EVAL(
            Value lambdaArgs = car(args);
            Value body = cadr(args);
            Value lambdaEnv = eval(env, Object::List(Value::Sym(SYM(record)), lambdaArgs), false);
            eval(lambdaEnv, body, false); // Evaluate abstract body to infer constraints.
            return Object::Function(lambdaEnv, body);
        ),
        GEN(
            return Value::Nil();
        ));

    FFUN(record, record,
        EVAL(
            Value record = Object::Record(asRecord(env));
            while (!args.isNil()) {
                eval(record, car(args), true);
                args = cdr(args);
            }
            return record;
        ),
        GEN(
            return Value::Nil();
        ));
    
    FUN(int?, isint,
        ENV(SYM(x), Value::Nil()),
        EVAL(
            Value x = ARG(x);
            if (x.isInt()) return x;
            else if (x.isObj() && x.asObj()->isData()) {
                if (hasNoTraits(x) || anyTrueTrait([](Value trait) -> bool { return trait.isInt(); }, x))
                    return addTrait(x, self);
            }
            return Value::Nil();
        ),
        GEN(
            return Value::Nil();
        ));
        
    FFUN(list, list,
        EVAL(
            return evalList(env, args, isRecord);
        ),
        GEN(
            return Value::Nil();
        ));

    // rootRecord->def(SYM(lambda), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = car(originalArgs);
    //             Value body = cadr(originalArgs);
    //             Value lambdaEnv = eval(Value::Obj(env), Object::List(Value::Sym(SYM(record)), args), false);
    //             return Object::Function(lambdaEnv, body);
    //         )
    //     ));
        
    // rootRecord->def(SYM(seq),
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = originalArgs;
    //             Value last = Value::Nil();
    //             while (!args.isNil()) {
    //                 last = eval(Value::Obj(env), args.asObj()->asList()->hd, isRecord);
    //                 args = args.asObj()->asList()->tl;
    //             }
    //             return last;
    //         )
    //     ));

    // rootRecord->def(SYM(fun), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value name = car(originalArgs), args, body;
    //             if (name.isSym()) // Name before args.
    //                 args = cadr(originalArgs), body = caddr(originalArgs);
    //             else
    //                 args = cdr(name), name = car(name), body = cadr(originalArgs);
    //             Value funEnv = eval(Value::Obj(env), Object::List(Value::Sym(SYM(record)), args), isRecord);
    //             Value fun = Object::Function(funEnv, body);
    //             env->def(name.asSym(), fun);
    //             return fun;
    //         )
    //     ));
        
    // rootRecord->def(SYM(if), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = originalArgs;
    //             Value branches = cadr(originalArgs);
    //             if (car(branches).isSym() && car(branches).asSym() == SYM(seq))
    //                 branches = cdr(branches);
    //             Value cond = eval(Value::Obj(env), car(args), isRecord);
    //             if (!cond.isNil())
    //                 return eval(Value::Obj(env), car(branches), isRecord);
    //             else
    //                 return eval(Value::Obj(env), cadr(branches), isRecord);
    //         )
    //     ));
        
    // rootRecord->def(SYM(var), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = originalArgs;
    //             while (!args.isNil()) {
    //                 eval(Value::Obj(env), car(args), true);
    //                 args = cdr(args);
    //             }
    //             return Value::Nil();
    //         )
    //     ));
        
    // rootRecord->def(SYM(cons), 
    //     Object::Function(
    //         Object::RecordOf(
    //             SYM(head), Value::Nil(),
    //             SYM(tail), Value::Nil()),
    //         BUILTIN(
    //             return Object::List(ARG(head), ARG(tail));
    //         )
    //     ));
        
    // rootRecord->def(SYM(head), 
    //     Object::Function(
    //         Object::RecordOf(
    //             SYM(list), Value::Nil()),
    //         BUILTIN(
    //             return car(ARG(list));
    //         )
    //     ));
        
    // rootRecord->def(SYM(tail), 
    //     Object::Function(
    //         Object::RecordOf(
    //             SYM(list), Value::Nil()),
    //         BUILTIN(
    //             return cdr(ARG(list));
    //         )
    //     ));
        
    // rootRecord->def(SYM(int?), 
    //     Object::Function(
    //         Object::RecordOf(SYM(v), Value::Nil()),
    //         BUILTIN(
    //             Value arg = ARG(v);
    //             return arg.isInt() ? arg : Value::Nil();
    //         )
    //     ));
        
    // rootRecord->def(SYM(num?), 
    //     Object::Function(
    //         Object::RecordOf(SYM(v), Value::Nil()),
    //         BUILTIN(
    //             Value arg = ARG(v);
    //             return arg.isNum() ? arg : Value::Nil();
    //         )
    //     ));
        
    // rootRecord->def(SYM(null?), 
    //     Object::Function(
    //         Object::RecordOf(SYM(v), Value::Nil()),
    //         BUILTIN(
    //             Value arg = ARG(v);
    //             return arg.isNil() ? Value::Sym(SYM(true)) : Value::Nil();
    //         )
    //     ));
        
    // rootRecord->def(SYM(list?), 
    //     Object::Function(
    //         Object::RecordOf(SYM(v), Value::Nil()),
    //         BUILTIN(
    //             Value arg = ARG(v);
    //             return arg.isObj() && arg.asObj()->isList() ? arg : Value::Nil();
    //         )
    //     ));
        
    // rootRecord->def(SYM(record?), 
    //     Object::Function(
    //         Object::RecordOf(SYM(v), Value::Nil()),
    //         BUILTIN(
    //             Value arg = ARG(v);
    //             return arg.isObj() && arg.asObj()->isRecord() ? arg : Value::Nil();
    //         )
    //     ));
        
    // rootRecord->def(SYM(function?), 
    //     Object::Function(
    //         Object::RecordOf(SYM(v), Value::Nil()),
    //         BUILTIN(
    //             Value arg = ARG(v);
    //             return arg.isObj() && arg.asObj()->isFunction() ? arg : Value::Nil();
    //         )
    //     ));
        
    // rootRecord->def(SYM(=), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value var = eval(Value::Obj(env), car(originalArgs), isRecord);
    //             Value second = eval(Value::Obj(env), cadr(originalArgs), isRecord);
    //             if (isRecord && var.isNil()) {
    //                 var = second;
    //                 env->def(car(originalArgs).asSym(), second);
    //             }
    //             return var == second ? var : Value::Nil();
    //         )
    //     ));
        
    // rootRecord->def(SYM(+), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = originalArgs;
    //             Value sum = Value::Int(0);
    //             bool failed = false;
    //             while (!args.isNil()) {
    //                 Value item = eval(Value::Obj(env), car(args), isRecord);
    //                 if (item.isInt()) sum = Value::Int(sum.asInt() + item.asInt());
    //                 else failed = true;
    //                 args = cdr(args);
    //             }
    //             return failed ? Value::Nil() : sum;
    //         )
    //     ));
        
    // rootRecord->def(SYM(-), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = originalArgs;
    //             Value sum = eval(Value::Obj(env), car(args), isRecord);
    //             if (!sum.isInt())
    //                 return Value::Nil();
    //             args = cdr(args);
    //             if (args.isNil())
    //                 return Value::Int(-sum.asInt());
    //             bool failed = false;
    //             while (!args.isNil()) {
    //                 Value item = eval(Value::Obj(env), car(args), isRecord);
    //                 if (item.isInt()) sum = Value::Int(sum.asInt() - item.asInt());
    //                 else failed = true;
    //                 args = cdr(args);
    //             }
    //             return failed ? Value::Nil() : sum;
    //         )
    //     ));
        
    // rootRecord->def(SYM(*), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = originalArgs;
    //             Value prod = Value::Int(1);
    //             bool failed = false;
    //             while (!args.isNil()) {
    //                 Value item = eval(Value::Obj(env), car(args), isRecord);
    //                 if (item.isInt()) prod = Value::Int(prod.asInt() * item.asInt());
    //                 else failed = true;
    //                 args = cdr(args);
    //             }
    //             return failed ? Value::Nil() : prod;
    //         )
    //     ));
        
    // rootRecord->def(SYM(/), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = originalArgs;
    //             Value quot = eval(Value::Obj(env), car(args), isRecord);
    //             if (!quot.isInt())
    //                 return Value::Nil();
    //             args = cdr(args);
    //             if (args.isNil())
    //                 return Value::Int(1 / quot.asInt());
    //             bool failed = false;
    //             while (!args.isNil()) {
    //                 Value item = eval(Value::Obj(env), car(args), isRecord);
    //                 if (item.isInt()) quot = Value::Int(quot.asInt() / item.asInt());
    //                 else failed = true;
    //                 args = cdr(args);
    //             }
    //             return failed ? Value::Nil() : quot;
    //         )
    //     ));
        
    // rootRecord->def(SYM(%), 
    //     Object::Function(
    //         Value::Nil(),
    //         BUILTIN(
    //             Value args = originalArgs;
    //             Value quot = eval(Value::Obj(env), car(args), isRecord);
    //             if (!quot.isInt())
    //                 return Value::Nil();
    //             args = cdr(args);
    //             if (args.isNil())
    //                 return Value::Int(1 / quot.asInt());
    //             bool failed = false;
    //             while (!args.isNil()) {
    //                 Value item = eval(Value::Obj(env), car(args), isRecord);
    //                 if (item.isInt()) quot = Value::Int(quot.asInt() / item.asInt());
    //                 else failed = true;
    //                 args = cdr(args);
    //             }
    //             return failed ? Value::Nil() : quot;
    //         )
    //     ));
    
    return root;
}

Record* copyRecord(Record* rec) {
    return new Record(*rec);
}

Value apply(Function* function, Value env, Value args, bool isRecord);

bool isConsistent(Value target, Value env, Value value) {
    if (target.isObj() && target.asObj()->isData()) {
        Data* data = asData(target);
        // Returns if the provided value is consistent with the requirements of data.
        for (const Value& trait : data->traits) {
            if (trait.isObj() && trait.asObj()->isFunction()) {
                Value result = apply(trait.asObj()->asFunction(), env, Object::List(value, Value::Nil()), false);
                if (result.isNil())
                    return false;
            } 
            else if (!(value == trait))
                return false;
        }
        return true;
    }
    else return target.isNil() || target == value;
}

Value apply(Function* function, Value env, Value args, bool isRecord) {
    // If the function has a nil env, evaluate in the caller's environment.
    Record* rec = function->env.isNil() ? nullptr : copyRecord(function->env.asObj()->asRecord());
    
    Value list = args;
    if (!(function->flags & Function::FLAG_FEXPR)) for (Symbol s : rec->defList) {
        Value arg = list.isNil() ? list : list.asObj()->asList()->hd;
        Value existing = rec->lookup(s);
        if (!isConsistent(existing, env, arg)) {
            println("Argument ", arg, " is inconsistent with ", existing);
            return Value::Nil();
        }
        rec->table.put(s, arg); 
        if (!list.isNil())
            list = list.asObj()->asList()->tl;
    }

    if (function->builtin)
        return function->builtin->eval(rec ? Value::Obj(rec) : env, Value::Obj(function), args, isRecord);
    else
        return eval(Value::Obj(rec), function->body, isRecord);
}

Value eval(Value env, Value expr, bool isRecord) {
    if (expr.isInt() || expr.isNum() || expr.isNil()) 
        return expr;
    else if (expr.isSym()) {
        if (env.isObj() && env.asObj()->isRecord()) {
            auto def = env.asObj()->asRecord()->lookup(expr.asSym());
            if (isRecord && def.isNil()) {
                Value dataVal = Object::Data(asRecord(env), expr.asSym());
                asRecord(env)->def(expr.asSym(), dataVal);
                return dataVal;
            }
            return def;
        }
    }
    else if (expr.isObj() && expr.asObj()->isList()) {
        Value head = eval(env, expr.asObj()->asList()->hd, isRecord);
        if (head.isObj() && head.asObj()->isFunction()) {
            if (head.asObj()->asFunction()->flags & Function::FLAG_FEXPR)
                return apply(head.asObj()->asFunction(), env, expr.asObj()->asList()->tl, isRecord);
            else
                return apply(head.asObj()->asFunction(), env, evalList(env, cdr(expr), isRecord), isRecord);
        }
        evalError("Expected function value, got ", head);
    }

    evalError("Can't evaluate value ", expr);
    return Value::Nil();
}

ENDMODULE()