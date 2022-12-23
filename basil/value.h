#ifndef BASIL_VALUE_H
#define BASIL_VALUE_H

#include "core/def.h"
#include "lib/malloc.h"
#include "lib/slice.h"
#include "lib/vec.h"
#include "lib/buffer.h"
#include "lib/hash.h"
#include "basil/lex.h"
#include "basil/basil.h"

MODULE(basil)

struct Object;

struct Value {
    enum Tag {
        TAG_OBJ = 0b000,
        TAG_INT = 0b001,
        TAG_NUM = 0b010,
        TAG_SYM = 0b011
    };

    union {
        Object* object;
        i64 integer;
        double number;
        i64 symbol;
    };

    inline static Value Nil() {
        Value v;
        v.object = nullptr;
        return v;
    }

    inline bool isNil() const {
        return object == nullptr;
    }

    inline static Value Obj(Object* object) {
        Value v;
        v.object = object;
        return v;
    }

    inline bool isObj() const {
        return object && (integer & 0b111) == TAG_OBJ;
    }

    inline Object* asObj() const {
        return object;
    }

    inline static Value Int(i64 integer) {
        Value v;
        v.integer = integer << 3;
        v.integer |= TAG_INT;
        return v;
    }

    inline bool isInt() const {
        return (integer & 0b111) == TAG_INT;
    }

    inline i64 asInt() const {
        return integer >> 3;
    }

    inline static Value Num(double number) {
        Value v;
        v.number = number;
        v.integer &= 0xfffffffffffffff8;
        v.integer |= TAG_NUM;
        return v;
    }

    inline i64 isNum() const {
        return (integer & 0b111) == TAG_NUM;
    }

    inline double asNum() const {
        i64 i = integer;
        i &= 0xfffffffffffffff8;
        return *(double*)&i;
    }

    inline static Value Sym(Symbol symbol) {
        Value v;
        v.symbol = i64(symbol) << 3;
        v.symbol |= TAG_SYM;
        return v;
    }

    inline i64 isSym() const {
        return (integer & 0b111) == TAG_SYM;
    }

    inline Symbol asSym() const {
        return symbol >> 3;
    }

    inline bool operator==(const Value& other) const {
        if (isInt() && other.isInt()) return asInt() == other.asInt();
        else if (isNum() && other.isNum()) return asNum() == other.asNum();
        else if (isSym() && other.isSym()) return asSym() == other.asSym();
        else if (isNil() && other.isNil()) return true;
        else if (isObj() && other.isObj()) return asObj() == other.asObj();
        else return false;
    }
};

struct Data;
struct List;
struct Record;
struct Function;

using BytecodeBuffer = bytebuf<gc_allocator>;

struct Builtin {
    Symbol name;
    Value (*eval)(Value, Value, Value, bool);
    Value (*gen)(Value, Value, Value, bool, BytecodeBuffer&);
};

struct Object {
    enum Tag {
        TAG_DATA = 0b000,
        TAG_LIST = 0b001,
        TAG_RECORD = 0b010,
        TAG_FUNCTION = 0b011
    };

    Tag tag;

    inline Object(Tag tag_in): tag(tag_in) {}

    inline static Value Data(basil::Record* env, Symbol name);
    inline static Value List(Value hd, Value tl);
    inline static Value Record();
    inline static Value Record(basil::Record* parent_in);
    inline static Value Function(Value env, Value body, i32 flags);
    inline static Value Function(Value env, Builtin* builtin, i32 flags);

    inline static Value ListOf(const vec<Value>& values) {
        Value list = Value::Nil();
        for (i32 i = i32(values.size()) - 1; i >= 0; i --)
            list = Object::List(values[i], list);
        return list;
    }

    inline static Value ListOf() {
        return Value::Nil();
    }

    template<typename... Args>
    inline static Value ListOf(Value front, const Args&... values) {
        return Object::List(front, Object::ListOf(values...));
    }

    inline static Value RecordOf(basil::Record* record) {
        return Value::Obj((Object*)record);
    }

    template<typename... Args>
    inline static Value RecordOf(basil::Record* record, Symbol sym, Value val, const Args&... values);

    template<typename... Args>
    inline static Value RecordOf(basil::Record* record, Value sym, Value val, const Args&... values);

    template<typename... Args>
    inline static Value RecordOf(basil::Record* record, pair<Symbol, Value> entry, const Args&... values);

    template<typename... Args>
    inline static Value RecordOf(basil::Record* record, pair<Value, Value> entry, const Args&... values);

    template<typename... Args>
    inline static Value RecordOf(const Args&... values);

    inline bool isData() const {
        return tag == TAG_DATA;
    }

    inline basil::Data* asData() const {
        return (basil::Data*)this;
    }

    inline bool isList() const {
        return tag == TAG_LIST;
    }

    inline basil::List* asList() const {
        return (basil::List*)this;
    }

    inline bool isRecord() const {
        return tag == TAG_RECORD;
    }

    inline basil::Record* asRecord() const {
        return (basil::Record*)this;
    }

    inline bool isFunction() const {
        return tag == TAG_FUNCTION;
    }

    inline basil::Function* asFunction() const {
        return (basil::Function*)this;
    }
};

struct List : public Object {
    Value hd, tl;

    inline List(Value hd_in, Value tl_in): 
        Object(TAG_LIST), hd(hd_in), tl(tl_in) {}
};

struct Record : public Object {
    Record* parent;
    vec<Symbol> defList;
    map<Symbol, Value> table;

    inline Record(): Object(TAG_RECORD), parent(nullptr) {}
    inline Record(Record* parent_in): Object(TAG_RECORD), parent(parent_in) {}

    inline Value lookup(Symbol sym) {
        auto it = table.find(sym);
        if (it != table.end()) return it->value;
        else if (parent) return parent->lookup(sym);
        else return Value::Nil();
    }

    inline void def(Symbol sym, Value value) {
        table.put(sym, value);
        defList.push(sym);
    }
};

struct Data : public Object {
    basil::Record* env;
    Symbol name;
    vec<Value> traits;

    inline Data(basil::Record* env_in, Symbol name_in): 
        Object(TAG_DATA), env(env_in), name(name_in) {}
};

struct Function : public Object {
    enum FunctionFlags {
        NO_FLAGS = 0,
        FLAG_FEXPR = 1, // Does not evaluate its argument list.
    };

    Value env, body;
    Builtin* builtin;
    i32 flags;

    inline Function(Value env_in, Builtin* builtin_in, i32 flags_in): 
        Object(TAG_FUNCTION), env(env_in), body(Value::Nil()), builtin(builtin_in), flags(flags_in) {}
    inline Function(Value env_in, Value body_in, i32 flags_in):
        Object(TAG_FUNCTION), env(env_in), body(body_in), builtin(nullptr), flags(flags_in) {}
};

Value Object::Data(basil::Record* env, Symbol name) {
    return Value::Obj(new basil::Data(env, name));
}

Value Object::List(Value hd, Value tl) {
    return Value::Obj(new basil::List(hd, tl));
}

Value Object::Record() {
    return Value::Obj(new basil::Record());
}

Value Object::Record(basil::Record* parent) {
    return Value::Obj(new basil::Record(parent));
}

Value Object::Function(Value env, Value body, i32 flags = Function::NO_FLAGS) {
    return Value::Obj(new basil::Function(env, body, flags));
}

Value Object::Function(Value env, Builtin* builtin, i32 flags = Function::NO_FLAGS) {
    return Value::Obj(new basil::Function(env, builtin, flags));
}

template<typename... Args>
inline Value Object::RecordOf(const Args&... values) {
    basil::Record* rec = new basil::Record();
    return RecordOf(rec, values...);
}

template<typename... Args>
inline Value Object::RecordOf(basil::Record* rec, Symbol sym, Value val, const Args&... values) {
    rec->def(sym, val);
    return RecordOf(rec, values...);
}

template<typename... Args>
inline Value Object::RecordOf(basil::Record* rec, Value sym, Value val, const Args&... values) {
    rec->def(sym.asSym(), val);
    return RecordOf(rec, values...);
}

template<typename... Args>
inline Value Object::RecordOf(basil::Record* rec, pair<Symbol, Value> entry, const Args&... values) {
    rec->def(entry.first, entry.second);
    return RecordOf(rec, values...);
}

template<typename... Args>
inline Value Object::RecordOf(basil::Record* rec, pair<Value, Value> entry, const Args&... values) {
    rec->def(entry.first.asSym(), entry.second);
    return RecordOf(rec, values...);
}

void replRead(vec<Token>& tokens);

struct TokenView {
    constexpr static Token NONE = Token{TK_NONE, 0, 0, 0, 0};
    
    vec<i32> indents;
    vec<Token>& tokens;
    i32 i, depth = 0;

    inline const Token& peek() const {
        if (i >= tokens.size()) return NONE;
        else return tokens[i];
    }

    inline const Token& read() {
        const Token& result = peek();
        if (i < tokens.size()) i ++;
        return result;
    }

    inline operator bool() const {
        if (Options::mode == Options::REPL && i >= tokens.size()) {
            print(". ");
            flush(file_stdout);
            replRead(tokens);
        }
        return i < tokens.size();
    }
};

Value parse(vec<Token>& tokens);
Value parseRepl(TokenView& tokens);
void write_impl(fd io, Value value);

Value createRoot();
Value eval(Value env, Value expr, bool isRecord);

ENDMODULE()

#endif