#ifndef CLOVER_INTERPRETER_H
#define CLOVER_INTERPRETER_H

#include "clover/ast.h"
#include "clover/type.h"
#include "util/rc.h"
#include "util/ptrhax.h"

namespace clover {
    struct IndexedProps;
    struct KeyedProps;

    struct Object {
        IndexedProps* indices;
        KeyedProps* keys;
    };

    struct Value {
        enum Kind : u8 {
            Int, Unsigned, Undefined, Float, Char, Bool, Obj, String
        };
        union {
            i64 i;
            u64 u;
            f64 f;
            bool b;
            u32 ch;
            void* ptr;
            TypeIndex t;
            u64 bits;
        };
        ptr_tuple<void*, Kind> kindAndPointer;

        inline Object getObj() const {
            return { (IndexedProps*)ptr, (KeyedProps*)kindAndPointer.getPtr() };
        }

        inline void setObj(Object obj) {
            if (indexedProps() != obj.indices) {
                asref<IndexedProps>(indexedProps()).deref();
                asref<IndexedProps>(obj.indices).ref();
                ptr = obj.indices;
            }
            if (keyedProps() != obj.keys) {
                asref<KeyedProps>(keyedProps()).deref();
                asref<KeyedProps>(obj.keys).ref();
                kindAndPointer.setPtr(obj.keys);
            }
        }

        inline const IndexedProps* indexedProps() const {
            return (const IndexedProps*)ptr;
        }

        inline IndexedProps* indexedProps() {
            return (IndexedProps*)ptr;
        }

        inline const KeyedProps* keyedProps() const {
            return (const KeyedProps*)kindAndPointer.getPtr();
        }

        inline KeyedProps* keyedProps() {
            return (KeyedProps*)kindAndPointer.getPtr();
        }

        inline void refObj() {
            asref<IndexedProps>(indexedProps()).ref();
            asref<KeyedProps>(keyedProps()).ref();
        }

        inline void derefObj() {
            asref<IndexedProps>(indexedProps()).deref();
            asref<KeyedProps>(keyedProps()).deref();
        }

        inline Value(): kindAndPointer(nullptr, Undefined) {
            bits = 0;
        }

        inline Value(Object object): kindAndPointer(object.keys, Obj) {
            ptr = object.indices;
            refObj();
        }

        inline Kind kind() const {
            return kindAndPointer.getExtra();
        }

        inline void setKind(Kind kind) {
            kindAndPointer.setExtra(kind);
        }

        inline bool isObj() const {
            return kind() >= Obj;
        }

        inline bool isAnyInt() const {
            return kind() <= Unsigned;
        }

        inline ~Value() {
            if (isObj())
                derefObj();
        }

        inline Value(const Value& other): kindAndPointer(other.kindAndPointer) {
            bits = other.bits;
            if (isObj())
                refObj();
        }

        inline Value& operator=(const Value& other) {
            if (&other != this) {
                if (isObj())
                    derefObj();
                kindAndPointer = other.kindAndPointer;
                bits = other.bits;
                if (isObj())
                    refObj();
            }
            return *this;
        }
    };

    struct IndexedProps {
        u32 length;
        bool isString;
        union {
            i8 bytes[0];
            Value values[0];
        };

        PREVENT_COPYING(IndexedProps);
        PREVENT_MOVING(IndexedProps);

        inline ~IndexedProps() {
            if (!isString) for (u32 i = 0; i < length; i ++)
                values[i].~Value();
        }
    };

    struct KeyedProps {
        map<Value, Value> fields;
    };

    inline Value boxInt(i64 i) {
        Value value;
        value.setKind(Value::Int);
        value.i = i;
        return value;
    }

    inline Value boxUnsigned(u64 u) {
        Value value;
        value.setKind(Value::Unsigned);
        value.u = u;
        return value;
    }

    inline Value boxFloat(f64 f) {
        Value value;
        value.setKind(Value::Float);
        value.f = f;
        return value;
    }

    inline Value boxBool(bool b) {
        Value value;
        value.setKind(Value::Bool);
        value.bits = 0;
        value.b = b;
        return value;
    }

    inline Value boxChar(rune r) {
        Value value;
        value.setKind(Value::Char);
        value.bits = 0;
        value.ch = r.get();
        return value;
    }

    inline IndexedProps* makeIndices(u32 length) {
        i8* ptr = new i8[sizeof(i64) + sizeof(IndexedProps) + sizeof(Value) * length];
        *(i64*)ptr = 0;
        IndexedProps* props = (IndexedProps*)(ptr + sizeof(i64));
        props->length = length;
        props->isString = false;
        return props;
    }

    inline IndexedProps* makeString(u32 length) {
        i8* ptr = new i8[sizeof(i64) + sizeof(IndexedProps) + length];
        *(i64*)ptr = 0;
        IndexedProps* props = (IndexedProps*)(ptr + sizeof(i64));
        props->length = length;
        props->isString = true;
        return props;
    }

    inline KeyedProps* makeKeys() {
        i8* ptr = new i8[sizeof(i64) + sizeof(KeyedProps)];
        *(i64*)ptr = 0;
        KeyedProps* props = (KeyedProps*)(ptr + sizeof(i64));
        return new (props) KeyedProps();
    }

    inline Value makeArray(const_slice<Value> values) {
        IndexedProps* indices = makeIndices(values.size());
        for (u32 i = 0; i < values.size(); i ++)
            indices->values[i] = values[i];
        return Value(Object { indices, nullptr });
    }

    inline Value makeArrayWithLength(u32 length) {
        IndexedProps* indices = makeIndices(length);
        for (u32 i = 0; i < length; i ++)
            indices->values[i] = Value();
        return Value(Object { indices, nullptr });
    }

    inline Value makeObject(const_slice<pair<Value, Value>> values) {
        KeyedProps* keys = makeKeys();
        for (auto pair : values)
            keys->fields.put(pair.first, pair.second);
        return Value(Object { nullptr, keys });
    }

    maybe<i64> getIndexInBounds(IndexedProps* props, Value idx) {
        if (idx.kind() == Value::Int && idx.i >= 0 && idx.i < props->length)
            return some<i64>(idx.i);
        if (idx.kind() == Value::Unsigned && idx.u < props->length)
            return some<i64>(idx.u);
        return none<i64>();
    }

    inline Value get(Value obj, Value key) {
        assert(obj.isObj());
        Object object = obj.getObj();
        if (object.indices && key.isAnyInt()) {
            if (auto idx = getIndexInBounds(object.indices, key)) {
                if (object.indices->isString)
                    return boxInt(object.indices->bytes[*idx]);
                else
                    return object.indices->values[*idx];
            }
        }
        if (object.keys) {
            auto it = object.keys->fields.find(key);
            if (it != object.keys->fields.end())
                return it->value;
        }
        return Value();
    }

    inline void set(Value obj, Value key, Value value) {
        assert(obj.isObj());
        Object object = obj.getObj();
        if (object.indices && key.isAnyInt()) {
            if (auto idx = getIndexInBounds(object.indices, key)) {
                if (object.indices->isString) {
                    if (value.kind() == Value::Int && fitsSigned<8>(value.i))
                        object.indices->bytes[*idx] = i8(value.i);
                    else if (value.kind() == Value::Unsigned && fitsUnsigned<8>(value.u))
                        object.indices->bytes[*idx] = u8(value.u);
                    else {
                        // Must be an out-of-bounds integer, or another type of
                        // value. Transition time!
                        IndexedProps* newProps = makeIndices(object.indices->length);
                        for (u32 i = 0; i < newProps->length; i ++)
                            newProps->values[i] = boxInt(object.indices->bytes[i]);
                        newProps->values[*idx] = value;
                        object.indices = newProps;
                        obj.setObj(object);
                    }
                } else
                    object.indices->values[*idx] = value;
                return;
            }
        }
        if (object.keys) {
            auto it = object.keys->fields.find(key);
            if (it != object.keys->fields.end()) {
                it->value = value;
                return;
            }
        }
    }

    inline bool operator==(const Value& a, const Value& b) {
        if (a.kind() != b.kind())
            return false;
        if (!a.isObj())
            return a.bits == b.bits;
        Object ao = a.getObj();
        Object bo = b.getObj();
        if (!!ao.indices != !!bo.indices)
            return false;
        if (!!ao.keys != !!bo.keys)
            return false;
        if (ao.indices) {
            if (ao.indices->length != bo.indices->length)
                return false;
            for (u32 i = 0; i < ao.indices->length; i ++) {
                Value ai = ao.indices->isString ? boxInt(ao.indices->bytes[i]) : ao.indices->values[i];
                Value bi = bo.indices->isString ? boxInt(bo.indices->bytes[i]) : bo.indices->values[i];
                if (!(ai == bi))
                    return false;
            }
        }
        if (ao.keys) {
            if (ao.keys->fields.size() != bo.keys->fields.size())
                return false;
            const auto& ak = ao.keys->fields, &bk = bo.keys->fields;
            for (const auto& [k, v] : ak) {
                auto it = bk.find(k);
                if (it == bk.end())
                    return false;
                if (!(v == it->value))
                    return false;
            }
        }
        return true;
    }

    inline u64 hash(const Value& v) {
        if (!v.isObj())
            return mixHash(intHash(v.kind()), intHash(v.bits));
        Object obj = v.getObj();
        auto indices = obj.indices;
        u64 ih = intHash(0);
        if (indices) {
            if (indices->isString)
                ih = raw_hash(indices->bytes, indices->length);
            else for (u32 i = 0; i < indices->length; i ++)
                ih = mixHash(ih, hash(indices->values[i]));
        }
        auto keys = obj.keys;
        u64 kh = intHash(0);
        if (keys) {
            for (const auto& [k, v] : keys->fields)
                kh = mixHash(kh, mixHash(hash(k), hash(v)));
        }
        return mixHash(ih, kh);
    }

    Value eval(Value env, AST expr);
    Value call(AST func, slice<Value> values);
}

#endif