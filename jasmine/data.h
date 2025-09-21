#ifndef JASMINE_DATA_H
#define JASMINE_DATA_H

#include "rt/def.h"
#include "jasmine/const.h"
#include "jasmine/type.h"

namespace jasmine {
    using ValueIndex = i32;

    struct Value {
        enum Kind : u32 {
            I8, I16, I32, I64,
            U8, U16, U32, U64,
            Ptr, F32, F64, Func,
            Static, Data, Array, Struct
        };

        union {
            struct { Kind kind : 4; u32 isInline : 1; i32 payload : 27; };
            struct { u32 : 4; ValueIndex ref : 28; };
            struct { u32 : 4; Symbol sym : 28; };
            struct { u32 : 4; u32 isSpecialized : 1; TypeIndex type : 27; };
            u32 bits;
        };

        inline bool isRef() const {
            return kind == Array || kind == Struct;
        }

        inline i64 intValue(ConstantTable* constants) const {
            return isInline ? payload : (*constants)[payload].i;
        }

        inline f32 f32Value(ConstantTable* constants) const {
            return isInline ? bitcast<f32>(u32(payload) << 5) : (*constants)[payload].f;
        }

        inline f64 f64Value(ConstantTable* constants) const {
            return isInline ? bitcast<f64>(u64(payload) << 37) : (*constants)[payload].d;
        }
    };

    static_assert(sizeof(Value) == 4);

    struct ValueTable {
        Module* mod;
        ConstantTable* constants;
        vec<Value, 32> words;
        vec<u32, 16> values;
        map<Symbol, TypeIndex> staticUninitDefs;
        map<Symbol, Value> staticDefs, dataDefs;

        ValueTable(Module* mod_in);

        template<typename T>
        inline Value makeInt(TypeKind type, T t) const {
            Value word;
            if (type == PTR || type == REF)
                word.kind = Value::Kind(~PTR);
            else
                word.kind = Value::Kind(~type);
            if (fitsSigned<27>(i64(t))) {
                word.isInline = 1;
                word.payload = i32(i64(t));
            } else {
                auto idx = constants->intern(Constant::Int(i64(t)));
                word.isInline = 0;
                word.payload = idx;
            }
            return word;
        }

        inline Value makeF32(f32 f) const {
            u32 asInt = bitcast<u32>(f);
            Value word;
            word.kind = Value::F32;
            if (!(asInt & 0x1f)) {
                word.isInline = 1;
                word.payload = i32(asInt >> 5);
            } else {
                auto idx = constants->intern(Constant::F32(f));
                word.isInline = 0;
                word.payload = idx;
            }
            return word;
        }

        inline Value makeF64(f64 f) const {
            u64 asInt = bitcast<u64>(f);
            Value word;
            word.kind = Value::F64;
            if (!(asInt & 0x1fffffffffull)) {
                word.isInline = 1;
                word.payload = i32(asInt >> 37);
            } else {
                auto idx = constants->intern(Constant::F64(f));
                word.isInline = 0;
                word.payload = idx;
            }
            return word;
        }

        inline Value makeFuncref(Symbol sym) const {
            Value word;
            word.kind = Value::Func;
            word.sym = sym;
            return word;
        }

        inline Value makeDataref(Symbol sym) const {
            Value word;
            word.kind = Value::Data;
            word.sym = sym;
            return word;
        }

        inline Value makeStaticref(Symbol sym) const {
            Value word;
            word.kind = Value::Static;
            word.sym = sym;
            return word;
        }

        void defStatic(Symbol sym, Value word);
        void defStaticUninit(Symbol sym, TypeIndex type);
        void defData(Symbol sym, Value word);

        Value makeI8Array(TypeIndex type, const_slice<i8> data);
        Value makeU8Array(TypeIndex type, const_slice<u8> data);
        Value makeI16Array(TypeIndex type, const_slice<i16> data);
        Value makeU16Array(TypeIndex type, const_slice<u16> data);
        Value makeI32Array(TypeIndex type, const_slice<i32> data);
        Value makeU32Array(TypeIndex type, const_slice<u32> data);
        Value makeI64Array(TypeIndex type, const_slice<i64> data);
        Value makeU64Array(TypeIndex type, const_slice<u64> data);
        Value makeF32Array(TypeIndex type, const_slice<f32> data);
        Value makeF64Array(TypeIndex type, const_slice<f64> data);
        Value makeValueArray(TypeIndex type, const_slice<Value> data);

        Value makeStruct(TypeIndex type, const_slice<Value> data);

    private:
        template<typename T>
        Value makeArray(TypeIndex type, const_slice<T> data);
    };
}

#endif