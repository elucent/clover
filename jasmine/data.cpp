#include "jasmine/data.h"
#include "jasmine/mod.h"

namespace jasmine {
    ValueTable::ValueTable(Module* mod_in):
        mod(mod_in), constants(&mod->constants) {}

    void ValueTable::defStatic(Symbol sym, Value word) {
        staticDefs.put(sym, word);
    }

    void ValueTable::defStaticUninit(Symbol sym, TypeIndex type) {
        staticUninitDefs.put(sym, type);
    }

    void ValueTable::defData(Symbol sym, Value word) {
        dataDefs.put(sym, word);
    }

    template<typename T>
    constexpr bool isSpecialized() { return true; }

    template<>
    constexpr bool isSpecialized<Value>() { return false; }

    template<typename T>
    Value ValueTable::makeArray(TypeIndex type, const_slice<T> data) {
        const auto& arrayType = mod->typeContext()[type];

        u32 result = words.size();
        Value header;
        header.kind = Value::Array;
        header.isSpecialized = isSpecialized<T>();
        header.type = type;
        words.push(header);

        u32 numWords;
        if (sizeof(T) <= 4)
            numWords = divideRoundingUp<u32>(arrayType.length(), 4 / sizeof(T));
        else
            numWords = arrayType.length() * 2;
        for (u32 i = 0; i < numWords; i ++)
            words.push({ .bits = 0 });
        memory::copy(words.end() - numWords, data.data(), data.size() * sizeof(T));

        Value ref;
        ref.kind = header.kind;
        ref.ref = result;
        return ref;
    }

    Value ValueTable::makeI8Array(TypeIndex type, const_slice<i8> data) {
        assert(mod->typeContext()[type].elementType() == I8);
        return makeArray<i8>(type, data);
    }

    Value ValueTable::makeI16Array(TypeIndex type, const_slice<i16> data) {
        assert(mod->typeContext()[type].elementType() == I16);
        return makeArray<i16>(type, data);
    }

    Value ValueTable::makeI32Array(TypeIndex type, const_slice<i32> data) {
        assert(mod->typeContext()[type].elementType() == I32);
        return makeArray<i32>(type, data);
    }

    Value ValueTable::makeI64Array(TypeIndex type, const_slice<i64> data) {
        assert(mod->typeContext()[type].elementType() == I64);
        return makeArray<i64>(type, data);
    }

    Value ValueTable::makeF32Array(TypeIndex type, const_slice<f32> data) {
        assert(mod->typeContext()[type].elementType() == F32);
        return makeArray<f32>(type, data);
    }

    Value ValueTable::makeF64Array(TypeIndex type, const_slice<f64> data) {
        assert(mod->typeContext()[type].elementType() == F64);
        return makeArray<f64>(type, data);
    }

    Value ValueTable::makeValueArray(TypeIndex type, const_slice<Value> data) {
        return makeArray<Value>(type, data);
    }

    Value ValueTable::makeStruct(TypeIndex type, const_slice<Value> data) {
        const auto& structType = mod->typeContext()[type];
        u32 numWords = structType.fieldCount;
        u32 result = words.size();
        Value firstWord;
        firstWord.kind = Value::Struct;
        firstWord.isSpecialized = 0;
        firstWord.type = type;
        words.push(firstWord);
        for (u32 i = 0; i < numWords; i ++)
            words.push(data[i]);

        Value ref;
        ref.kind = Value::Struct;
        ref.ref = result;
        return ref;
    }

    Value ValueTable::makeUnion(TypeIndex type, Value member) {
        u32 result = words.size();
        Value firstWord;
        firstWord.kind = Value::Union;
        firstWord.isSpecialized = 0;
        firstWord.type = type;
        words.push(firstWord);
        words.push(member);

        Value ref;
        ref.kind = Value::Union;
        ref.ref = result;
        return ref;
    }
}