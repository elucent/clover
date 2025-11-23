#ifndef JASMINE_MODULE_H
#define JASMINE_MODULE_H

#include "jasmine/type.h"
#include "jasmine/ir.h"
#include "jasmine/data.h"
#include "jasmine/pass.h"

#ifdef RT_AMD64
#include "asm/arch/amd64.h"
#ifdef RT_LINUX
using DefaultTarget = AMD64LinuxAssembler;
#endif
#endif

namespace jasmine {
    struct Module {
        Symbol name;
        vec<Function*> functions;
        map<Symbol, u32> functionMap;
        PassContext passes;
        TypeContext types;
        SymbolTable syms;
        ValueTable values;
        ConstantTable constants;
        const TargetInterface* target;

        Module(const i8* moduleName);
        Module(const_slice<i8> moduleName);
        ~Module();

        Function& defineFunction(TypeIndex returnType, const i8* name, FunctionFlags flags);
        Function& defineFunction(TypeIndex returnType, const_slice<i8> name, FunctionFlags flags);
        void releaseFunction(Function& fn);

        inline const TypeContext& typeContext() const {
            return types;
        }

        inline TypeContext& typeContext() {
            return types;
        }

        inline const PassContext& passContext() const {
            return passes;
        }

        inline PassContext& passContext() {
            return passes;
        }

        inline TypeIndex arrayType(TypeIndex element, u32 length) {
            return types.arrayType(element, length);
        }

        inline TypeIndex vectorType(TypeIndex element, u32 length) {
            return types.vectorType(element, length);
        }

        template<typename... Args>
        inline TypeIndex structType(Args... args) {
            return types.structType(args...);
        }

        template<typename... Args>
        inline StructBuilder structBuilder(Args... args) const {
            return types.structBuilder(args...);
        }

        template<typename... Args>
        inline TypeIndex unionType(Args... args) {
            return types.unionType(args...);
        }

        template<typename... Args>
        inline UnionBuilder unionBuilder(Args... args) const {
            return types.unionBuilder(args...);
        }

        template<typename... Args>
        inline TypeIndex functionType(TypeIndex returnType, Args... args) {
            return types.functionType(returnType, args...);
        }

        template<typename... Args>
        inline FunctionBuilder functionBuilder(TypeIndex returnType, Args... args) const {
            return types.functionBuilder(returnType, args...);
        }
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Module& m) {
        for (const auto& f : m.functions)
            io = format(io, *f);
        // for (const auto& f : m.functions)
        //     io = format(io, " - ", f->name(), " : ", TypeLogger { m, f->functionType() }, '\n');
        return io;
    }
}

#endif