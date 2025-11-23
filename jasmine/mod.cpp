#include "jasmine/mod.h"
#include "jasmine/pass/target.h"

namespace jasmine {
    Module::Module(const i8* moduleName):
        Module(cstring(moduleName)) {}

    Module::Module(const_slice<i8> moduleName):
        values(this) {
        ensureInited();
        if (config::printJasmineAssembly) {
            using Target = Compose<Printer<DefaultTarget>, DefaultTarget>;
            Printer<DefaultTarget>::write_to(file::stdout);
            target = new TargetImplementation<Target>();
            passes.targetSpecificPasses = new TargetSpecificPasses<Target>(passes);
        } else {
            using Target = DefaultTarget;
            target = new TargetImplementation<Target>();
            passes.targetSpecificPasses = new TargetSpecificPasses<Target>(passes);
        }
        name = syms[moduleName];
    }

    Module::~Module() {
        for (Function* function : this->functions)
            delete function;
    }

    Function& Module::defineFunction(TypeIndex returnType, const i8* name, FunctionFlags flags) {
        return defineFunction(returnType, cstring(name), flags);
    }

    Function& Module::defineFunction(TypeIndex returnType, const_slice<i8> name, FunctionFlags flags) {
        Symbol sym = syms[name];
        functionMap.put(sym, functions.size());
        functions.push(new Function(*this, sym, returnType, flags));
        functions.back()->indexInModule = functions.size() - 1;
        return *functions.back();
    }

    void Module::releaseFunction(Function& fn) {
        u32 idx = functionMap[fn.sym];
        delete functions[idx];
        functions[idx] = nullptr;
    }
}