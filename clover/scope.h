#ifndef CLOVER_SCOPE_H
#define CLOVER_SCOPE_H

#include "clover/limits.h"
#include "clover/type.h"
#include "clover/value.h"
#include "util/bloom.h"
#include "util/maybe.h"

namespace clover {
    using UniqueId = u32;

    struct Module;
    struct Function;
    struct AST;

    struct Variable {
        u32 index;

        inline Variable(u32 index_in):
            index(index_in) {}
    };

    struct Local : public Variable {
        inline Local(u32 index): Variable(index) {}
    };

    struct Global : public Variable {
        inline Global(u32 index): Variable(index) {}
    };

    //
    // Variable definitions are organized somewhat trickily in the Clover
    // compiler. Let's walk through how they work from the start:
    //
    //  - During lexing, names in the source are interned into "identifiers".
    //    Identifiers are 24-bit integer IDs assigned to unique strings. All
    //    symbols with the same string will be assigned the same identifier.
    //
    //  - During parsing, we build the AST using identifiers for any kind of
    //    variable use. At this point, we're still using the text symbol as
    //    the source of identity.
    //
    //  - When we compute scopes, we build tables mapping from symbols to new
    //    unique IDs. These IDs are either local or global. Local IDs are
    //    local to a specific function and all its non-function child scopes.
    //    Global IDs are assigned to variables outside of any function
    //    definition (so, top-level, plus any child type or namespace scopes),
    //    and are assigned on a per-module basis. In cases where we have nested
    //    scopes that aren't distinguished by local/global (i.e. if we have a
    //    nested function definition) variables from the enclosing function
    //    must be explicitly added to the inner function's numbering system -
    //    think of this like closing over the external definitions.
    //
    //  - Once definitions have been processed and the scope tree has been
    //    built, names go through a resolution pass. Here, we destructively
    //    replace identifier nodes, not only by reducing them to their local/
    //    global numbering, but also diversifying them based on the type of
    //    definition they correspond to. Names that refer to values become
    //    variables, while names that refer to types become typenames. Each
    //    of these name kinds has a local and a global variant, since they can
    //    be defined under either a local or global scope.
    //

    struct VariableInfo {
        enum Scope : u32 {
            Local, Global, Capture, NumScopes
        };

        static constexpr const i8* ScopeNames[NumScopes] = {
            "Local", "Global", "Captured"
        };

        enum Kind : u32 {
            Variable, Constant, ConstFunction, Function, OverloadedFunction, Type, Member, GenericFunction, GenericType, Temp, Forward, NumKinds
        };

        static constexpr const i8* KindNamesUpper[NumKinds] = {
            "Variable", "Constant", "Const Function", "Function", "Overloaded Function", "Type", "Member", "Generic Function", "Generic Type"
        };

        static constexpr const i8* KindNamesLower[NumKinds] = {
            "variable", "constant", "const function", "function", "overloaded function", "type", "member", "generic function", "generic type"
        };

        TypeIndex type : Limits::TypesPerCompilationBits;
        Scope scope : 3;
        Kind kind : 5;
        union {
            struct { u32 isImport : 1; NodeIndex decl : 31; Symbol name; };
            struct { u32 : 1; u32 functionIndex : 31; u32 : 32; };
            struct { u32 : 1; u32 constantIndex : 31; u32 : 32; };
            struct { u32 overloads; u32 : 32; };
            struct { ScopeIndex defScope; u32 index; };
        };
    };

    using VariableKind = VariableInfo::Kind;

    struct Function;
    struct Overloads;

    struct Scope {
        enum class Kind : u8 {
            Function, Type, Module, Block, TopLevel, Root
        };

        Module* module;
        Scope* parent;
        Function* function;
        bloom<u32> inChain, inTable;
        map<Symbol, u32> entries;
        NodeIndex owner;
        ScopeIndex index;
        Kind kind;
        bool hasInChain = false;

        inline Scope(Kind kind_in, Module* module_in, Function* function_in, Scope* parent_in, NodeIndex owner_in, ScopeIndex index_in):
            module(module_in), parent(parent_in), function(function_in), owner(owner_in), index(index_in), kind(kind_in) {}

        inline Scope(Kind kind_in, Module* module_in, Scope* parent_in, NodeIndex owner_in, ScopeIndex index_in):
            module(module_in), parent(parent_in), function(nullptr), owner(owner_in), index(index_in), kind(kind_in) {
            if (parent) {
                Scope* p = parent;
                while (p->kind != Kind::Root) {
                    if (p->kind == Kind::Function) {
                        function = p->function;
                        break;
                    }
                    p = p->parent;
                }
            }
        }

        inline bool isGlobal() const {
            return function == nullptr;
        }

        inline bool isLocal() const {
            return function != nullptr;
        }

        void addToRoot(VariableKind kind, TypeIndex type, Symbol name);
        void add(VariableKind kind, const AST& decl, Symbol name);
        void add(VariableKind kind, const AST& decl, TypeIndex type, Symbol name);
        void addConstant(VariableKind kind, const AST& decl, Symbol name, Value value);
        void addImport(VariableKind kind, TypeIndex type, Symbol name);
        void addFunctionImport(VariableKind kind, Function* function);
        void addOverloadedFunction(Overloads* overloads, Symbol name);
        void addIndirect(Module* module, const AST& import, Scope* defScope, u32 index, Symbol name);

        struct FindResult {
            Scope* scope;
            u32 index;
            bool g, isNone;

            inline FindResult(Scope* scope_in, u32 index_in, bool isGlobal_in):
                scope(scope_in), index(index_in), g(isGlobal_in), isNone(false) {}

            inline FindResult(): isNone(true) {}

            inline operator bool() const {
                return !isNone;
            }

            inline Global global() const {
                assert(isGlobal());
                return Global(index);
            }

            inline Local local() const {
                assert(!isGlobal());
                return Local(index);
            }

            inline bool isGlobal() const {
                return g;
            }
        };

        inline FindResult find(Symbol name, bool searchParent = true) {
            // TODO: Re-enable in-chain computation in the presence of late definitions.
            // if (hasInChain && !inChain.mayContain(name.symbol))
            //     return {};
            if (inTable.mayContain(name.symbol)) {
                auto it = entries.find(name);
                if (it != entries.end())
                    return FindResult(this, it->value, !function);
            }
            if (parent && searchParent)
                return parent->find(name);
            return {};
        }

        inline FindResult findLocal(Symbol name) {
            return find(name, false);
        }

        void computeInChain();
    };

    using ScopeKind = Scope::Kind;

    Scope* getRootScope(Module* module);
    NOINLINE Artifact* computeScopes(Artifact* artifact);
    void computeScopes(Module* module, Scope* currentScope, AST ast);
}

#endif