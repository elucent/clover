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
    struct Namespace;
    struct GenericType;
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
            Variable, Constant, ConstFunction, Function, OverloadedFunction, Namespace, Type, TypeParameter, Member, GenericFunction, GenericType, Projection, Temp, Forward, ThisAccess, NumKinds
        };

        static constexpr const i8* KindNamesUpper[NumKinds] = {
            "Variable", "Constant", "Const Function", "Function", "Overloaded Function", "Namespace", "Type", "Type Parameter", "Member", "Generic Function", "Generic Type", "Projection", "Temp", "Forward", "ThisAccess"
        };

        static constexpr const i8* KindNamesLower[NumKinds] = {
            "variable", "constant", "const function", "function", "overloaded function", "namespace", "type", "type parameter", "member", "generic function", "generic type", "projection", "temp", "forward", "this access"
        };

        TypeIndex type : Limits::TypesPerCompilationBits;
        Scope scope : 3;
        Kind kind : 5;
        union {
            struct { u32 isImport : 1; NodeIndex decl : 31; Symbol name; };
            struct { u32 : 1; u32 functionIndex : 31; u32 : 32; };
            struct { u32 : 1; u32 constantIndex : 31; u32 : 32; };
            struct { u32 : 1; u32 namespaceIndex : 31; u32 : 32; };
            struct { u32 : 1; u32 genericTypeIndex : 31; u32 : 32; };
            struct { u32 : 1; u32 overloadsIndex : 31; u32 : 32; };
            struct { u32 : 1; ScopeIndex defScope : 31; u32 index; };
        };
    };

    using VariableKind = VariableInfo::Kind;

    struct Function;
    struct Overloads;

    struct Scope {
        enum class Kind : u8 {
            Function, Type, GenericType, Namespace, Block, TopLevel, Root
        };

        Module* module;
        Scope* parent;

        // This special sibling is only non-null for scopes associated with
        // generic function instantiations. In particular, this is the scope
        // which that function is allowed to look for method definitions in,
        // so that a generic function defined in one module can still include
        // the extensions made in another.
        Scope* instParent = nullptr;

        Function* function;
        bloom<u32, 1> inTable;
        map<Symbol, u32> entries;
        NodeIndex owner;
        ScopeIndex index, globalIndex;
        u32 ns = -1;
        Kind kind;
        bool hasInChain = false;
        bool isGenericTypeCase = false;

        // This is kind of important - this distinguishes a Scope as containing
        // nontrivial function/overload definitions, meaning it might change
        // the resolution of methods for any generics instantiated within. If
        // we do instantiate a generic function in this scope, we mangle its
        // name based on the chain of parent scopes - but starting from the
        // first parent for which definesFunctions = true.
        bool definesFunctions = false;

        Scope(Kind kind_in, Module* module_in, Function* function_in, Scope* parent_in, NodeIndex owner_in, ScopeIndex index_in);
        Scope(Kind kind_in, Module* module_in, Scope* parent_in, NodeIndex owner_in, ScopeIndex index_in);

        inline bool isGlobal() const {
            return function == nullptr;
        }

        inline bool isLocal() const {
            return function != nullptr;
        }

        void addToRoot(VariableKind kind, TypeIndex type, Symbol name);
        void add(VariableKind kind, const AST& decl, Symbol name);
        void add(VariableKind kind, const AST& decl, TypeIndex type, Symbol name);
        void addConstant(VariableKind kind, const AST& decl, Symbol name);
        void addImport(VariableKind kind, TypeIndex type, Symbol name);
        void addFunction(VariableKind kind, Function* function);
        void addOverloadedFunction(Overloads* overloads);
        void addIndirect(Module* module, const AST& import, Scope* defScope, u32 index, Symbol name);
        void addConstantIndirect(Module* module, const AST& import, Scope* defScope, u32 index, Symbol name);
        void addNamespace(const AST& decl, Symbol name, Namespace* ns);
        void addGenericType(const AST& decl, GenericType* type);

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

            const VariableInfo& info() const;
            VariableInfo& info();
        };

        FindResult find(Symbol name, bool searchParent = true);
        FindResult findMethodOnly(Symbol name, bool searchParent = true);

        inline FindResult findLocal(Symbol name) {
            return find(name, false);
        }
    };

    inline u64 hash(Scope* scope) {
        return ::hash(u64(scope));
    }

    using ScopeKind = Scope::Kind;

    Scope* getRootScope(Module* module);
    NOINLINE Artifact* computeScopes(Artifact* artifact);
    void computeScopes(Module* module, Scope* currentScope, AST ast);
    void setScopes(Module* module, Scope* scope, AST ast);
}

#endif