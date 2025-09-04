#ifndef CLOVER_COMPILATION_H
#define CLOVER_COMPILATION_H

#include "util/sym.h"
#include "util/hash.h"
#include "util/path.h"
#include "clover/jitruntime.h"
#include "clover/limits.h"

namespace clover {
    struct TypeSystem;
    struct Error;
    struct Compilation;
    struct Scope;

    enum InvalidTag {
        Invalid
    };

    struct Symbol {
        u32 symbol;

        inline Symbol() = default;

        inline Symbol(u32 symbol_in):
            symbol(symbol_in) {}

        inline Symbol(InvalidTag):
            symbol(Limits::InvalidSymbol) {}

        inline explicit operator bool() const {
            return symbol >= 0;
        }

        inline bool operator==(Symbol other) const {
            return symbol == other.symbol;
        }

        inline bool operator!=(Symbol other) const {
            return symbol != other.symbol;
        }
    };

    inline u64 hash(const Symbol& symbol) {
        return ::hash(symbol.symbol);
    }

    #define FOR_EACH_ARTIFACT_KIND(macro) \
        macro(None) \
        macro(Source) \
        macro(Tokens) \
        macro(ParsedAST) \
        macro(ScopedAST) \
        macro(ResolvedAST) \
        macro(CheckedAST) \
        macro(AnalyzedAST) \
        macro(FinalizedAST) \
        macro(JasmineIR) \
        macro(Assembly) \
        macro(DataFile) \
        macro(DynamicLibrary)

    enum class ArtifactKind : u8 {
        #define DEFINE_ENUM(name) name,
        FOR_EACH_ARTIFACT_KIND(DEFINE_ENUM)
        #undef DEFINE_ENUM
        NumArtifactKinds
    };

    extern const i8* ArtifactKindNames[(u32)ArtifactKind::NumArtifactKinds];

    struct ArtifactData {
        Error* errors = nullptr;

        virtual ~ArtifactData() = 0;
        virtual void print(Compilation* compilation) = 0;
        virtual u64 reportSize() = 0;
        virtual maybe<const_slice<i8>> getSource() = 0;
        virtual maybe<const_slice<u32>> getLineOffsets() = 0;

        inline bool hasErrors() const {
            return errors;
        }

        void addError(Error* error);
        void takeErrors(vec<Error*>& collector);
    };

    struct Artifact;

    struct Package {
        Compilation* compilation;
        Package* parent;
        Symbol name;
        map<Symbol, Package*> subpackages;
        vec<Artifact*> artifacts;
    };

    struct Directory;
    struct Source;
    struct Tokens;
    struct Module;
    struct JasmineArtifact;
    struct AssemblyArtifact;

    struct Artifact {
        Directory* parent;
        Symbol name;
        const_slice<i8> filename;
        ArtifactKind kind;
        ArtifactData* data;

        inline Artifact(Directory* parent_in, Symbol name_in):
            parent(parent_in), name(name_in), filename({ nullptr, 0 }), kind(ArtifactKind::None), data(nullptr) {}

        ~Artifact() {
            if (data)
                delete data;
            if (filename.data())
                delete[] filename.data();
        }

        template<typename T>
        inline T* as() {
            return (T*)data;
        }

        template<typename T>
        inline const T* as() const {
            return (const T*)data;
        }

        inline void update(Source* source) { update(ArtifactKind::Source, (ArtifactData*)source, true); }
        inline void update(Tokens* tokens) { update(ArtifactKind::Tokens, (ArtifactData*)tokens, true); }
        inline void update(ArtifactKind kind, Module* module) { update(kind, (ArtifactData*)module, kind == ArtifactKind::ParsedAST); }
        inline void update(JasmineArtifact* jasmine) { update(ArtifactKind::JasmineIR, (ArtifactData*)jasmine, true); }
        inline void update(AssemblyArtifact* assembly) { update(ArtifactKind::Assembly, (ArtifactData*)assembly, true); }

        void print();

        inline bool hasErrors() const { return data->hasErrors(); }
        inline void addError(Error* error) { data->addError(error); }
        inline void takeErrors(vec<Error*>& collector) { data->takeErrors(collector); }

    private:
        inline void update(ArtifactKind kind_in, ArtifactData* data_in, bool doDelete) {
            if (data && doDelete)
                delete data;
            kind = kind_in;
            data = data_in;
        }
    };

    struct Directory {
        Compilation* compilation;
        Directory* parent;
        Symbol name;
        vec<Directory*> directoryList;
        vec<Artifact*> artifactList;
        map<Symbol, i32> mapping;

        inline Directory(Compilation* compilation_in, Directory* parent_in, Symbol name_in):
            compilation(compilation_in), parent(parent_in), name(name_in) {}

        inline ~Directory() {
            for (Directory* directory : directoryList)
                delete directory;
            for (Artifact* artifact : artifactList)
                delete artifact;
        }

        PREVENT_COPYING(Directory);
        PREVENT_MOVING(Directory);

        void addArtifact(Symbol name, Artifact* artifact);
        void addSubDirectory(Symbol name, Directory* directory);
        Artifact* artifactByName(Symbol name);
        Directory* directoryByName(Symbol name);
        Directory* ensureDirectoryByName(Symbol name);
    };

    struct Compilation {
        SymbolTable symbols;
        Directory* root;
        TypeSystem* types;
        Module* rootModule;
        Scope* rootScope;
        JITRuntimeShims* shims = nullptr;
        const_slice<i8> cwd;
        bool compilationErrored = false;
        bool filesChanged = false;
        u32 optimizationLevel = 0;
        vec<Symbol> toplevels;

        Compilation();
        ~Compilation();

        PREVENT_COPYING(Compilation);
        PREVENT_MOVING(Compilation);

        inline const_slice<i8> str(clover::Symbol sym) const {
            return symbols[sym.symbol];
        }

        template<typename StringLike>
        inline Symbol sym(StringLike str) {
            return symbols[str];
        }

        struct IndentedItem {
            void* obj;
            u32 indent;
            bool isDir;
        };

        template<typename IO, typename Format = Formatter<IO>>
        void printProjectTree(IO io) const {
            vec<IndentedItem, 16> dfs;
            vec<Directory*, 16> path;
            auto indent = [&](u32 n) {
                for (u32 i = 0; i < n; i ++)
                    io = format(io, "  ");
            };
            dfs.push({ root, 0, true });
            while (dfs.size()) {
                auto dir = dfs.pop();
                path.clear();
                Directory* iter = dir.isDir ? (Directory*)dir.obj : ((Artifact*)dir.obj)->parent;
                while (iter && iter != root)
                    path.push(iter), iter = iter->parent;
                indent(dir.indent);
                for (i64 i = i64(path.size()) - 1; i >= 0; i --) {
                    if (i != i64(path.size()) - 1)
                        io = format(io, '.');
                    io = format(io, str(path[i]->name));
                }
                if (dir.isDir) {
                    io = format(io, "/\n");
                    for (Directory* sub : ((Directory*)dir.obj)->directoryList)
                        dfs.push({ sub, dir.indent + 1, true });
                    for (Artifact* item : ((Directory*)dir.obj)->artifactList)
                        dfs.push({ item, dir.indent + 1, false });
                } else {
                    if (path.size())
                        io = format(io, '.');
                    io = format(io, str(((Artifact*)dir.obj)->name), '\n');
                }
            }
        }

        template<typename Func>
        inline void forEachDirectoryIn(Directory*& directory, const Func& func) {
            for (Directory*& subdir : directory->directoryList)
                forEachDirectoryIn(subdir, func);
            func(directory);
        }

        template<typename Func>
        inline void forEachDirectory(const Func& func) {
            forEachDirectoryIn(root, func);
        }

        template<typename Func>
        inline void forEachArtifactIn(Directory* directory, const Func& func) {
            for (Directory* subdir : directory->directoryList)
                forEachArtifactIn(subdir, func);
            for (Artifact*& artifact : directory->artifactList)
                func(artifact);
        }

        template<typename Func>
        inline void forEachArtifact(const Func& func) {
            forEachArtifactIn(root, func);
        }

        JITRuntimeShims* ensureJITRuntimeShims();
    };

    #define FOR_EACH_RESERVED_SYMBOL(macro) \
        macro(MetaNone, "$none") \
        macro(WhitespaceIndent, "$indent") \
        macro(WhitespaceDedent, "$dedent") \
        macro(WhitespaceNewline, "$newline") \
        macro(PunctuatorDot, ".") \
        macro(PunctuatorColon, ":") \
        macro(PunctuatorComma, ",") \
        macro(PunctuatorSemicolon, ";") \
        macro(PunctuatorLeftParen, "(") \
        macro(PunctuatorRightParen, ")") \
        macro(PunctuatorLeftBracket, "[") \
        macro(PunctuatorRightBracket, "]") \
        macro(PunctuatorLeftBrace, "{") \
        macro(PunctuatorRightBrace, "}") \
        macro(OperatorAdd, "+") \
        macro(OperatorSub, "-") \
        macro(OperatorMul, "*") \
        macro(OperatorDiv, "/") \
        macro(OperatorRem, "%") \
        macro(OperatorExp, "**") \
        macro(OperatorBitAnd, "&") \
        macro(OperatorBitOr, "|") \
        macro(OperatorBitXor, "^") \
        macro(OperatorRightShift, ">>") \
        macro(OperatorLeftShift, "<<") \
        macro(OperatorRightRotate, "/>") \
        macro(OperatorLeftRotate, "/<") \
        macro(OperatorLess, "<") \
        macro(OperatorLessEqual, "<=") \
        macro(OperatorGreater, ">") \
        macro(OperatorGreaterEqual, ">=") \
        macro(OperatorEqual, "==") \
        macro(OperatorNotEqual, "!=") \
        macro(OperatorBitNot, "~") \
        macro(OperatorIncr, "++") \
        macro(OperatorDecr, "--") \
        macro(OperatorRange, "..") \
        macro(OperatorEllipsis, "...") \
        macro(OperatorAssign, "=") \
        macro(OperatorAddAssign, "+=") \
        macro(OperatorSubAssign, "-=") \
        macro(OperatorMulAssign, "*=") \
        macro(OperatorDivAssign, "/=") \
        macro(OperatorRemAssign, "%=") \
        macro(OperatorExpAssign, "**=") \
        macro(OperatorBitAndAssign, "&=") \
        macro(OperatorBitOrAssign, "|=") \
        macro(OperatorBitXorAssign, "^=") \
        macro(OperatorRightShiftAssign, ">>=") \
        macro(OperatorLeftShiftAssign, "<<=") \
        macro(OperatorRightRotateAssign, "/>=") \
        macro(OperatorLeftRotateAssign, "/<=") \
        macro(KeywordTrue, "true") \
        macro(KeywordFalse, "false") \
        macro(KeywordNot, "not") \
        macro(KeywordAnd, "and") \
        macro(KeywordOr, "or") \
        macro(KeywordDo, "do") \
        macro(KeywordThen, "then") \
        macro(KeywordIf, "if") \
        macro(KeywordElse, "else") \
        macro(KeywordUnless, "unless") \
        macro(KeywordWhile, "while") \
        macro(KeywordUntil, "until") \
        macro(KeywordFor, "for") \
        macro(KeywordBreak, "break") \
        macro(KeywordContinue, "continue") \
        macro(KeywordReturn, "return") \
        macro(KeywordIs, "is") \
        macro(KeywordIn, "in") \
        macro(KeywordFun, "fun") \
        macro(KeywordVar, "var") \
        macro(KeywordAlias, "alias") \
        macro(KeywordConst, "const") \
        macro(KeywordType, "type") \
        macro(KeywordAny, "any") \
        macro(KeywordMatch, "match") \
        macro(KeywordCase, "case") \
        macro(KeywordOn, "on") \
        macro(KeywordDefault, "default") \
        macro(KeywordUse, "use") \
        macro(KeywordAs, "as") \
        macro(KeywordExport, "export") \
        macro(KeywordRaise, "raise") \
        macro(KeywordRaises, "raises") \
        macro(KeywordOperator, "operator") \
        macro(KeywordNew, "new") \
        macro(KeywordOwn, "own") \
        macro(KeywordUninit, "uninit") \
        macro(IdentifierLoopItems, "__items") \
        macro(IdentifierLoopIterator, "__iter") \
        macro(BuiltinVoid, "void") \
        macro(BuiltinBool, "bool") \
        macro(BuiltinChar, "char") \
        macro(BuiltinI8, "i8") \
        macro(BuiltinI16, "i16") \
        macro(BuiltinI32, "i32") \
        macro(BuiltinI64, "i64") \
        macro(BuiltinU8, "u8") \
        macro(BuiltinU16, "u16") \
        macro(BuiltinU32, "u32") \
        macro(BuiltinU64, "u64") \
        macro(BuiltinF32, "f32") \
        macro(BuiltinF64, "f64") \
        macro(BuiltinString, "string") \
        macro(MethodIter, "iter") \
        macro(MethodDone, "done") \
        macro(MethodPeek, "peek") \
        macro(MethodNext, "next") \
        macro(BuiltinMemoryAlloc, "memory.alloc") \
        macro(BuiltinMemoryFree, "memory.free") \
        macro(BuiltinMathIpow, "math.pow(i64)") \
        macro(BuiltinMathUpow, "math.pow(u64)") \
        macro(BuiltinMathFpow, "math.pow(f64)") \
        macro(BuiltinDebug, "debug")

    enum ReservedSymbols : u32 {
        #define DEFINE_ENUM(name, ...) name,
        FOR_EACH_RESERVED_SYMBOL(DEFINE_ENUM)
        #undef DEFINE_ENUM
        FirstOperator = OperatorAdd,
        LastOperator = OperatorLeftRotateAssign,
        FirstKeyword = KeywordTrue,
        LastKeyword = KeywordUninit,
        NumReservedSymbols = LastKeyword + 1
    };

    static_assert(LastOperator + 1 == FirstKeyword);
    static_assert(LastKeyword + 1 == NumReservedSymbols);

    Artifact* compileUntil(Compilation* compilation, ArtifactKind target, Artifact* artifact);
    void compileUntil(Compilation* compilation, ArtifactKind target);
    void compile(Compilation* compilation);

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const ArtifactKind& kind) {
        return format(io, ArtifactKindNames[(u32)kind]);
    }

    void reportErrorsAndExit(Artifact* artifact);
    void reportErrorsAndExit(ArtifactData* data);
}

#endif