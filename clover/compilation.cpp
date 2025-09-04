#include "clover/compilation.h"
#include "clover/scope.h"
#include "clover/resolve.h"
#include "clover/typecheck.h"
#include "clover/type.h"
#include "clover/analyze.h"
#include "clover/error.h"
#include "clover/lex.h"
#include "clover/parse.h"
#include "clover/gen.h"
#include "util/config.h"

namespace clover {
    const i8* ArtifactKindNames[(u32)ArtifactKind::NumArtifactKinds] = {
        #define CREATE_STRING(name) #name,
        FOR_EACH_ARTIFACT_KIND(CREATE_STRING)
        #undef CREATE_STRING
    };

    ArtifactData::~ArtifactData() {}

    void Directory::addArtifact(Symbol name, Artifact* artifact) {
        auto it = mapping.find(name);
        if (it != mapping.end()) {
            if (it->value < 0)
                panic("Resource '", compilation->str(name), "' is a directory!");
            return;
        }
        mapping.put(name, artifactList.size());
        artifactList.push(artifact);
        compilation->filesChanged = true;
    }

    void Directory::addSubDirectory(Symbol name, Directory* directory) {
        auto it = mapping.find(name);
        if (it != mapping.end()) {
            if (it->value >= 0)
                panic("Resource '", compilation->str(name), "' is not a directory!");
            return;
        }
        mapping.put(name, -i32(directoryList.size() + 1));
        directoryList.push(directory);
        compilation->filesChanged = true;
    }

    Artifact* Directory::artifactByName(Symbol name) {
        auto it = mapping.find(name);
        if UNLIKELY(it == mapping.end())
            return nullptr;
        assert(it->value >= 0);
        return artifactList[it->value];
    }

    Directory* Directory::directoryByName(Symbol name) {
        auto it = mapping.find(name);
        if UNLIKELY(it == mapping.end())
            return nullptr;
        assert(it->value < 0);
        return directoryList[-it->value - 1];
    }

    Directory* Directory::ensureDirectoryByName(Symbol name) {
        auto it = mapping.find(name);
        if UNLIKELY(it == mapping.end()) {
            auto newDirectory = new Directory(compilation, this, name);
            addSubDirectory(name, newDirectory);
            return newDirectory;
        }
        assert(it->value < 0);
        return directoryList[-it->value - 1];
    }

    void Artifact::print() {
        Compilation* compilation = parent->compilation;
        ::print("=== Artifact ");
        Directory* directory = parent;
        vec<Symbol, 16> path;
        while (directory != compilation->root)
            path.push(directory->name), directory = directory->parent;
        for (i64 i = i64(path.size()) - 1; i >= 0; i --)
            ::print(compilation->str(path[i]), '.');
        ::println(compilation->str(name), ": ", ArtifactKindNames[(u32)kind], " (", data->reportSize(), " bytes) ===");
        data->print(compilation);
        ::println();
    }

    void ArtifactData::addError(Error* error) {
        error->next = errors;
        errors = error;
    }

    void ArtifactData::takeErrors(vec<Error*>& collector) {
        while (errors) {
            Error* node = errors;
            errors = node->next;
            node->next = nullptr;
            collector.push(node);
        }
    }

    Compilation::Compilation() {
        #define DEFINE_SYMBOL(name, value) sym(value);
        FOR_EACH_RESERVED_SYMBOL(DEFINE_SYMBOL)
        #undef DEFINE_SYMBOL

        #define ASSERT_SYMBOL(name, value) assert(sym(value) == (u32)ReservedSymbols::name);
        FOR_EACH_RESERVED_SYMBOL(ASSERT_SYMBOL)
        #undef ASSERT_SYMBOL

        root = new Directory(this, nullptr, sym(""));
        vec<u32> fakeOffsets;
        rootModule = new Module(this, cstring(""), move(fakeOffsets));
        rootScope = nullptr;
        types = new TypeSystem(this);
        slice<i8> buf = { new i8[256], 256 };
        cwd = file::cwd(buf);
    }

    Compilation::~Compilation() {
        delete root;
        delete types;
        delete[] cwd.data();
        if (shims) delete shims;
    }


    JITRuntimeShims* Compilation::ensureJITRuntimeShims() {
        if (!shims) {
            shims = new JITRuntimeShims(symbols);
            initializeJITRuntime(*shims);
        }
        return shims;
    }

    void printDirectoryName(Directory* directory) {
        if (directory->parent) {
            printDirectoryName(directory->parent);
            print("/", directory->compilation->str(directory->name));
        }
    }

    void printArtifactName(Artifact* artifact) {
        Compilation* compilation = artifact->parent->compilation;
        printDirectoryName(artifact->parent);
        if (artifact->parent != compilation->root)
            print("/");
        if (artifact->filename.size())
            print(artifact->filename);
        else
            print(compilation->str(artifact->name));
    }

    void printSourceLine(ArtifactData* data, Pos pos) {
        auto src = data->getSource();
        auto lines = data->getLineOffsets();
        if (src && lines) {
            const_slice<i8> line;
            if (pos.line == lines->size() - 1)
                line = src->drop(lines->last());
            else {
                u32 firstOffset = (*lines)[pos.line];
                u32 lastOffset = (*lines)[pos.line + 1];
                line = (*src)[{ firstOffset, lastOffset }];
            }
            for (u32 i = 0; i < 4; i ++)
                print(' ');
            print(line);
            if (line.last() != '\n')
                println();
            for (u32 i = 0; i < pos.column + 4; i ++)
                print(' ');
            println('^');
        }
    }

    void reportNote(Artifact* artifact, Note* note) {
        print("[");
        printArtifactName(artifact);
        println(":", note->pos.line + 1, ":", note->pos.column + 1, "] " BOLDGRAY "note" RESET ": ", note->message);
        printSourceLine(artifact->data, note->pos);
    }

    void reportNote(ArtifactData* data, Note* note) {
        println("[<unknown>:", note->pos.line + 1, ":", note->pos.column + 1, "] " BOLDGRAY "note" RESET ": ", note->message);
        printSourceLine(data, note->pos);
    }

    void reportError(Artifact* artifact, Error* error) {
        print("[");
        printArtifactName(artifact);
        println(":", error->pos.line + 1, ":", error->pos.column + 1, "] " BOLDRED "error" RESET ": ", error->message);
        printSourceLine(artifact->data, error->pos);

        for (Note* note : error->notes)
            reportNote(artifact, note);
    }

    void reportError(ArtifactData* data, Error* error) {
        println("[<unknown>:", error->pos.line + 1, ":", error->pos.column + 1, "] " BOLDRED "error" RESET ": ", error->message);
        printSourceLine(data, error->pos);

        for (Note* note : error->notes)
            reportNote(data, note);
    }

    void reportErrorsAndExit(Artifact* artifact) {
        vec<Error*> errors;
        artifact->takeErrors(errors);

        for (Error* error : reversed(errors))
            reportError(artifact, error);
        process::exit(1);
    }

    void reportErrorsAndExit(ArtifactData* data) {
        vec<Error*> errors;
        data->takeErrors(errors);

        for (Error* error : reversed(errors))
            reportError(data, error);
        process::exit(1);
    }

    Artifact* compileUntil(Compilation* compilation, ArtifactKind target, Artifact* artifact) {
        if (artifact->kind == target)
            return artifact;
        switch (target) {
            case ArtifactKind::Source:
                return artifact;
            case ArtifactKind::Tokens:
                artifact = lex(artifact);
                if (artifact->hasErrors())
                    reportErrorsAndExit(artifact);
                return artifact;
            case ArtifactKind::ParsedAST:
                artifact = compileUntil(compilation, ArtifactKind::Tokens, artifact);
                if UNLIKELY(config::parseAsSexp)
                    artifact = clover::parseAsSexp(artifact);
                else
                    artifact = clover::parse(artifact);
                if (artifact->hasErrors())
                    reportErrorsAndExit(artifact);
                return artifact;
            case ArtifactKind::ScopedAST:
                artifact = compileUntil(compilation, ArtifactKind::ParsedAST, artifact);
                artifact = clover::computeScopes(artifact);
                if (artifact->hasErrors())
                    reportErrorsAndExit(artifact);
                return artifact;
            case ArtifactKind::ResolvedAST:
                artifact = compileUntil(compilation, ArtifactKind::ScopedAST, artifact);
                artifact = clover::resolveNamesAndTypes(artifact);
                if (artifact->hasErrors())
                    reportErrorsAndExit(artifact);
                return artifact;
            case ArtifactKind::CheckedAST:
                artifact = compileUntil(compilation, ArtifactKind::ResolvedAST, artifact);
                artifact = clover::inferAndCheckTypes(artifact);
                if (artifact->hasErrors())
                    reportErrorsAndExit(artifact);
                return artifact;
            case ArtifactKind::FinalizedAST:
                artifact = compileUntil(compilation, config::finalizeAfterTypechecking ? ArtifactKind::CheckedAST : ArtifactKind::AnalyzedAST, artifact);
                artifact->update(ArtifactKind::FinalizedAST, artifact->as<Module>());
                if (artifact->hasErrors())
                    reportErrorsAndExit(artifact);
                return artifact;
            case ArtifactKind::JasmineIR:
                artifact = compileUntil(compilation, ArtifactKind::FinalizedAST, artifact);
                artifact = generateJasmine(artifact, compilation->optimizationLevel);
                if (artifact->hasErrors())
                    reportErrorsAndExit(artifact);
                return artifact;
            case ArtifactKind::Assembly:
                artifact = compileUntil(compilation, ArtifactKind::JasmineIR, artifact);
                if (artifact->kind == ArtifactKind::Assembly)
                    return artifact;
                artifact = emitAssembly(artifact, compilation->optimizationLevel);
                if (artifact->hasErrors())
                    reportErrorsAndExit(artifact);
                return artifact;
            default:
                unreachable("Unimplemented compilation stage ", target);
        }
    }

    void compileUntil(Compilation* compilation, ArtifactKind target) {
        do {
            compilation->filesChanged = false;
            compilation->forEachArtifact([&](Artifact*& artifact) {
                artifact = compileUntil(compilation, target, artifact);
            });
        } while (compilation->filesChanged);
    }

    void compile(Compilation* compilation) {
        ArtifactKind defaultTarget = ArtifactKind::Assembly;
        if UNLIKELY(config::loadOnly)
            defaultTarget = ArtifactKind::Source;
        if UNLIKELY(config::lexOnly)
            defaultTarget = ArtifactKind::Tokens;
        if UNLIKELY(config::parseOnly)
            defaultTarget = ArtifactKind::ParsedAST;
        if UNLIKELY(config::typecheckOnly)
            defaultTarget = ArtifactKind::CheckedAST;
        if UNLIKELY(config::compileToJasmine)
            defaultTarget = ArtifactKind::JasmineIR;

        compileUntil(compilation, defaultTarget);
    }
}