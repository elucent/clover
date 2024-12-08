#include "clover/compilation.h"
#include "clover/load.h"
#include "clover/lex.h"
#include "clover/parse.h"
#include "clover/resolve.h"
#include "clover/typecheck.h"
#include "clover/analyze.h"
#include "clover/gen.h"
#include "util/test/harness.h"

using namespace clover;

struct OwnedArtifact {
    Artifact* artifact;

    inline OwnedArtifact(Artifact* artifact_in):
        artifact(artifact_in) {}

    inline ~OwnedArtifact() {
        if (artifact)
            delete artifact->parent->compilation;
    }

    inline OwnedArtifact(OwnedArtifact&& other):
        artifact(other.artifact) {
        other.artifact = nullptr;
    }

    OwnedArtifact& operator=(OwnedArtifact&& other) = delete;

    PREVENT_COPYING(OwnedArtifact);

    template<typename T>
    inline T* as() {
        return artifact->as<T>();
    }

    template<typename T>
    inline const T* as() const {
        return artifact->as<T>();
    }
};

enum ShouldComparePos { ComparePos, IgnorePos };
bool sameAST(ArtifactKind, Function*, AST a, AST b, ShouldComparePos posMode);

inline bool sameAST(OwnedArtifact&& a, OwnedArtifact&& b, ShouldComparePos posMode) {
    return sameAST(a.artifact->kind, nullptr, a.as<Module>()->getTopLevel(), b.as<Module>()->getTopLevel(), posMode);
}

#define SYM(s) artifact.artifact->parent->compilation->sym(s)
#define STR(s) artifact.artifact->parent->compilation->str(s)

extern bool inExpectErrorsScope;

struct ExpectErrorsScope {
    bool reportImmediately, inExpectErrors;

    inline ExpectErrorsScope() {
        inExpectErrors = inExpectErrorsScope;
        reportImmediately = config::reportErrorsImmediately;

        config::reportErrorsImmediately = false;
        inExpectErrorsScope = true;
    }

    inline ~ExpectErrorsScope() {
        config::reportErrorsImmediately = reportImmediately;
        inExpectErrorsScope = inExpectErrors;
    }
};

#define EXPECT_ERRORS ExpectErrorsScope _errorsScope

#define ASSERT_DID_ERROR(instance) ASSERT(instance.artifact->hasErrors())
#define ASSERT_NO_ERRORS(instance) ASSERT(!instance.artifact->hasErrors())
#define ASSERT_NO_ERRORS_ARTIFACT(artifact) [&] { if (!inExpectErrorsScope) { ASSERT(!artifact->hasErrors()); } }()

#define LEX(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        return source; \
    }())

#define PARSE_SEXP(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        source = lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = parseAsSexp(source); \
        return source; \
    }())

#define PARSE(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        source = lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::parse(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        return source; \
    }())

#define PARSE_EXPAND(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        source = lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::parse(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        return source; \
    }())

#define SCOPE(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        source = lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::parse(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::computeScopes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        return source; \
    }())

#define RESOLVE(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        source = lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::parse(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::computeScopes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::resolveNamesAndTypes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        return source; \
    }())

#define TYPECHECK(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        source = lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::parse(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::computeScopes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::resolveNamesAndTypes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::inferAndCheckTypes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        return source; \
    }())

#define ANALYZE(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        source = lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::parse(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::computeScopes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::resolveNamesAndTypes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::inferAndCheckTypes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::analyze(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        return source; \
    }())

#define COMPILE(str) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        JITRuntimeShims shims(compilation->symbols); \
        Artifact* source = addSourceString(compilation, cstring("test"), cstring(str)); \
        source = lex(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::parse(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::computeScopes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::resolveNamesAndTypes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source = clover::inferAndCheckTypes(source); \
        ASSERT_NO_ERRORS_ARTIFACT(source); \
        source->update(ArtifactKind::FinalizedAST, source->as<Module>()); \
        source = clover::generateJasmine(source, config::forceOptLevel == -1 ? 0 : config::forceOptLevel); \
        if (source->kind != ArtifactKind::Assembly) \
            source = clover::emitAssembly(source, config::forceOptLevel == -1 ? 0 : config::forceOptLevel); \
        return source; \
    }())
