#ifndef CLOVER_RESOLVE_H
#define CLOVER_RESOLVE_H

#include "clover/type.h"
#include "clover/ast.h"

namespace clover {
    bool isTypeExpression(AST ast);
    bool isTypeDecl(AST ast);

    AST resolveNode(Scope* scope, AST parent, AST ast);
    NOINLINE Artifact* resolveNamesAndTypes(Artifact* artifact);
}

#endif