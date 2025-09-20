#ifndef CLOVER_INTERPRETER_H
#define CLOVER_INTERPRETER_H

#include "clover/value.h"
#include "clover/ast.h"

namespace clover {
    Value eval(Value env, AST expr);
    Value call(AST func, slice<Value> values);
}

#endif