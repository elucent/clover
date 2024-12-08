#ifndef CLOVER_LIMITS_H
#define CLOVER_LIMITS_H

#include "rt/def.h"

namespace clover {
    using NodeIndex = u32;
    using TypeIndex = u32;
    using ScopeIndex = u32;
    using ConstraintIndex = u32;

    namespace Limits {
        constexpr u32
            SymbolsPerCompilationBits = 24,
            SymbolsPerCompilation = (1 << SymbolsPerCompilationBits) - 1,
            TypesPerCompilationBits = 24,
            TypesPerCompilation = (1 << TypesPerCompilationBits) - 1,
            ScopesPerCompilationBits = 24,
            ScopesPerCompilation = (1 << ScopesPerCompilationBits) - 1,
            IdsPerCompilationBits = 24,
            IdsPerCompilation = (1 << IdsPerCompilationBits) - 1,
            NodesPerFunctionBits = 24,
            NodesPerFunction = (1 << NodesPerFunctionBits) - 1,
            ConstantsPerFunctionBits = 23,
            ConstantsPerFunction = (1 << ConstantsPerFunctionBits) - 1,
            MaxChildrenPerNodeBits = 10,
            MaxChildrenPerNode = (1 << MaxChildrenPerNodeBits) - 1,
            MaxFieldsPerTypeBits = 15,
            MaxFieldsPerType = (1 << MaxFieldsPerTypeBits) - 1,
            ConstraintsPerCompilationBits = 24,
            ConstraintsPerCompilation = (1 << ConstraintsPerCompilationBits) - 1;

        constexpr u64
            MaxArrayLengthBits = 32,
            MaxArrayLength = 1ull << MaxArrayLengthBits;

        constexpr u32
            InvalidSymbol = SymbolsPerCompilation,
            InvalidType = TypesPerCompilation,
            InvalidNode = NodesPerFunction,
            InvalidScope = ScopesPerCompilation,
            InvalidConstant = ConstantsPerFunction,
            InvalidConstraint = ConstraintsPerCompilation,
            InvalidId = IdsPerCompilation;
    };

    using Limits::InvalidSymbol;
    using Limits::InvalidType;
    using Limits::InvalidNode;
    using Limits::InvalidScope;
    using Limits::InvalidConstant;
    using Limits::InvalidId;
    using Limits::InvalidConstraint;
}

#endif