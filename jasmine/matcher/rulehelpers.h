#ifndef JASMINE_MATCHER_RULEHELPERS_H
#define JASMINE_MATCHER_RULEHELPERS_H

#include "jasmine/pass/reduce.h"

namespace jasmine {
    inline bool canSafelyNegate(AbstractType type, i64 i) {
        switch (type) {
            case AbstractType::I8:
                return i > -0x80 && i <= 0x7f;
            case AbstractType::I16:
                return i > -0x8000 && i <= 0x7fff;
            case AbstractType::I32:
                return i > -0x80000000 && i <= 0x7fffffff;
            case AbstractType::I64:
                return i > -0x8000000000000000ll && i <= 0x7fffffffffffffffll;
            default:
                return false;
        }
    }
}

#endif