#include "clover/type.h"
#include "clover/scope.h"

namespace clover {
    TypeSystem* TypeKey::sys;

    void StructBuilder::add(Scope* scope) {
        this->scope = scope;
    }

    void UnionBuilder::add(Scope* scope) {
        this->scope = scope;
    }

    void Constraints::validateConstraintGraph() {
        // This function is a debugging tool, add it manually to places you
        // want to audit for possible memory bugs.

        for (ConstraintIndex i = 0; i < constraints.size(); i ++) {
            TypeIndex t = constrainedTypes[i];
            assert(t != InvalidType);
            assert(i == types->constraintNodes[t].index());
            const ConstraintList& list = constraints[i];
            assert(list.sz <= 65536); // Sanity check - if it's higher than this, it's a good chance it's memory corruption.
            if (list.isForwarded()) {
                assert(!list.hasRefinementList);
                assert(list.expand());
                assert(*list.expand() < constraints.size());
            } else {
                assert(!list.hasRefinementList || list.data()[0].kind == Constraint::Order);
                for (Constraint constraint : list) {
                    assert(constraint.index < constraints.size());
                    assert(constraint.kind == Constraint::Subtype || constraint.kind == Constraint::Substitute || constraint.kind == Constraint::Order);
                }
            }
        }
    }
}