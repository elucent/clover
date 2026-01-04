#include "clover/type.h"
#include "clover/scope.h"
#include "clover/ast.h"

namespace clover {
    TypeSystem* TypeKey::sys;

    void StructBuilder::add(Scope* scope) {
        this->scope = scope;
    }

    void UnionBuilder::add(Scope* scope) {
        this->scope = scope;
    }

    void printInstantiations(GenericType* generic) {
        if (!generic->instantiations)
            return;
        TypeSystem* sys = generic->module->types;
        for (const auto& [k, v] : *generic->instantiations)
            println(" - ", TypesKeyLogger { sys, k }, " => ", sys->get(v.type), " @ ", generic->module->node(v.node));
    }

    template<TypeKind Kind>
    void concretifyGeneric(Type t) {
        auto type = t.as<Kind>();

        TypesKey key = TypesKey::withLength(type.typeParameterCount());
        for (u32 i = 0; i < type.typeParameterCount(); i ++) {
            Type param = expand(type.typeParameter(i));
            param.concretify();
            param = expand(param);
            key.setParameterType(i, param.index);
        }
        key.computeHash();
        TypeSystem* sys = type.types;
        GenericType* generic = type.genericOrigin();
        assert(generic->instantiations);
        auto it = generic->instantiations->find(key);
        if (it != generic->instantiations->end()) {
            Type other = sys->get(it->value.type = expand(sys, it->value.type));
            assert(type.isGeneric());
            assert(other.is<Kind>());
            assert(other.as<Kind>().isGeneric());
            assert(type.genericOriginIndex() == other.as<Kind>().genericOriginIndex());

            #ifndef RELEASE
                for (u32 i = 0; i < type.typeParameterCount(); i ++) {
                    assert(expand(type.typeParameter(i)) == expand(other.as<Kind>().typeParameter(i)));
                    assert(other.as<Kind>().typeParameter(i).isntVar());
                }
            #endif

            // A named type must have at least enough space to store a var type. So
            // we replace this type's data in-place with a type variable pointing
            // to our replacement.

            type.firstWord().kind = TypeKind::Var;
            type.firstWord().isConcrete = false;
            type.firstWord().typeBound = other.index;

            type.nthWord(1).markedEqual = true;
            type.nthWord(1).hasOwner = false;
            type.nthWord(1).typeBound = other.index;

            if UNLIKELY(config::readableTypeVars) {
                type.nthWord(2).bits = sys->vars.size();
                sys->vars.push(type.index);
            }
        } else {
            // Must not have been created before. We know our type params are
            // concrete at this point, so we can update them in-place to their
            // concrete assignments, and re-enter ourselves into the
            // instantiations map.
            TypesKey current = TypesKey::withLength(type.typeParameterCount());
            for (u32 i = 0; i < type.typeParameterCount(); i ++) {
                current.setParameterType(i, type.typeParameterIndex(i));
                type.setTypeParameterIndex(i, key.parameterType(i));
            }
            current.computeHash();
            auto it = generic->instantiations->find(current);
            assert(it != generic->instantiations->end());
            NodeIndex node = it->value.node;
            generic->instantiations->erase(current);
            generic->instantiations->put(key, { type.index, node });
        }
    }

    void NamedType::concretify() {
        if (!isGeneric() || isConcrete())
            return;
        return concretifyGeneric<TypeKind::Named>(*this);
    }

    void StructType::concretify() {
        if (!isGeneric() || isConcrete())
            return;
        return concretifyGeneric<TypeKind::Struct>(*this);
    }

    void UnionType::concretify() {
        if (!isGeneric() || isConcrete())
            return;
        return concretifyGeneric<TypeKind::Union>(*this);
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