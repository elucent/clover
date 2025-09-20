#include "clover/analyze.h"
#include "clover/ast.h"
#include "clover/error.h"
#include "clover/type.h"
#include "util/bits.h"
#include "util/pool.h"
#include "util/deque.h"

namespace clover {
    using PlaceIndex = u32;
    constexpr PlaceIndex InvalidPlace = 0xffffff;

    struct Place;
    struct Tuple;
    struct Ref;
    struct OwnRef;

    struct PlaceWord {
        enum Kind : u32 {
            Dead,   // Dead value.
            Data,  // Value with unspecified layout.
            Scope,  // Scope with an unspecified layout. The difference with Value is that a Scope's lifetime cannot be controlled by moving values around.
            Tuple,  // Tuple of several other values.
            Ref,    // Reference to another value.
            Own     // Owning reference to another value.
        };

        union {
            struct { Kind kind : 8; PlaceIndex parent : 24; }; // Common header
            struct { u32 length; }; // Length of tuple or other aggregate
            struct { PlaceIndex target; }; // Target of reference
        };
    };

    using PlaceKind = PlaceWord::Kind;

    template<typename PlaceLike>
    PlaceIndex placeIndexOf(const PlaceLike& p);

    template<>
    PlaceIndex placeIndexOf(const PlaceIndex& p) { return p; }
    template<>
    PlaceIndex placeIndexOf(const Place& p);

    struct Places {
        vec<u32> places;
        vec<PlaceWord> words;

        Place get(PlaceIndex i);

        template<typename PlaceLike>
        Place makeScope(PlaceLike parent);
        template<typename PlaceLike>
        Place makeData(PlaceLike parent);
        template<typename PlaceLike1, typename PlaceLike2>
        Ref makeRef(PlaceLike1 parent, PlaceLike2 other);
        template<typename PlaceLike1, typename PlaceLike2>
        OwnRef makeOwn(PlaceLike1 parent, PlaceLike2 other);
        template<typename PlaceLike, typename ...Args>
        Tuple makeTuple(PlaceLike parent, const Args&... args);

        u32 placeCountOf(const Place& p) { return 1; }
        u32 placeCountOf(const PlaceIndex& p) { return 1; }
        template<typename T, u32 N>
        u32 placeCountOf(const vec<T, N>& p) {
            u32 sum = 0;
            for (const auto& t : p)
                sum += placeCountOf(t);
            return sum;
        }

        u32 countPlaces() { return 0; }
        template<typename T, typename ...Args>
        u32 countPlaces(const T& t, const Args&... args) {
            return placeCountOf(t) + countPlaces(args...);
        }

        void appendPlace(const Place& p) { words.push({ .target = placeIndexOf(p) }); }
        void appendPlace(const PlaceIndex& p) { words.push({ .target = p }); }
        template<typename T, u32 N>
        u32 appendPlace(const vec<T, N>& p) {
            u32 sum = 0;
            for (const auto& t : p)
                appendPlace(t);
            return sum;
        }

        void appendPlaces() {}
        template<typename T, typename ...Args>
        void appendPlaces(const T& t, const Args&... args) {
            appendPlace(t);
            appendPlaces(args...);
        }
    };

    struct Place {
        Places* places;
        PlaceIndex index;
        u32 word;

        PlaceWord nthWord(u32 i) const;
        PlaceWord& nthWord(u32 i);
        PlaceWord firstWord() const { return nthWord(0); }
        PlaceWord& firstWord() { return nthWord(0); }

        PlaceKind kind() const { return firstWord().kind; }
        bool isDead() const { return firstWord().kind == PlaceKind::Dead; }
        bool isAlive() const { return firstWord().kind != PlaceKind::Dead; }
        bool isScope() const { return firstWord().kind == PlaceKind::Scope; }
        bool isData() const { return firstWord().kind == PlaceKind::Data; }
        bool isTuple() const { return firstWord().kind == PlaceKind::Tuple; }
        bool isRef() const { return firstWord().kind == PlaceKind::Ref; }
        bool isOwn() const { return firstWord().kind == PlaceKind::Own; }

        Place parent() const { return places->get(firstWord().parent); }

        Tuple asTuple() const;
        Ref asRef() const;
        OwnRef asOwn() const;
    };

    template<>
    PlaceIndex placeIndexOf(const Place& p) { return p.index; }

    struct Ref : public Place {
        Place target() { return places->get(nthWord(1).target); }
    };

    struct OwnRef : public Ref {};

    struct Tuple : public Place {
        u32 count() const { return nthWord(1).length; }
        Place at(u32 i) const { return places->get(nthWord(2 + i).target); }
    };

    // Places

    Place Places::get(PlaceIndex i) {
        return Place {
            .places = this,
            .index = i,
            .word = places[i]
        };
    }

    template<typename PlaceLike>
    Place Places::makeData(PlaceLike parent) {
        places.push(words.size());
        words.push({
            .kind = PlaceKind::Data,
            .parent = placeIndexOf(parent)
        });
        return get(places.back());
    }

    template<typename PlaceLike>
    Place Places::makeScope(PlaceLike parent) {
        places.push(words.size());
        words.push({
            .kind = PlaceKind::Scope,
            .parent = placeIndexOf(parent)
        });
        return get(places.back());
    }

    template<typename PlaceLike1, typename PlaceLike2>
    Ref Places::makeRef(PlaceLike1 parent, PlaceLike2 other) {
        places.push(words.size());
        words.push({
            .kind = PlaceKind::Ref,
            .parent = placeIndexOf(parent)
        });
        words.push({
            .target = placeIndexOf(other)
        });
        return get(places.back()).asRef();
    }

    template<typename PlaceLike1, typename PlaceLike2>
    OwnRef Places::makeOwn(PlaceLike1 parent, PlaceLike2 other) {
        places.push(words.size());
        words.push({
            .kind = PlaceKind::Own,
            .parent = placeIndexOf(parent)
        });
        words.push({
            .target = placeIndexOf(other)
        });
        return get(places.back()).asOwn();
    }

    template<typename PlaceLike, typename ...Args>
    Tuple Places::makeTuple(PlaceLike parent, const Args&... args) {
        places.push(words.size());
        words.push({
            .kind = PlaceKind::Own,
            .parent = placeIndexOf(parent)
        });
        words.push({
            .length = countPlaces(args...)
        });
        appendPlaces(args...);
        return get(places.back()).asTuple();
    }

    // Place

    PlaceWord Place::nthWord(u32 i) const {
        return places->words[i];
    }

    PlaceWord& Place::nthWord(u32 i) {
        return places->words[i];
    }

    Tuple Place::asTuple() const {
        return Tuple { places, index, word };
    }

    Ref Place::asRef() const {
        return Ref { places, index, word };
    }

    OwnRef Place::asOwn() const {
        return OwnRef { places, index, word };
    }

    struct State;

    struct TransientPlace {
        // A TransientPlace is either a place, or a special form of place that
        // isn't yet bound to a named location in the current state. These are
        // the output of our abstract interpretation, and specifically we use
        // them over places to represent transient values that may come up when
        // evaluating expression trees. Since values in expression trees are
        // known to be used only once statically, we don't waste memory storing
        // them in the current state. They are also implicitly under the
        // parent of the current scope.

        enum Kind {
            Place,
            Data,
            Ref,
            Own
        };

        union {
            struct { State* state; Kind kind : 8; PlaceIndex place : 24; u32 word; }; // This is how we encode a normal place, or a data value if the place and word are ignored.
            struct { State* _; Kind : 8; PlaceIndex target : 24; u32 : 32; }; // This is how we encode a reference.
        };
    };

    struct State {
        Places* places;
        PlaceIndex scope, external;
        vec<PlaceIndex> variables;

        Place persist(TransientPlace value) {
            switch (value.kind) {
                case TransientPlace::Place:
                    return Place { places, value.place, value.word };
                case TransientPlace::Data:
                    return places->makeData(scope);
                case TransientPlace::Ref:
                    return places->makeRef(scope, value.target);
                case TransientPlace::Own:
                    return places->makeOwn(scope, value.target);
            }
        }

        template<typename PlaceLike>
        TransientPlace valueFrom(PlaceLike p) {
            return TransientPlace { .state = this, .kind = TransientPlace::Place, .place = placeIndexOf(p), .word = places->places[placeIndexOf(p)] };
        }

        TransientPlace makeData() {
            return TransientPlace { .state = this, .kind = TransientPlace::Data, .place = InvalidPlace, .word = 0 };
        }

        template<typename PlaceLike>
        TransientPlace makeRef(PlaceLike p) {
            return TransientPlace { .state = this, .kind = TransientPlace::Ref, .target = placeIndexOf(p), .word = 0 };
        }

        template<typename PlaceLike>
        TransientPlace makeOwn(PlaceLike p) {
            return TransientPlace { .state = this, .kind = TransientPlace::Own, .target = placeIndexOf(p), .word = 0 };
        }

        void init(Places* places_in, u32 count) {
            places = places_in;
            variables.expandTo(count, InvalidPlace);
        }

        Place get(u32 var) const {
            assert(variables[var] != InvalidPlace);
            return places->get(variables[var]);
        }

        void set(u32 var, Place place) {
            variables[var] = place.index;
        }

        void set(u32 var, TransientPlace value) {
            variables[var] = persist(value).index;
        }
    };

    void consume(Module* module, AST ast, TransientPlace value) {
    }

    TransientPlace interpret(Module* module, Places* places, State* state, AST ast) {
        switch (ast.kind()) {
            case ASTKind::Int:
            case ASTKind::Unsigned:
            case ASTKind::Float:
            case ASTKind::Bool:
            case ASTKind::Char:
            case ASTKind::String:
                return state->makeData();

            case ASTKind::Minus:
            case ASTKind::Plus:
            case ASTKind::Paren:
            case ASTKind::Not:
            case ASTKind::BitNot:
                consume(module, ast, interpret(module, places, state, ast.child(0)));
                return state->makeData();

            case ASTKind::Global:
                return state->valueFrom(state->get(ast.variable()));

            case ASTKind::Local:
                return state->valueFrom(state->get(ast.variable()));

            case ASTKind::TopLevel: {
                // Set up the top-level state.
                Places globalPlaces;
                State globalState;

                // Global scope has no external scope.
                // TODO: Re-evaluate this when we have threads?
                globalState.init(&globalPlaces, module->globals.size());
                globalState.scope = globalPlaces.makeScope(InvalidPlace).index;
                globalState.external = InvalidPlace;

                for (AST child : ast)
                    consume(module, child, interpret(module, &globalPlaces, &globalState, child));
                return state->makeData();
            }

            case ASTKind::FunDecl: {
                // Set up the function's state. Places do not cross between
                // function boundaries.
                Places functionPlaces;
                State functionState;

                // Function scope does have an external scope, representing
                // all parent frames and the global scope.
                functionState.init(&functionPlaces, ast.function()->locals.size());
            }

            default:
                unreachable("Can't borrow-check node ", ast);
        }
    }

    NOINLINE Artifact* analyze(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::CheckedAST);

        Module* module = artifact->as<Module>();
        interpret(module, nullptr, nullptr, module->getTopLevel());

        artifact->update(ArtifactKind::AnalyzedAST, module);
        return artifact;
    }
}