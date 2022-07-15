#ifndef BASIL_SOLVER_TERM_H
#define BASIL_SOLVER_TERM_H

#include "core/def.h"
#include "lib/vec.h"
#include "lib/hash.h"
#include "lib/io.h"

using Symbol = i32;

struct SymbolTable {
    map<const_slice<i8>, Symbol> table;
    vec<const_slice<i8>> strings;

    inline Symbol operator()(const_slice<i8> s) {
        auto it = table.find(s);
        if (it == table.end()) {
            strings.push(s);
            table.put(s, strings.size() - 1);
            return strings.size() - 1;
        }
        else return it->value;
    }

    inline Symbol operator()(const i8* s) {
        return operator()({s, cidx(s, 0)});
    }

    inline const_slice<i8> operator()(Symbol s) const {
        return strings[s];
    }
};

struct Context;

struct VarEntry {
    Symbol name;
    const Context* ctx;

    inline bool operator==(const VarEntry& other) const {
        return name == other.name && ctx == other.ctx;
    }
};

inline u64 hash(const VarEntry& v) {
    return hash(v.name) * 31 ^ hash(u64(v.ctx));
}

struct Term {
    enum Kind : u8 {
        NONE, VAR, LIT, PRED
    };

    static const Term TNONE;

    Kind kind;
    union {
        Symbol lit;
        struct { Symbol var; Context* varenv; };
        struct { Symbol pred; slice<Term> params; };
    };

    inline Term(): kind(Term::NONE) {}

    static inline const Term& None() {
        return TNONE;
    }

    static inline Term Lit(Symbol s) {
        Term t;
        t.kind = Term::LIT;
        t.lit = s;
        return t;
    }

    static inline Term Var(Symbol s, Context* env) {
        Term t;
        t.kind = Term::VAR;
        t.var = s;
        t.varenv = env;
        return t;
    }

    static inline Term Pred(Symbol s, slice<Term> p) {
        Term t;
        t.kind = Term::PRED;
        t.pred = s;
        t.params = p;
        return t;
    }
};

struct Context {
    Context* parent;
    u32 depth;
    map<VarEntry, Term> bindings;

    inline Context(Context* parent_in): parent(parent_in), depth(parent ? parent->depth + 1 : 0) {}

    inline const Term& lookup_sym(Symbol s) const {
        auto it = bindings.find(VarEntry{s, this});
        if (it != bindings.end()) return it->value;
        else return Term::None();
    }

    inline const Term& lookup(const VarEntry& v) const {
        auto it = bindings.find(v);
        if (it != bindings.end()) return it->value;
        else return Term::None();
    }

    inline const Term& nearest(const VarEntry& v) const {
        auto it = bindings.find(v);
        if (it != bindings.end()) return it->value;
        else if (parent) return parent->nearest(v);
        else return Term::None();
    }

    inline bool contains(const VarEntry& v) const {
        return bindings.find(v) != bindings.end();
    }

    inline void def_sym(Symbol s, Term t) {
        // print("      ", Term::Var(s, this), " = ", t, '\n');
        bindings.put({s, this}, t);
    }

    inline void def(const VarEntry& v, Term t) {
        // print("      ", Term::Var(v.name, (Context*)v.ctx), " = ", t, '\n');
        bindings.put(v, t);
    }
};

inline bool unify(Context* ctx, const Term& lhs, const Term& rhs) {
    const Term& a = lhs.kind <= rhs.kind ? lhs : rhs;
    const Term& b = lhs.kind <= rhs.kind ? rhs : lhs;
    u8 ored = a.kind << 4 | b.kind;
    print("    unifying ", a, " with ", b, " in ", *ctx, '\n');
    switch (ored) {
        case Term::NONE << 4 | Term::NONE:
        case Term::NONE << 4 | Term::PRED:
        case Term::NONE << 4 | Term::LIT:
        case Term::NONE << 4 | Term::VAR:
            return true;
        case Term::VAR << 4 | Term::VAR: {
            const Term& va = a.varenv->depth >= b.varenv->depth ? a : b;
            const Term& vb = a.varenv->depth >= b.varenv->depth ? b : a;
            if (va.varenv != ctx && !ctx->contains({va.var, va.varenv}))
                ctx->def({va.var, va.varenv}, ctx->nearest({va.var, va.varenv}));
            const Term& av = ctx->lookup({va.var, va.varenv});
            if (unify(ctx, av, vb)) {
                if (av.kind == Term::NONE) ctx->def({va.var, va.varenv}, vb);
                return true;
            }
            else return false;
        }
        case Term::VAR << 4 | Term::LIT:
        case Term::VAR << 4 | Term::PRED: {
            if (a.varenv != ctx && !ctx->contains({a.var, a.varenv}))
                ctx->def({a.var, a.varenv}, ctx->nearest({a.var, a.varenv}));
            const Term& av = ctx->lookup({a.var, a.varenv});
            if (unify(ctx, av, b)) {
                if (av.kind == Term::NONE) ctx->def({a.var, a.varenv}, b);
                return true;
            }
            else return false;
        }
        case Term::LIT << 4 | Term::LIT:
            return a.lit == b.lit;
        case Term::LIT << 4 | Term::PRED:
            return false;
        case Term::PRED << 4 | Term::PRED:
            if (lhs.pred != rhs.pred) return false;
            if (lhs.params.n != rhs.params.n) return false;
            for (iptr i = 0; i < lhs.params.n; i ++) {
                if (!unify(ctx, lhs.params[i], rhs.params[i])) return false;
            }
            return true;
        default:
            unreachable("Unreachable combination of term kinds.");
            return false;
    }
}

struct Query;
struct DB;

struct Rule {
    Symbol pred;
    slice<Term> params;
    Term body;
    Query* (*builtin)(Context*, DB*, const Term&) = nullptr;

    inline Rule(Term pred_in, Term body_in):
        pred(pred_in.pred), params(pred_in.params), body(body_in) {}
    inline Rule(Term pred_in, decltype(builtin) fn): 
        pred(pred_in.pred), params(pred_in.params), builtin(fn) {} 
};

struct DB {
    SymbolTable& syms;
    set<Symbol> litv;
    map<Symbol, vec<Rule>> rulev;

    static const vec<Rule> NO_RULES;

    inline DB(SymbolTable& syms_in): syms(syms_in) {}

    inline const vec<Rule>& rules(Symbol s) const {
        auto it = rulev.find(s);
        if (it != rulev.end()) return it->value;
        else return NO_RULES;
    }

    inline void fact(Term term) {
        switch (term.kind) {
            case Term::NONE:
            case Term::VAR: break;
            case Term::LIT: litv.insert(term.lit);
            case Term::PRED: rulev[term.pred].push(Rule{term, Term::Pred(syms("true"), slice<Term>{nullptr, (iptr)0})});
        }
    }

    inline void rule(Term pred, Term body) {
        rulev[pred.pred].push(Rule(pred, body));
    }

    inline void rule(Term pred, decltype(Rule::builtin) fn) {
        rulev[pred.pred].push(Rule(pred, fn));
    }
};

struct Answer {
    Context* ctx;
    bool cut;

    inline operator bool() const {
        return ctx;
    }
};

struct Query {
    Context* ctx;

    inline Query(Context* ctx_in): ctx(ctx_in) {}
    inline virtual ~Query() { delete ctx; }
    virtual Answer next() = 0;
};

struct TrueQuery : public Query {
    inline TrueQuery(Context* ctx): Query(ctx) {}
    inline Answer next() override {
        return {ctx, true};
    }
};

struct FalseQuery : public Query {
    inline FalseQuery(Context* ctx): Query(ctx) {}
    inline Answer next() override {
        return {nullptr, true};
    }
};

inline Context* record(Context* cur, Term pred) {
    Context* pctx = new Context(cur);
    for (const Term& t : pred.params) {
        if (t.kind == Term::VAR) pctx->def_sym(t.var, Term::None());
    }
    // print("created ctx ", *pctx, " with depth ", pctx->depth, " for ", pred, '\n');
    return pctx;
}

inline Term matpred(Term pred, Context* pctx) {
    slice<Term> params = {new Term[pred.params.n], pred.params.n};
    for (u32 i = 0; i < params.n; i ++) {
        if (pred.params[i].kind == Term::VAR) params[i] = Term::Var(pred.params[i].var, pctx);
        else if (pred.params[i].kind == Term::PRED) params[i] = matpred(pred.params[i], pctx);
        else params[i] = pred.params[i];
    }
    return Term::Pred(pred.pred, params);
}

struct DBQuery;
inline DBQuery* dbquery(Context* ctx, const Term& t);

struct DBQuery : public Query {
    DB* db;
    Query* q = nullptr;
    Term t;
    u32 i = 0;
    inline DBQuery(Context* ctx, DB* db_in, const Term& t_in): Query(ctx), db(db_in), t(t_in) {}
    inline ~DBQuery() { delete[] t.params.ptr; }

    inline Answer next() override {
        print("?- ", t, ' ', *ctx, '\n');
        if (q) {
            Answer ans = q->next();
            if (ans) {
                if (ans.cut) {
                    // delete q;
                    q = nullptr;
                }
                // print("  propagating bindings from ", *ans.ctx, " to ", *ctx, "\n");
                for (const auto& binding : ans.ctx->bindings) {
                    if (binding.key.ctx->depth <= ctx->depth) ctx->def(binding.key, binding.value);
                }
                print(t, " returned ", *ans.ctx, '\n');
                return {ctx, false};
            }
            else {
                print(t, " failed\n");
                // delete q;
                q = nullptr;
            }
        }
        const vec<Rule>& rules = db->rules(t.pred);
        while (i < rules.size()) {
            const Rule& r = rules[i ++];
            Term rpred = Term::Pred(r.pred, r.params);
            Context* ictx = record(ctx, Term::Pred(r.pred, r.params));
            Term mpred = matpred(rpred, ictx);
            // print("  considering ", mpred, " for ", t, " in ", *ictx, '\n');

            if (unify(ictx, mpred, t)) {
                // print("  using ", mpred, ' ', *ictx, '\n');
                if (r.builtin) q = r.builtin(ictx, db, mpred);
                else q = new DBQuery(ictx, db, matpred(r.body, ictx));
                return next();
            }
        }
        return {nullptr, false};
    }
};

struct ANDQuery : public Query {
    DB* db;
    Query *a, *b;
    inline ANDQuery(Context* ctx, DB* db_in): 
        Query(ctx), db(db_in), a(new DBQuery(ctx, db, ctx->lookup_sym(db->syms("X")))), b(nullptr) {}
    inline ~ANDQuery() { 
        delete a; 
        if (b) delete b;
    }
    inline Answer next() override {
        while (true) {
            if (b) {
                if (Answer ans = b->next()) return ans;
                else {
                    for (auto& e : ctx->bindings) if (e.key.ctx->depth < ctx->depth) {
                        e.value = ctx->parent->nearest(e.key);
                    }
                    // delete b;
                    b = nullptr;
                }
            }
            else if (Answer ans = a->next()) b = new DBQuery(ctx, db, ctx->lookup_sym(db->syms("Y")));
            else break;
        }
        return {nullptr, false};
    }
};

struct EQQuery : public Query {
    DB* db;
    inline EQQuery(Context* ctx, DB* db_in): Query(ctx), db(db_in) {}
    inline Answer next() override {
        return {unify(ctx, ctx->lookup_sym(db->syms("X")), ctx->lookup_sym(db->syms("Y"))) ? ctx : nullptr, true};
    }
};

struct NEQQuery : public Query {
    DB* db;
    inline NEQQuery(Context* ctx, DB* db_in): Query(ctx), db(db_in) {}
    inline Answer next() override {
        return {unify(ctx, ctx->lookup_sym(db->syms("X")), ctx->lookup_sym(db->syms("Y"))) ? nullptr : ctx, true};
    }
};

struct Solver {
    SymbolTable syms;
    DB db;
    Context root;
    u32 indent = 0;

    void def_builtins() {
        slice<Term> args = {new Term[2], 2};
        args[0] = Term::Var(syms("X"), &root), args[1] = Term::Var(syms("Y"), &root);
        db.rule(Term::Pred(syms("and"), args), [](Context* parent, DB* db, const Term& t) -> Query* { return new ANDQuery(parent, db); });
        db.rule(Term::Pred(syms("eq"), args), [](Context* parent, DB* db, const Term& t) -> Query* { return new EQQuery(parent, db); });
        db.rule(Term::Pred(syms("neq"), args), [](Context* parent, DB* db, const Term& t) -> Query* { return new NEQQuery(parent, db); });
        db.rule(Term::Pred(syms("true"), slice<Term>{nullptr, (iptr)0}), [](Context* parent, DB* db, const Term& t) -> Query* { return new TrueQuery(parent); });
        db.rule(Term::Pred(syms("false"), slice<Term>{nullptr, (iptr)0}), [](Context* parent, DB* db, const Term& t) -> Query* { return new FalseQuery(parent); });
    }

    inline Solver(): db(syms), root(nullptr) {
        def_builtins();
    }


    inline bool query(Term q) {
        if (q.kind < Term::LIT) return true;
        if (q.kind == Term::LIT) return db.litv.find(q.lit) != db.litv.end();

        Query* qp = new DBQuery(&root, &db, q);
        Answer ans = qp->next();
        print(ans ? "true" : "false", " ");
        if (ans) print(*ans.ctx);
        print('\n');
        return ans;
    }
};

extern Solver* SOLVER;

inline void write(stream& io, const Term& t) {
    switch (t.kind) {
        case Term::NONE: ::write(io, '!'); break;
        case Term::VAR: ::write(io, SOLVER->syms(t.var), t.varenv->depth, '?'); write(io, '('); write(io, t.varenv->lookup({t.var, t.varenv})); write(io, ')'); break;
        case Term::LIT: ::write(io, SOLVER->syms(t.var)); break;
        case Term::PRED:
            ::write(io, SOLVER->syms(t.pred), '(');
            for (const auto& p : t.params) {
                if (&p != &t.params[0]) ::write(io, ", ");
                ::write(io, p);
            }
            ::write(io, ')');
            break;
    }
}

inline void write(stream& io, const Rule& r) {
    ::write(io, Term::Pred(r.pred, r.params), " :- ", r.body);
}

inline Term concrete(const Context& ctx, const VarEntry& v) {
    Term t = ctx.lookup(v);
    while (t.kind == Term::VAR) t = ctx.lookup({t.var, t.varenv});
    return t;
}

inline void write(stream& io, const Context& r) {
    ::write(io, '[');
    bool first = true;
    for (const auto& binding : r.bindings) {
        if (!first) ::write(io, ", ");
        first = false;
        ::write(io, SOLVER->syms(binding.key.name), binding.key.ctx->depth, '?', '=', r.lookup(binding.key));
    }
    ::write(io, ']');
    // if (r.parent) ::write(io, " <- ", *r.parent);
}


inline void write(stream& io, const Solver& s) {
    for (const auto& rule : s.db.rulev) for (const auto& r : rule.value) if (!r.builtin) write(io, r, ".\n");
}

#endif