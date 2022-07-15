#include "solver/term.h"

i32 main(i32 argc, i8** argv) {
    Solver solver;
    SOLVER = &solver;

    Term Alice = Term::Lit(solver.syms("Alice"));
    Term Bob = Term::Lit(solver.syms("Bob"));
    Term Cathy = Term::Lit(solver.syms("Cathy"));

    slice<Term> params1 = {new Term[2], 2};
    params1[0] = Alice, params1[1] = Cathy;
    slice<Term> params2 = {new Term[2], 2};
    params2[0] = Bob, params2[1] = Cathy;
    slice<Term> params3 = {new Term[2], 2};
    params3[0] = Alice, params3[1] = Term::Var(solver.syms("X"), &solver.root);
    slice<Term> params4 = {new Term[2], 2};
    params4[0] = Term::Var(solver.syms("A"), &solver.root), params4[1] = Term::Var(solver.syms("B"), &solver.root);
    slice<Term> params5 = {new Term[2], 2};
    params5[0] = Term::Var(solver.syms("A"), &solver.root), params5[1] = Term::Var(solver.syms("C"), &solver.root);
    slice<Term> params6 = {new Term[2], 2};
    params6[0] = Term::Var(solver.syms("B"), &solver.root), params6[1] = Term::Var(solver.syms("C"), &solver.root);
    slice<Term> params7 = {new Term[2], 2};
    params7[0] = Term::Pred(solver.syms("parent"), params5), params7[1] = Term::Pred(solver.syms("parent"), params6);
    slice<Term> params8 = {new Term[2], 2};
    params8[0] = Term::Pred(solver.syms("and"), params7), params8[1] = Term::Pred(solver.syms("neq"), params4);

    solver.db.fact(Term::Pred(solver.syms("parent"), params1));
    solver.db.fact(Term::Pred(solver.syms("parent"), params2));
    solver.db.rule(Term::Pred(solver.syms("sibling"), params4), Term::Pred(solver.syms("and"), params8));
    print(solver, '\n');

    Term q = Term::Pred(solver.syms("sibling"), params3);
    solver.query(q);
    return 0;
}