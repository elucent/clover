type Expr:
    case Int: int
    case Add: 
        Expr* lhs, rhs

int eval(Expr* e):
    match *e:
        case Int(i): i
        case Add(l, r): eval(l) + eval(r)

with Expr:
    var program: new Add(new Int(1), new Int(2))
    print(eval(program))
