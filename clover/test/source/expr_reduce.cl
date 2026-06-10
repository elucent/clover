use std/hash
use std/io

type Ctx:
    Map(char, i32) bindings

type Expr:
    case Int: i32
    case Var:
        char name
    case Add:
        own Expr* lhs, rhs
    case Let:
        char name
        own Expr* init, body
use Expr.*

own Expr* reduce(own Expr* expr, Ctx* ctx):
    match expr:
        case own Int* i:
            i
        case own Var* v:
            if ctx.bindings.contains(v.name):
                return new Int(ctx.bindings.get(v.name))
            return v
        case own Add* add:
            add.lhs = reduce(add.lhs, ctx)
            add.rhs = reduce(add.rhs, ctx)
            if add.lhs is Int(i) and add.rhs is Int(j):
                return new Int(i + j)
            return add
        case own Let* let:
            let.init = reduce(let.init, ctx)
            if let.init is Int(i):
                ctx.bindings.set(let.name, i)
                return reduce(let.body, ctx)
            return let

var expr: new Let(
    'x', new Add(new Int(1), new Int(2)),
    new Let(
        'y', new Add(new Int(3), new Int(4)),
        new Add(new Var('x'), new Var('y'))
    )
)

var ctx: Ctx(makemap([]))
expr = expr.reduce(&ctx)
if expr is Int(i):
    println(i) # 10
