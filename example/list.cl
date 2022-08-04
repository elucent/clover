
type List(T):
    case Cons:
        T car
        List(T)* cdr

        int len(): 1 + cdr.len()
    case Nil:
        int len(): 0

with List(char):
    var list: new 'a'.Cons(new 'b'.Cons(new Nil()))
    print(list.len())
    print(len(list))

