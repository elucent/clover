type Either:
    case Int: int
    case Float: float

Either* e: new Either.Int(1)

match *e:
    case Int(i): print("Int!")
    case Float: print("Float!")
