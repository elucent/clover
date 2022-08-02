
type Foo:
    case Bar:
        int x
        void quux(): print("is bar")
    case Baz:
        float y
        void quux(): print("is baz")

var foo: Foo.Bar(1)
foo.quux()
foo = Foo.Baz(2.0)
foo.quux()
