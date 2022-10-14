
type Foo(T):
    T y
    void bar():
        print(y)

var x: Foo(1)
var y: Foo("hello")

x.bar()
y.bar()

int add(int a, int b):
    a + b
