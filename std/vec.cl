
type Vec(T):
    i8[] data
    int size

    fun grow():
        var old: data
        data = new 0[|old| * 2]
        var i: 0
        var oldts: T[](T*(old), size)
        var newts: T[](T*(data), size)
        until i == size:
            newts[i] = oldts[i]
            i ++
        del oldts

    T[] items():
        T[](T*(data), size)

    fun set(T t, int i):
        this.items()[i] = t

    fun get(int i):
        this.items()[i]

    fun push(T t):
        if size + 1 >= |data|: this.grow()
        this.items()[size ++] = t

    fun pop():
        size --

    fun clear():
        size = 0

fun fill(T? t, int n):
    var v: Vec(T)(new 0[|T| * n], n)
    int i: 0
    until i == v.size:
        v.items()[i ++] = t
    return v

var v: fill(0, 4)
v.clear()
int i: 0
until i == 10: v.push(i ++)
until i == 0: print(v.get(-- i))
