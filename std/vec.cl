
type Vec(T):
    i8[] data
    int size

    fun grow():
        var old: data
        data = new 0[|old| * 2]
        var i: 0
        var oldts: T*(old)[:size]
        var newts: T*(data)[:size]
        until i == size:
            newts[i] = oldts[i]
            i ++
        del oldts

    T[] items():
        T*(data)[:size]

    type Iterator:
        Vec(T)* v
        int i

        bool empty():
            i == v.size

        T read():
            v.items()[i]
        
        Iterator next():
            Iterator(v, i + 1)

    fun Vec(T)*.iter():
        Iterator(&this, 0)

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

module vec:
    fun of(T? t, int n):
        return Vec(T)(new t[|T| * n], n)