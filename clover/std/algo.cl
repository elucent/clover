use std/rt

T min(type T, T a, T b):
    a if a < b else b

T max(type T, T a, T b):
    a if a > b else b

T min(type T, T[] xs):
    var x: xs[0]
    for y in xs[1:] if y < x:
        x = y
    return x

T max(type T, T[] xs):
    var x: xs[0]
    for y in xs[1:] if y > x:
        x = y
    return x

void swap(type T, T* a, T* b):
    T tmp: uninit
    memory.move(&tmp, a, |T|)
    memory.move(a, b, |T|)
    memory.move(b, &tmp, |T|)

void reverse(type T, T[] xs):
    for i < |xs| / 2:
        swap(&xs[i], &xs[|xs| - i - 1])

type Enumerator:
    var internal
    u32 count

Enumerator enumerate(iterable):
    return Enumerator(iterable.iter(), 0)

Enumerator(T) Enumerator(T).iter(type T):
    this

fun Enumerator(T).read(type T):
    (count, internal.read())

Enumerator(T) Enumerator(T).next(type T):
    Enumerator(T)(count + 1, internal.next())

bool Enumerator(T).done(type T):
    internal.done()