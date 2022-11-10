int count(xs):
    var num: 0
    for x in xs:
        num ++
    return num

fun first(xs):
    xs.read()

fun fold(f, i, xs):
    var acc: i
    for x in xs:
        acc = f(acc, x)
    return acc

fun reduce(f, xs):
    fold(f, xs.read(), xs.next())

fun collect(xs):
    var len: 0
    T? elt
    for x in xs:
        elt = x
        len ++
    var result: new elt[len]
    len = 0
    for x in xs:
        result[len ++] = x
    return result
