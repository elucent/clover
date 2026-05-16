type Vec(type T):
    own T[] items
    u64 size

Vec(T) makevec(type T, T[] items):
    var cap: 8
    cap *= 2 while cap < |items|
    var storage: new T[cap]
    storage[:|items|] = items
    Vec(T)(storage as own T[], |items|)

void grow(type T, Vec(T)* vec):
    var buf: new T[|vec.items| * 2]
    buf[:vec.size] = vec.items[:vec.size]
    vec.items = buf as own T[]

void push(type T, Vec(T)* vec, T item):
    vec.grow() if vec.size + 1 > |vec.items|
    vec.items[vec.size ++] = item

T pop(type T, Vec(T)* vec):
    vec.items[vec.size --]

T get(type T, Vec(T)* vec, u64 i):
    vec.items[i]

T* at(type T, Vec(T)* vec, u64 i):
    &vec.items[i]

void set(type T, Vec(T)* vec, u64 i, T item):
    vec.items[i] = item

void clear(type T, Vec(T)* vec):
    del x for x in vec.items
    vec.size = 0

T[] iter(type T, Vec(T)* vec):
    vec.items[:vec.size]

T last(type T, Vec(T)* vec):
    vec.items[vec.size - 1]

void removeIf(type T, Vec(T)* vec, bool(T) predicate):
    var writer: 0, reader: 0
    vec[writer] = vec[reader] if predicate(vec.items[reader]) while reader < vec.size:
    del vec.items[i] for writer <= i < reader
    vec.size = writer

