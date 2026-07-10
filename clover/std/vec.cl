use std/algo

type Vec(type T):
    own T[] items
    u32 size

Vec(T) makevec(type T, T[] items):
    var cap: 8
    cap *= 2 while cap < |items|
    var storage: new T[cap]
    storage[:|items|] = items
    Vec(T)(storage as own T[], |items|)

void Vec(T)*.grow(type T):
    var buf: new T[|items| * 2]
    buf[:size] = items[:size]
    items = buf as own T[]

void Vec(T)*.push(type T, T item):
    this.grow() if size + 1 > |items|
    items[size ++] = item

u32 Vec(T)*.size(type T):
    this.size

T Vec(T)*.pop(type T):
    items[-- size]

T Vec(T)*.get(type T, u64 i):
    items[i]

T* Vec(T)*.at(type T, u64 i):
    &items[i]

void Vec(T)*.set(type T, u64 i, T item):
    items[i] = item

void Vec(T)*.clear(type T):
    size = 0

T[] Vec(T)*.iter(type T):
    items[:size]

T Vec(T)*.last(type T):
    items[size - 1]

void Vec(T)*.append(type T, iterable):
    this.push(item) for item in iterable

void Vec(T)*.reverse(type T):
    items[:size].reverse()

bool Vec(T)*.contains(type T, T item):
    items[:size].contains(item)

void removeIf(type T, Vec(T)* vec, bool(T) predicate):
    var writer: 0, reader: 0
    vec[writer] = vec[reader] if predicate(vec.items[reader]) while reader < vec.size
    vec.size = writer
