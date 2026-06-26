use std/algo

type Deque(type T):
    own T[] items
    u32 start, end

Deque(T) makedeque(type T, T[] items):
    var deque: Deque(T)((new T[8]) as own T[], 0, 0)
    for item in items:
        deque.pushRight(item)
    return deque

void Deque(T)*.grow(type T):
    var buf: new T[|items| * 2]
    var i: start, j: 0
    while i != end:
        buf[j ++] = items[i]
        i = (i + 1) & (|items| - 1)
    start = 0
    end = j
    items = buf as own T[]

void Deque(T)*.pushLeft(type T, T item):
    start = (start - 1) & (|items| - 1)
    this.grow() if end == start
    items[start] = item

void Deque(T)*.pushRight(type T, T item):
    items[end] = item
    end = (end + 1) & (|items| - 1)
    this.grow() if end == start

T Deque(T)*.popLeft(type T):
    var result: items[start]
    start = (start + 1) & (|items| - 1)
    result

T Deque(T)*.popRight(type T):
    end = (end - 1) & (|items| - 1)
    items[end]

T Deque(T)*.get(type T, u64 i):
    items[(start + i) & (|items| - 1)]

T* Deque(T)*.at(type T, u64 i):
    &items[(start + i) & (|items| - 1)]

void Deque(T)*.set(type T, u64 i, T item):
    items[(start + i) & (|items| - 1)] = item

u32 Deque(T)*.size(type T):
    u32 size: end
    if start > end:
        size += |items|
    return size - start

void Deque(T)*.clear(type T):
    var i: start
    while i != end:
        i = (i + 1) & (|items| - 1)
    end = 0
    start = end

type DequeIterator(type T):
    Deque(T)* deque
    u32 i

DequeIterator(T) Deque(T)*.iter(type T):
    DequeIterator(T)(this, start)

T DequeIterator(T).read(type T):
    deque.get(i)

DequeIterator(T) DequeIterator(T).next(type T):
    DequeIterator(T)(deque, (i + 1) & (|deque.items| - 1))

bool DequeIterator(T).done(type T):
    i == deque.end

T Deque(T)*.first(type T):
    items[start]

T Deque(T)*.last(type T):
    items[(end - 1) & (|items| - 1)]

void Deque(T)*.prepend(type T, iterable):
    var sizeBefore: this.size()
    this.pushLeft(item) for item in iterable
    var numAdded: this.size() - sizeBefore
    for i < numAdded / 2:
        swap(this.at(i), this.at(numAdded - i - 1))

void Deque(T)*.append(type T, iterable):
    this.pushRight(item) for item in iterable

