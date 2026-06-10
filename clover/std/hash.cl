#######
# Set #
#######

type Bucket:
    case Empty
    case Ghost
    case Filled

type Set(type T):
    own Bucket[] buckets
    own T[] entries
    u32 size

Set(T) makeset(type T, T[] items):
    use Bucket.*

    Set(T) set: Set(T)((new Bucket[8]) as own Bucket[], (new T[8]) as own T[], 0)
    set.buckets[i] = Empty for i < 8
    for item in items:
        set.insert(item)
    return set

type FindResult(type T):
    case Found:
        T* item
    case None

FindResult(T) find(type T, Set(T)* set, T key):
    use Bucket.*
    use FindResult.*

    u64 h: key.hash()
    u64 cap: |set.buckets|
    u64 i: h & cap - 1
    while true:
        match set.buckets[i]:
            case Empty:
                return None
            case Ghost:
                i = i + 1 & cap - 1
            case Filled:
                if set.entries[i].equals(key):
                    return Found(&set.entries[i])
                i = i + 1 & cap - 1
    return None

u32 size(type T, Set(T)* set):
    return set.size

bool contains(type T, Set(T)* set, T key):
    use FindResult.*
    return set.find(key) is not None

void grow(type T, Set(T)* set):
    use Bucket.*

    own Bucket[] oldBuckets: set.buckets
    own T[] oldEntries: set.entries

    set.buckets = (new Bucket[|oldBuckets| * 2]) as own Bucket[]
    set.buckets[i] = Empty for i < |set.buckets|

    set.entries = (new T[|oldBuckets| * 2]) as own T[]
    set.size = 0
    for i < |oldBuckets|:
        set.insert(oldEntries[i]) if oldBuckets[i] is Filled

void insert(type T, Set(T)* set, T key):
    use Bucket.*
    use FindResult.*

    if (set.size + 1) * 10 > |set.buckets| * 6:
        set.grow()

    u64 h: key.hash()
    u64 cap: |set.buckets|
    u64 i: h & cap - 1
    while true:
        match set.buckets[i]:
            case Empty:
                set.buckets[i] = Filled
                set.entries[i] = key
                set.size ++
                return
            case Ghost:
                set.buckets[i] = Filled
                set.entries[i] = key
                set.size ++
                return
            case Filled:
                if set.entries[i].equals(key):
                    set.entries[i] = key
                    return
                i = i + 1 & cap - 1

void remove(type T, Set(T)* set, T key):
    use Bucket.*
    use FindResult.*

    u64 h: key.hash()
    u64 cap: |set.buckets|
    u64 i: h & cap - 1
    while true:
        var bucket: set.buckets[i]
        return if bucket is Empty
        if bucket is Filled and set.entries[i].equals(key):
            set.buckets[i] = Empty
            set.size --
            return
        i = i + 1 & cap - 1

type SetIterator(type T):
    Set(T)* set
    u32 i

SetIterator(T) iter(type T, Set(T)* set):
    use Bucket.*
    var i: 0
    while i < |set.buckets| and set.buckets[i] is not Filled:
        i ++
    return SetIterator(T)(set, i)

bool done(type T, SetIterator(T) iter):
    iter.i == |iter.set.buckets|

T read(type T, SetIterator(T) iter):
    var result: uninit # TODO: Add a panic/exception.
    if iter.i < |iter.set.buckets|:
        result = iter.set.entries[iter.i]
    return result

SetIterator(T) next(type T, SetIterator(T) iter):
    use Bucket.*
    iter.i ++
    while iter.i < |iter.set.buckets| and iter.set.buckets[iter.i] is not Filled:
        iter.i ++
    return iter

#####################################
# Primitive hash/equality functions #
#####################################

u64 intHash(u64 u):
    u = ((u >> 16) ^ u) * 0x45d9f3b
    u = ((u >> 16) ^ u) * 0x45d9f3b
    u = (u >> 16) ^ u
    return u

u64 hash(u64 u): intHash(u)
u64 hash(i64 i): intHash(i as u64)

bool equals(u64 a, u64 b): a == b
bool equals(i64 a, i64 b): a == b

#################
# Map and Entry #
#################

type Entry:
    var key
    var value

u64 hash(type K, type V, Entry(K, V) entry):
    return hash(entry.key)

bool equals(type K, type V, Entry(K, V) a, Entry(K, V) b):
    return a.key.equals(b.key)

type Map:
    Set(Entry) entries

Map(K, V) makemap(type K, type V, (K, V)[] pairs):
    var map: Map(K, V)(makeset(Entry(K, V), []))
    for (k, v) in pairs:
        map.set(k, v)
    return map

void erase(type K, type V, Map(K, V)* map, K key):
    Entry(K, V) entry: uninit
    entry.key = key
    map.entries.erase(entry)

bool contains(type K, type V, Map(K, V)* map, K key):
    Entry(K, V) entry: uninit
    entry.key = key
    map.entries.contains(entry)

V get(type K, type V, Map(K, V)* map, K key):
    use FindResult.*

    Entry(K, V) entry: uninit
    entry.key = key
    var result: map.entries.find(entry)
    match result:
        case Found(f): f.value
        case None: entry.value

V* at(type K, type V, Map(K, V)* map, K key):
    use FindResult.*

    Entry(K, V) entry: uninit
    entry.key = key
    var result: map.entries.find(entry)
    match result:
        case Found(f): &f.value
        case None: &entry.value

void set(type K, type V, Map(K, V)* map, K key, V value):
    var entry: Entry(K, V)(key, value)
    map.entries.insert(entry)

type MapIterator(type K, type V):
    SetIterator(Entry(K, V)) internal

MapIterator(K, V) iter(type K, type V, Map(K, V)* map):
    MapIterator(K, V)(map.entries.iter())

bool done(type K, type V, MapIterator(K, V) iter):
    return iter.internal.done()

(K, V) read(type K, type V, MapIterator(K, V) iter):
    var entry: iter.internal.read()
    (entry.key, entry.value)

MapIterator(K, V) next(type K, type V, MapIterator(K, V) iter):
    iter.internal = iter.internal.next()
    return iter

##################################
# String hash/equality functions #
##################################

u64 hash(i8[] str):
	const m: 0xc6a4a7935bd1e995
	const r: 47
	u64 h: 7576351903513440497 ^ |str| * m
    u64 suffix: |str| % 8

    u64[] words: str as u64[]
    for i < |str| / 8:
        u64 k: words[i] * m
		k ^= k >> r
		h ^= k * m
		h *= m
    for i < suffix:
        h ^= str[|str| - suffix + i] as u64
        h /<= 8
	h ^= h >> r
	h *= m
	h ^= h >> r

	return h

bool equals(i8[] a, i8[] b):
    return false if |a| != |b|
    return false if a[i] != b[i] for i < |a|
    return true

