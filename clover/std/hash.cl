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

FindResult(T) Set(T)*.find(type T, T key):
    use Bucket.*
    use FindResult.*

    u64 h: key.hash()
    u64 cap: |buckets|
    u64 i: h & cap - 1
    while true:
        match buckets[i]:
            case Empty:
                return None
            case Ghost:
                i = i + 1 & cap - 1
            case Filled:
                if entries[i].equals(key):
                    return Found(&entries[i])
                i = i + 1 & cap - 1
    return None

u32 Set*.size():
    return this.size

bool Set(T)*.contains(type T, T key):
    use FindResult.*
    return this.find(key) is not None

void Set(T)*.grow(type T):
    use Bucket.*

    own Bucket[] oldBuckets: buckets
    own T[] oldEntries: entries

    buckets = (new Bucket[|oldBuckets| * 2]) as own Bucket[]
    buckets[i] = Empty for i < |buckets|

    entries = (new T[|oldBuckets| * 2]) as own T[]
    size = 0
    for i < |oldBuckets|:
        this.insert(oldEntries[i]) if oldBuckets[i] is Filled

void Set(T)*.insert(type T, T key):
    use Bucket.*
    use FindResult.*

    if (size + 1) * 10 > |buckets| * 6:
        this.grow()

    u64 h: key.hash()
    u64 cap: |buckets|
    u64 i: h & cap - 1
    while true:
        match buckets[i]:
            case Empty:
                buckets[i] = Filled
                entries[i] = key
                size ++
                return
            case Ghost:
                buckets[i] = Filled
                entries[i] = key
                size ++
                return
            case Filled:
                if entries[i].equals(key):
                    entries[i] = key
                    return
                i = i + 1 & cap - 1

void Set(T)*.remove(type T, T key):
    use Bucket.*
    use FindResult.*

    u64 h: key.hash()
    u64 cap: |buckets|
    u64 i: h & cap - 1
    while true:
        var bucket: buckets[i]
        return if bucket is Empty
        if bucket is Filled and entries[i].equals(key):
            buckets[i] = Empty
            size --
            return
        i = i + 1 & cap - 1

type SetIterator(type T):
    Set(T)* set
    u32 i

SetIterator(T) Set(T)*.iter(type T):
    use Bucket.*
    var i: 0
    while i < |buckets| and buckets[i] is not Filled:
        i ++
    return SetIterator(T)(this, i)

bool SetIterator(T).done(type T):
    i == |set.buckets|

T SetIterator(T).read(type T):
    var result: uninit # TODO: Add a panic/exception.
    if i < |set.buckets|:
        result = set.entries[i]
    return result

SetIterator(T) SetIterator(T).next(type T):
    use Bucket.*
    i ++
    while i < |set.buckets| and set.buckets[i] is not Filled:
        i ++
    return this

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
u64 hash(char c): intHash(c as u64)

bool equals(u64 a, u64 b): a == b
bool equals(i64 a, i64 b): a == b
bool equals(char a, char b): a == b

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

void Map(K, V)*.erase(type K, type V, K key):
    Entry(K, V) entry: uninit
    entry.key = key
    entries.erase(entry)

bool Map(K, V)*.contains(type K, type V, K key):
    Entry(K, V) entry: uninit
    entry.key = key
    entries.contains(entry)

V Map(K, V)*.get(type K, type V, K key):
    use FindResult.*

    Entry(K, V) entry: uninit
    entry.key = key
    var result: entries.find(entry)
    match result:
        case Found(f): f.value
        case None: entry.value

V* Map(K, V)*.at(type K, type V, K key):
    use FindResult.*

    Entry(K, V) entry: uninit
    entry.key = key
    var result: entries.find(entry)
    match result:
        case Found(f): &f.value
        case None: &entry.value

void Map(K, V)*.set(type K, type V, K key, V value):
    var entry: Entry(K, V)(key, value)
    entries.insert(entry)

type MapIterator(type K, type V):
    SetIterator(Entry(K, V)) internal

MapIterator(K, V) Map(K, V)*.iter(type K, type V):
    MapIterator(K, V)(entries.iter())

bool MapIterator(K, V).done(type K, type V):
    return internal.done()

(K, V) MapIterator(K, V).read(type K, type V):
    var entry: internal.read()
    (entry.key, entry.value)

MapIterator(K, V) MapIterator(K, V).next(type K, type V):
    internal = internal.next()
    return this

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

