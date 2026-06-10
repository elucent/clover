use std/hash
void log(i64)
void log(u64)
void log(bool)

#--- make_empty

Set(i32) ints: makeset([])
log(ints.size()) # 0

#--- make_fixed

Set(i32) ints: makeset([1, 2, 3])
log(ints.size()) # 3
log(ints.contains(1)) # true
log(ints.contains(2)) # true
log(ints.contains(3)) # true
log(ints.contains(4)) # false

#--- insert_one

var ints: makeset([])
log(ints.size()) # 0
log(ints.contains(42)) # false
ints.insert(42)
log(ints.size()) # 1
log(ints.contains(42)) # true

#--- insert_multiple

var ints: makeset([])
log(ints.size()) # 0
var containsAny: false
containsAny = true if ints.contains(i) for 1 <= i <= 5
log(containsAny) # false

ints.insert(i) for 1 <= i <= 5

var containsAll: true
containsAll = false if not ints.contains(i) for 1 <= i <= 5
log(containsAll) # true

#--- insert_remove_simple

var ints: makeset([])
log(ints.size()) # 0
log(ints.contains(1)) # false
log(ints.contains(2)) # false

ints.insert(1)
ints.insert(2)
log(ints.size()) # 2
log(ints.contains(1)) # true
log(ints.contains(2)) # true

ints.remove(1)
log(ints.size()) # 1
log(ints.contains(1)) # false
log(ints.contains(2)) # true

ints.remove(2)
log(ints.size()) # 0
log(ints.contains(1)) # false
log(ints.contains(2)) # false

#--- insert_remove_100

var ints: makeset([])
ints.insert(i) for i < 100

var containsAll: true
containsAll = false if not ints.contains(i) for i < 100
log(containsAll) # true

ints.remove(i) for i < 100 if i % 2 == 0

var containsAllEven: true
containsAllEven = false if not ints.contains(i) for i < 100 if i % 2 == 0
containsAllEven = false if ints.contains(i) for i < 100 if i % 2 != 0
log(containsAllEven) # true

#--- weird_key_type

type Key:
    i32 value

u64 Key.hash():
    value as u64 << 32 | value as u64

bool Key.equals(Key other):
    value == other.value

var keys: makeset([Key(1), Key(2), Key(3)])
log(keys.size()) # 3
keys.insert(Key(4))
log(keys.size()) # 4
log(keys.contains(Key(1))) # true
log(keys.contains(Key(4))) # true
log(keys.contains(Key(5))) # false

keys.remove(Key(1))
keys.remove(Key(2))
log(keys.contains(Key(1))) # false
log(keys.contains(Key(4))) # true

#--- string_insert_remove

var strings: makeset(["abc", "def"])
log(strings.size()) # 2
log(strings.contains("abc")) # true
log(strings.contains("def")) # true
log(strings.contains("abcd")) # false
log(strings.contains("ab")) # false
log(strings.contains("")) # false

strings.insert("abc")
strings.insert("def")
strings.insert("ghi")
log(strings.size()) # 3
log(strings.contains("abc")) # true
log(strings.contains("ghi")) # true

strings.remove("abc")
strings.remove("abc")
strings.remove("defg")
log(strings.size()) # 2
log(strings.contains("abc")) # false
log(strings.contains("def")) # true
log(strings.contains("ghi")) # true

#--- union

Set union(Set* a, Set* b):
    Set result: makeset([])
    for item in a:
        result.insert(item)
    for item in b:
        result.insert(item)
    return result

var a: makeset([1, 2, 3])
var b: makeset([3, 4, 5])
var c: union(&a, &b)
log(c.size()) # 5
var containsAll: true
containsAll = false if not c.contains(i) for 1 <= i <= 5
log(containsAll) # true

