use std/vec

in test:
    void log(i64)
    void log(u64)
    void log(bool)

#--- make_empty

Vec(i32) vec: makevec([])
test.log(vec.size()) # 0

#--- make_fixed

Vec(i32) vec: makevec([1, 2, 3])
test.log(vec.size()) # 3
test.log(vec.get(0)) # 1
test.log(vec.get(1)) # 2
test.log(vec.get(2)) # 3

#--- push_pop

Vec(i32) vec: makevec([1, 2, 3])
test.log(vec.last()) # 3
vec.push(4)
vec.push(5)
test.log(vec.last()) # 5
test.log(vec.pop()) # 5
test.log(vec.pop()) # 4
test.log(vec.last()) # 3

#--- append

Vec(i32) vec: makevec([1, 2, 3])
vec.append([4, 5])

i32[] slice: [6, 7]
vec.append(slice)

Vec(i32) othervec: makevec([8, 9])
vec.append(othervec)

test.log(vec.size()) # 9
bool allCorrect: true
for 1 <= i < 10 if vec.get(i - 1) != i as i32:
    allCorrect = false
test.log(allCorrect) # true

#--- reverse_empty

Vec(i32) vec: makevec([])

bool noIter: true
noIter = false for i in vec
test.log(vec.size()) # 0
test.log(noIter) # true

vec.reverse()
noIter = false for i in vec
test.log(vec.size()) # 0
test.log(noIter) # true

#--- reverse_single

Vec(i32) vec: makevec([42])

test.log(vec.get(0)) # 42
test.log(vec.size()) # 1

vec.reverse()
test.log(vec.get(0)) # 42
test.log(vec.size()) # 1

#--- reverse_multiple_odd

Vec(i32) vec: makevec([1, 2, 3])
test.log(vec.get(0)) # 1
test.log(vec.get(1)) # 2
test.log(vec.get(2)) # 3

vec.reverse()
test.log(vec.get(0)) # 3
test.log(vec.get(1)) # 2
test.log(vec.get(2)) # 1

#--- reverse_multiple_even

Vec(i32) vec: makevec([1, 2, 3, 4])
test.log(vec.get(0)) # 1
test.log(vec.get(1)) # 2
test.log(vec.get(2)) # 3
test.log(vec.get(3)) # 4

vec.reverse()
test.log(vec.get(0)) # 4
test.log(vec.get(1)) # 3
test.log(vec.get(2)) # 2
test.log(vec.get(3)) # 1

#--- reverse_100

Vec(i32) vec: makevec([])
for i < 100:
    vec.push(i)

test.log(vec.size()) # 100
bool allInOrder: true
for i < 100 if i > 0:
    if vec.get(i) != vec.get(i - 1) + 1:
        allInOrder = false
test.log(allInOrder) # true

vec.reverse()
test.log(vec.size()) # 100
for i < 100 if i > 0:
    if vec.get(i) != vec.get(i - 1) - 1:
        allInOrder = false
test.log(allInOrder) # true

#--- in

var vec: makevec([])
vec.push(i) for i < 10
test.log(1 in vec) # true
test.log(-1 in vec) # false
test.log(9 in vec) # true
test.log(10 not in vec) # true
test.log(42 not in vec) # true
