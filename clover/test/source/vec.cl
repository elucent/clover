use std/vec
void log(i64)
void log(u64)
void log(bool)

#--- make_empty

Vec(i32) vec: makevec([])
log(vec.size()) # 0

#--- make_fixed

Vec(i32) vec: makevec([1, 2, 3])
log(vec.size()) # 3
log(vec.get(0)) # 1
log(vec.get(1)) # 2
log(vec.get(2)) # 3

#--- push_pop

Vec(i32) vec: makevec([1, 2, 3])
log(vec.last()) # 3
vec.push(4)
vec.push(5)
log(vec.last()) # 5
log(vec.pop()) # 5
log(vec.pop()) # 4
log(vec.last()) # 3

#--- append

Vec(i32) vec: makevec([1, 2, 3])
vec.append([4, 5])

i32[] slice: [6, 7]
vec.append(slice)

Vec(i32) othervec: makevec([8, 9])
vec.append(othervec)

log(vec.size()) # 9
bool allCorrect: true
for 1 <= i < 10 if vec.get(i - 1) != i as i32:
    allCorrect = false
log(allCorrect) # true


