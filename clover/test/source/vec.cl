use std/vec
void log(i64)
void log(u64)

#--- make_empty

Vec(i32) vec: makevec([])
log(vec.size()) # 0

#--- make_fixed

Vec(i32) vec: makevec([1, 2, 3])
log(vec.size()) # 3
log(vec.get(0)) # 1
log(vec.get(1)) # 2
log(vec.get(2)) # 3
