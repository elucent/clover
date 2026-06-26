use std/deque

in test:
    void log(i64)
    void log(bool)
    void log(u64)

#--- make_empty

var deque: makedeque(i32, [])
test.log(deque.size()) # 0

#--- push_pop_stack

var deque: makedeque([])
test.log(deque.size()) # 0

deque.pushRight(1)
deque.pushRight(2)
deque.pushRight(3)

test.log(deque.size()) # 3
test.log(deque.popRight()) # 3
test.log(deque.popRight()) # 2
test.log(deque.popRight()) # 1
test.log(deque.size()) # 0

#--- push_pop_queue

var deque: makedeque([])
test.log(deque.size()) # 0

deque.pushLeft(1)
deque.pushLeft(2)
deque.pushLeft(3)
deque.pushLeft(4)

test.log(deque.size()) # 4
test.log(deque.popRight()) # 1
test.log(deque.popRight()) # 2
test.log(deque.popRight()) # 3
test.log(deque.popRight()) # 4
