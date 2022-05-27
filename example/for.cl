void int.print()

type Range: int low, high, incr

fun Range.iter(): this
fun Range.empty(): this.low == this.high
fun Range.read(): this.low
fun Range.next(): Range(this.low + this.incr, this.high, this.incr)

for i in Range(0, 50, 10):
    i.print()

for i in [3, 5, 7]:
    i.print()
