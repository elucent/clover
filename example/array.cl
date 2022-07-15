var arr: [1, 2, 3]
int[] s1: arr
var i: s1[0]
int[] s2: s1[1:]

fun do_thing(int[] arr):
    match arr:
        case [1, x, y]: print(x + y)
        case z: print("other!") 

do_thing(arr)
do_thing(s2)
