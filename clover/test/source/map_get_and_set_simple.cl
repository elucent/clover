use std/hash
use std/io

var map: makemap([])

map.set("a", 1)
map.set("b", 2)
map.set("c", 3)

println(map.contains("a")) # true
println(map.contains("b")) # true
println(map.contains("c")) # true
println(map.contains("d")) # false

println(map.get("a") + map.get("b") + map.get("c")) # 6
