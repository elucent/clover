i32[2] i: [1, 2]
i32* p: &i[0]
print(*p)       # 1
iptr ip: iptr(p)
ip += 4
p = i32*(ip)
print(*p)       # 2