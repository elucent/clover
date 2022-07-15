i8 x: 1

f32 a: x + 0.5
print(a)        # 1.5

f64 b: a - 1
print(b)        # 0.5

f32 c: f32(b) + 2
print(c)        # 2.5

int y: int(c - 0.4999)
print(y)        # 2