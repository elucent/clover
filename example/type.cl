void int.print()

type Vec2:
    int x, y

int Vec2.dot(Vec2 v):
    this.x * v.x + this.y * v.y

Vec2 a, b
a.x = 1
a.y = 0
b.x = 0
b.y = 1
print(a.dot(b))
