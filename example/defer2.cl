int foo():
    defer return 123
    return 456

print(foo())
