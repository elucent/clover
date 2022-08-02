i32 fib(i32 n):
    if n < 2: 
        n
    else: 
        fib(n - 1) + fib(n - 2)

print(fib(45))
