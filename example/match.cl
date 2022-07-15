int fib(int n):
    match n:
        case 0: 0
        case 1: 1
        case n: fib(n - 1) + fib(n - 2)

print(fib(10))