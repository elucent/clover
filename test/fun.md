# Functions

```
fun ackermann(m, n):
    if m == 0: 
        n + 1
    else if n == 0: 
        ackermann(m - 1, 1)
    else: 
        ackermann(m - 1, ackermann(m, n - 1))

ackermann(3, 2) # 29
```

Functions are bits of code that can be invoked on a set of parameters. Once a function is called on some parameters, it
will return a value. Similar to variables, functions can be declared globally (at the top level) or local to a module,
type, or other function. Unlike variables, functions have their own local scope, which is valid between the start of the
function and when it returns.

## Simple Definitions

A function definition is made up of four parts: a _type annotation_, a _name_, a _parameter list_, and a _body_. The type
annotation specifies what type of value the function returns, the name specifies what the function is called in the current
scope, the parameter list declares the names and types of the function's parameters, and the body contains the code 
executed when the function is called. Either the type annotation or the body may be omitted, but not both. Syntactically,
this looks like `type name(parameters): body`.

```cl
int add_one(int x): x + 1
print(add_one(1))           # 2
print(add_one(add_one(2)))  # 4

int times(int x, int y): x * y
print(times(2, 3))          # 6
```

## Function Types

Functions have function types, made up of a return type and the types of its arguments. Function types aren't convertible
to other function types, even if their return types or parameters are compatible.

```cl
int times2(int x):
    x * 2

int(int) fn: times2
print(fn(4))                # 8

int add2(int y):
    y + 2
    
int(int)[2] fns: [times2, add2]
for f in fns:
    print(f(3))             # 6
                            # 5
```

## Parameterless Function

If a function is defined with no parameters, it implicitly takes a single nameless parameter of type `unit`. Likewise,
if a function is called on no parameters, it is passed an implicit single parameter of type `unit`.

```cl
int number(): 42
int(unit) int_factory: number
print(int_factory())        # 42

unit u: ()
print(int_factory(u))       # 42
```

## Indented Function Body

For function bodies containing more than one statement, an indented block should be used. The function body will continue
until it reaches a non-empty line with fewer leading spaces than the first line of its body.

```cl
int do_stuff(int x):
    int y: x + 1
    y *= 2
    return x + y

print(do_stuff(7))          # 23
```

## Undefined Functions

Functions can be defined without a body, representing a function that is known to exist externally to the current compilation
unit. Functions defined in this way are required to have a return type annotation, since it can't be inferred from the
nonexistent body.

```cl
void foo(int x, int y)
```