# Variables

```
int x: 3, y: 4, z: (x * x + y * y) ** 0.5
```

Variables are named, mutable values that live within a particular scope. Variables can be declared both globally, also
referred to as the top level, and local to functions, types, or modules. Variables generally represent locations in memory,
so they and their members are guaranteed to be addressible.

## Simple Definitions

A variable definition is made up of three parts: a _type annotation_, a _name_, and an _initial value_. Either the type
annotation or initial value may be omitted, but not both. Syntactically, this looks like `type name: initial`.

```cl
int x: 1
int y: 2
print(x + y)        # 3

char c1: 'a'
char c2: 'z'
print(c1 < c2)      # true

int[2] arr: [42, 21]
print(arr[0])       # 42
print(arr[1])       # 21
```

## Undefined Variables

If the variable name is omitted, the variable's value is initially undefined. The variable can then be assigned a value
later on.

```cl
int x
float y
x = 1
y = x / 2.
print(x)            # 1
print(y)            # 0.5
```

## Type Inference

If the type annotation is omitted, the keyword `var` must be used in its place. In this situation, the type of the variable is
inferred from context. Clover's type inference is a little more sophisticated than what you would find in most C-like languages,
and types are not inferred exclusively from an initial value. `var x: 1` does not necessarily mean `x` is an integer. Instead,
`x` is assigned a _type variable_, which is repeatedly expanded when `x` is used to accommodate all usages of the value. If no
type exists that can be used in all required scenarios, a type error is reported. Generally, you don't need to worry about type
variables, and there is no way to refer to them directly in the language. But they aren't perfect, so sometimes you may still
need to add an annotation to clarify your intent in some places.

```cl
var x: 1
var y: x
x += 0.5

print(x * 3)        # 4.5
print(y / 2)        # 0.5

var circ: x
x += circ

print(x)            # 3.0
```

## Multiple Definition

Multiple variables can be declared in a single definition statement, and initialized (or left uninitialized) separately. If a
type annotation is provided, it applies to all of the defined variables. Otherwise, each variable's type is inferred
independently.

```cl
float x: 0.5, y: 1.2, z: 5
print(z / 2)        # 2.5

int a, b: 2, c: 3, d
a = b
a += c
d = a
print(d)            # 5

var foo: 42, bar: "hello", baz: true, quux: [d, foo]
print(foo)          # 42
print(bar)          # hello
print(baz)          # true

for i in quux:      # 5
    print(i)        # 42
```