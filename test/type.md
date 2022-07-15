# Types

```
type CountryCode: char[2]
type Address:
    string street, city
    CountryCode country
type Person:
    string name
    int age
    Address address

var alice: Person("Alice", 29, Address("1 Foo Street", "Barville", ['I', 'O']))
```

Types describe the properties of different kinds of information. Clover is a statically-typed language, so all Clover
expressions and declarations are required to be clearly typed at compile-time. To simplify writing Clover code, though,
Clover also has powerful type inference. If types aren't explicitly written out by the programmer, Clover can usually figure
them out without too much trouble.

# Primitive Types

Clover types are divided into two main categories: simpler primitive types, which are implemented by the language; and compound
types, which can be constructed out of primitive types to represent larger structures. We'll talk about primitive types first!

## Integers

Perhaps the simplest primitive type is the integer. In Clover, integers are whole numbers that can be either positive or negative,
within a range of possible values. This range is specified by the _width_ of the integer, in bits - the type `i8` is an 8-bit 
integer, and can represent 2^8 possible values, roughly evenly divided into positive and negative as the range `[-128, 127]`. 

Clover supports four default widths of integer: 8, 16, 32, and 64 bits.

```cl
i8 a: 100
i16 b: 10000
i32 c: 10000000
i64 d: 10000000000
print(a)        # 100
print(b)        # 10000
print(c)        # 10000000
print(d)        # 10000000000
```

Integers of a smaller width can be implicitly converted to integers of a larger width.

```cl
i8 x: 16
i16 y: x
i32 z: y
i64 w: z
print(w)        # 16
```

However, to convert from a larger integer to a smaller integer, an explicit cast must be performed.

```cl
i32 x: 100
i8 y: i8(x)
print(y)        # 100
```

In addition to integer types with explicit widths, Clover also defines a type called `int`. `int` is generally the same as the
largest integer type supported natively by the CPU, so you can think of it as the "recommended" integer type. For cases where you
just want an integer of a decent size, it's probably a good idea to use `int`.

```cl
int x: 1000
print(x)        # 1000
```

Finally, Clover also defines a type `iptr`. This is an integer type guaranteed to be wide enough to hold a memory address. Uniquely
among integer types, `iptr` can be cast to any pointer type, and all pointer types can be cast to `iptr`.

```cl
i32[2] i: [1, 2]
i32* p: &i[0]
print(*p)       # 1
iptr ip: iptr(p)
ip += 4
p = i32*(ip)
print(*p)       # 2
```

## Floats

In addition to integers, Clover supports two sizes of floating-point numbers. Floating-point is a numeric representation analogous
to scientific notation, and floating-point numbers can support both fractional quantities and very large numbers, albeit with limited
precision. Clover includes `f32` and `f64`, 32-bit and 64-bit floating-point number types respectively.

```cl
f32 x: 1.5, y: 0.000001, z: 12345678.0
f64 a: 1.5, b: 0.000001, c: 123456789000.0

print(x)    # 1.5
print(y)    # 0.000001
print(z)    # 12345678.0
print(a)    # 1.5
print(b)    # 0.000001
print(c)    # 123456789000.0
```

Like integers, floating-point numbers can be coerced to larger floating-point types, but going from a larger to smaller floating-point
type requires an explicit cast. Additionally, integers of all sizes may be coerced to any floating-point type, but the reverse requires
an explicit cast.

```cl
i8 x: 1

f32 a: x + 0.5
print(a)        # 1.5

f64 b: a - 1
print(b)        # 0.5

f32 c: f32(b) + 2
print(c)        # 2.5

int y: int(c - 0.5)
print(y)        # 2
```