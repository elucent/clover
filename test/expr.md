# Expressions

```
if (3 + 4) ** 2 == 49 and true == not false:
    print("Expressions are awesome!")
```

An expression in Clover is any code that evaluates to a value. This includes most operators, function calls, and constants.

# Constants

Constants are literal values that evaluate to themselves. Constants are immutable, and may or may not live in memory.

## Integer Constants

An integer constant is made up of one or more decimal digits (`0123456789`, or any character in the Unicode "decimal number"
category) followed by a separator. It represents a whole, positive number (negative numbers can be achieved using the minus
sign, i.e. `-42`). Integer constants have the type `iconst`, which is compatible with and convertible to all other numeric
types.

```cl
print(3)            # 3
print(04)           # 4
print(000000000)    # 0
print(1239)         # 1239
```

## Floating-point Constants

A floating-point constant is made up of one or more decimal digits (as described in [Integer Constants](#integer-constants)),
followed by a dot (`.`), followed by zero or more decimal digits and then a separator. Floating-point numbers represent
whole or fractional quantities represented using the IEEE-754 floating-point format. Floating-point constants have the type
`fconst`, which is compatible with and convertible to all floating-point types.

```cl
print(3.0)          # 3.0
print(3.)           # 3.0
print(3.000000000)  # 3.0
print(0.5)          # 0.5
print(1.501)        # 1.501
print(0.)           # 0.0
```

## Boolean Constants

Booleans are values that are either true or false, and a boolean constant is either the literal token `true` or `false`. All
boolean constants have the `bool` type.

```cl
print(true)         # true
print(false)        # false
```

## Char Constants

Character constants are made up of a single character or escape sequence (such as `\n` or `\t`) enclosed in single quotes `'...'`.
Each constant represents a single unicode code point. Character constants have type `char`, which stores the code point as a 32-bit
integer.

```cl
print('a')          # a
print('b')          # b
print('0')          # 0
print('^')          # ^
print('\\')         # \
print('λ')          # λ
print('🦝')        # 🦝
```

## String Constants

String constants are made up of any number of characters or escape sequences enclosed in double quotes `"..."`. Each constant
represents a sequence of unicode characters, encoded in UTF-8 as a byte string. String constants also contain a size, stored
as a pointer-size integer, representing how many bytes are contained within the string. String constants have type `string`.

```cl
print("abc")            # abc
print("")               # 
print("0 1 2 ^")        # 0 1 2 ^
print("こんにちは")      # こんにちは
print("i ❤️ ny")        # i ❤️ ny
```

# Arithmetic

Arithmetic expressions operate over numbers - integers and floats. The types of the operands, if there are multiple, must be
unifiable, and the type of the overall expression is whichever type is most general.

## Negation

Negation is a unary operator that negates its operand. Negation is right-associative, with a precedence higher than postfix
increment and decrement.

```cl
print(-1)           # -1
print(-143)         # -143
print(-0)           # 0
print(- -3)         # 3
```

## Positive

The positive sign is a unary operator that returns its operand. Positivity is right-associative, with the same precedence
as negation.

```cl
print(+12)          # 12
print(+-41)         # -41
print(-+9)          # -9
print(+ +3)         # 3
print(+0)           # 0
```

## Addition

Addition is a binary operator that computes the sum of two operands. Addition is left-associative, with a 
precedence lower than multiplication but higher than bitwise logic, and equal to subtraction.

```cl
print(1 + 2)        # 3
print(3 + 4 + 5)    # 12
print(1.4 + 0.5)    # 1.9
print(4 + 2.5)      # 6.5
```

## Subtraction

Subtraction is a binary operator that computes the difference between two operands. Subtraction is left-associative, with the
same precedence as addition.

```cl
print(5 - 3)        # 2
print(1 - 0)        # 1
print(6 - 9)        # -3
print(5.5 - 7.8)    # -2.3
print(9 - 4.6)      # 4.4

print(4 - 3 + 2)    # 3
print(4 - 3 * 2)    # -2
```

## Multiplication

Multiplication is a binary operator that computes the product of two operands. Multiplication is left-associative, with a 
precedence lower than exponentiation but higher than addition and subtraction.

```cl
print(1 * 1)        # 1
print(0 * 2)        # 0
print(0.5 * 2)      # 1.0
print(10 * 54)      # 540
print(2 * -2)       # -4
print(-2 * 2)       # -4
print(-3 * -3)      # 9
print(-3.2 * 2.5)   # -8.0

print(2 + 4 * 4)    # 18
print((2 + 4) * 4)  # 24
print(2 * 4 + 4)    # 12
```

## Division

Division is a binary operator that computes the quotient of two operands. Division is left-associative, with the same 
precedence as multiplication. Integer division rounds towards zero, while floating-point division permits fractions.
Division by zero for integer or floating-point numbers is undefined erroneous behavior, generally a system exception.

```cl
print(4 / 2)        # 2
print(51.0 / 3.0)   # 17.0
print(9 / -3)       # -3
print(-16 / -4)     # 4
print(5 / 2)        # 2
print(5 / 2.0)      # 2.5
print(-5 / 2)       # -2

print(4.0 / 2 * 3)  # 6.0
print(2 * 3 / 4.0)  # 1.5
print(360 / 24 / 3) # 5
```

## Remainder

Remainder is a binary operator that computes the signed remainder from dividing two operands. Remainder is 
left-associative, with the same precedence as multiplication and division. Division by zero, even just to take the
remainder, is undefined erroneous behavior and generally a system exception.

```cl
print(5 % 3)        # 2
print(-5 % 3)       # -2

print(19 % 10 / 3)  # 3
print(8 - 3 % 2)    # 7
print(25 % 7 * 4)   # 16
```

## Exponent

Exponentiation is a binary operator that raises the left operand to the power of the right. Exponentiation is
right-associative, with a lower precedence than prefix and postfix operations but higher precedence than multiplication.

```cl
print(3 ** 2)           # 9
print(36 ** 0.5)        # 6.0
print(8 ** (1. / 3.))   # 2.0
print(2 ** -1)          # 0
print(2.0 ** -1)        # 0.5
print(-7 ** 2)          # 49
print(-7 ** 3)          # -343

print(4 * 2 ** 3)       # 32
print((4 * 2) ** 3)     # 512
```

# Bitwise Logic

Bitwise logic expressions operate over integers. The types of the operands, if there are multiple, must be
unifiable, and the type of the overall expression is whichever type is most general.

## Bitwise NOT

Bitwise NOT is a unary operator that flips the bits of its operand. Bitwise NOT is right-associative, and has the
same precedence as the negative sign.

```cl
print(~0)               # -1
print(~-2)              # 1
print(~~463)            # 463
```

## Bitwise AND

Bitwise AND is a binary operator that performs an AND operation between each of the bits of its operands, returning
the resulting bit sequence. Bitwise AND is left-associative, and has a lower precedence than addition, but higher
precedence than bitwise XOR.

```cl
print(1 & 1)            # 1
print(1 & 0)            # 0
print(0 & 1)            # 0
print(0 & 0)            # 0

print(255 & 15)         # 15
print(44 & 7)           # 4
```

## Bitwise XOR

Bitwise XOR is a binary operator that performs an XOR operation between each of the bits of its operands, returning
the resulting bit sequence. Bitwise XOR is left-associative, and has a lower precedence than bitwise AND, but higher
precedence than bitwise OR.

```cl
print(1 ^ 1)            # 0
print(1 ^ 0)            # 1
print(0 ^ 1)            # 1
print(0 ^ 0)            # 0

print(53 ^ 0)           # 53
print(53 ^ 53)          # 0
```

## Bitwise OR

Bitwise OR is a binary operator that performs an OR operation between each of the bits of its operands, returning the
resulting bit sequence. Bitwise OR is left-associative, and has a lower precedence than bitwise XOR, but higher
precedence than comparison operations.

```cl
print(1 | 1)            # 1
print(1 | 0)            # 1
print(0 | 1)            # 1
print(0 | 0)            # 0

print(61 & 240 | 61 & 15)   # 61
```

## Bitwise Left Shift

Bitwise left shift is a binary operator that shifts the bits of its left operand towards the most-significant bit by
the right operand. Bits pushed past the MSB are truncated, and the value is padded out with zeroes on the right. Bitwise
left shift is left-associative, and has the same precedence as exponentiation.

```cl
print(7 << 0)           # 7
print(7 << 1)           # 14
print(7 << 2)           # 28
```

## Bitwise Right Shift

Bitwise right shift is a binary operator that shifts the bits of its left operand towards the least-significant bit by
the right operand. Bits pushed past the LSB are truncated, and the value is padded out with zeroes on the left. Bitwise
right shift is left-associative, and has the same precedence as exponentiation and left shift.

```cl
print(4 >> 1)           # 2
print(4 >> 0)           # 4
print(15 >> 2)          # 3
```

# Boolean Logic

Boolean logic expressions operate over booleans, and always return booleans.

## Logical NOT

Logical NOT is a unary operator that returns the logical complement of its operand. NOT is right-associative, and has the
same precedence as bitwise NOT and negation. 

```cl
print(not true)         # false
print(not false)        # true
print(not not true)     # true
```

## Logical AND

Logical AND is a binary operator that returns the logical conjunction of its operands. AND always evaluates its left
operand first, and doesn't evaluate the right operand if the left operand is false. AND is left-associative, and has lower
precedence than equality operators but higher precedence than logical OR.

```cl
print(true and true)    # true
print(true and false)   # false
print(false and true)   # false
print(false and false)  # false

print(true and true and false)  # false
print(true and not false)       # true

bool foo():
    print(1)
    true
false and foo()         

bool bar():
    print(2)
    true
true and bar()          # 2
```

## Logical OR

Logical OR is a binary operator that returns the logical disjunction of its operands. OR always evaluates its left
operand first, and doesn't evaluate the right operand if the left operand is true. OR is left-associative, and has lower
precedence than logical AND but higher precedence than assignment.

```cl
print(true or true)     # true
print(true or false)    # true
print(false or true)    # true
print(false or false)   # false

print(true or false or false)               # true
print(false and true or true and true)      # true
print(false and (true or true) and true)    # false

print(false or not true)        # false
print(not false or false)       # true

bool foo():
    print(1)
    true
true or foo()

bool bar():
    print(2)
    true
false or bar()          # 2
```

# Comparisons

Comparison operators compare their operands with regards to some kind of ordering, returning a boolean if the comparison
holds. The two operands of a comparison must be unifiable with each other, and the overall expression always returns
a boolean. Clover currently supports the following four comparison operators:

| Operator | Name | Description |
|---|---|---|
| `<` | Less than | True if the left operand is strictly less than the right. |
| `<=` | Less or equal | True if the left operand is less than or equal to the right. |
| `>` | Greater than | True if the left operand is strictly greater than the right. |
| `>=` | Greater or equal | True if the left operand is greater than or equal to the right. |

## Comparing Numbers

All numeric types (integers, floats, etc) can be compared, in which case they are ordered by numerical value.

```cl
print(3 < 5)            # true
print(-19. < -19.)      # false
print(-2 <= 6)          # true
print(-3 <= -3)         # true

print(2 > 1.99)         # true
print(0.501 >= 0.5)     # true
print(5 > 5)            # false
print(-5 >= 0)          # false
```

## Comparing Characters

Characters can be compared with other characters, in which case they are ordered by codepoint.

```cl
print('a' < 'b')        # true
print('Z' <= 'Q')       # false
print('Ω' < 'Ψ')        # false
print('$' <= '🐠')      # true

print('9' > '9')        # false
print('₂' >= '₂')       # true
print('Ж' >= '☃️')       # false
print('$' > ' ')        # true
```

## Comparing Strings

Strings can be compared with other strings. Strings are ordered primarily lexicographically, but if the two
strings have different lengths, they are compared lexicographically up to the smaller length and then ordered
by length in code points.

```cl
print("apple" < "box")  # true
print("ate" <= "at")    # false
print("151" < "151")    # false
print("" <= "")         # true

print("💀💀" > "aaaa") # true
print("Δύο" > "Τρία")   # false
print("𝕮𝖑𝖔𝖛𝖊𝖗" >= "Clover") # true
print(" " >= "  ")      # false
```

## Comparing Pointers

Pointers can be compared with other pointers of the same type, in which case they are ordered by memory address.

```cl
int[8] arr: [1, 2, 3, 4, 5, 6, 7, 8]
print(&arr[0] < &arr[1])    # true
print(&arr[5] <= &arr[4])   # false
print(&arr[3] < &arr[3])    # false
print(&arr[6] <= &arr[6])   # true

print(&arr[2] > &arr[0])    # true
print(&arr[7] >= &arr[3])   # true
print(&arr[1] > &arr[1])    # false
print(&arr[0] >= &arr[5])   # false
```
