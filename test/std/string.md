# Strings

## Iteration

```cl
use "std/string"

for c in "abc": # a
    print(c)    # b
                # c

for c in "":
    print(c)

var start: "abc".iter()
print(start.read())     # a

var b: start.next()
var c: start.next().next()
start = start.next().next().next()

print(start.empty())    # true
print(b.empty())        # false
for c in b:             # b
    print(c)            # c
print(c.empty())        # false
print(c.read())         # c
```

## First

```cl
use "std/string"

print("abc".first())    # a
print("123".first())    # 1
print("%".first())      # %
print("Œ".first())      # Œ
print("⁇".first())      # ⁇
print("✨".first())     # ✨
```

## Last

```cl
use "std/string"

print("abc".last())    # c
print("123".last())    # 3
print("%".last())      # %
print("Œ".last())      # Œ
print("⁇".last())      # ⁇
print("✨".last())     # ✨
```

## Length

```cl
use "std/string"

print("".length())      # 0
print("a".length())     # 1
print("abc".length())   # 3
print("0 1 2".length()) # 5
print("Ιλιάς".length()) # 5
print("🌲👀".length()) # 2
```

## Find Character

```cl
use "std/string"

print("abc".findc('a'))  # 0
print("abc".findc('c'))  # 2
print("abc".findc('e'))  # -1
print("".findc('a'))     # -1
```

## Find String

```cl
use "std/string"

print("abc".find("a").read())   # 0
print("abc".find("bc").read())  # 1
print("".find("abc").read())    # -1

var findempty: "abc".find("")
print(findempty.read())         # 0
print(findempty.next().read())  # 0

var findthe: "the cat and the dog".find("the")
print(findthe.read())               # 0
print(findthe.next().read())        # 12
print(findthe.next().next().read()) # -1

print("".find("").read())       # 0
```

## Contains String

```cl
use "std/string"

var text: "the lion, the witch, and the wardrobe"
print(text.contains("lion"))        # true
print(text.contains("the"))         # true
print(text.contains("wardrobe"))    # true
print(text.contains("tiger"))       # false

print(text.contains("wardrobe "))   # false
print(text.contains(" wardrobe"))   # true

print(text.contains(""))            # true
print("".contains(""))              # true
print("".contains(" "))             # false
```

## Split by String

```cl
use "std/string"

print("abc".split(" ").read())          # abc
print("abc".split(" ").next().empty())  # true
print("abc".split("").read())           # 
print("abc".split("").next().read())    # 

var text: "the lion, the witch, and the wardrobe"
for s in text.split(", "):      # the lion
    print(s)                    # the witch
                                # and the wardrobe
```

## To Char Slice

```cl
use "std/string"

var empty: "".chars()
print(|empty|)          # 0

var one: "a".chars()
print(|one|)            # 1
print(one[0])           # a

var abc: "abc".chars()
print(|"abc"|)          # 3
print(|abc|)            # 3
for c in abc:           # a
    print(c)            # b
                        # c

var twobyte: "Ιλιάς".chars()
print(|"Ιλιάς"|)        # 10
print(|twobyte|)        # 5
print(twobyte[0])       # Ι
print(twobyte[1])       # λ
print(twobyte[4])       # ς

var threebyte: "₁₂₃".chars()
print(|"₁₂₃"|)          # 9
print(|threebyte|)      # 3
print(threebyte[0])     # ₁
print(threebyte[2])     # ₃

var fourbyte: "😎👍".chars()
print(|"😎👍"|)          # 8
print(|fourbyte|)       # 2
print(fourbyte[0])      # 😎
print(fourbyte[1])      # 👍

var mixedbyte: "λambda".chars()
print(|"λambda"|)       # 7
print(|mixedbyte|)      # 6
print(mixedbyte[0])     # λ
print(mixedbyte[1])     # a
print(mixedbyte[5])     # a
```

## Append String

```cl
use "std/string"

print("hello".append(", world"))            # hello, world
print("cool".append(" ").append("beans"))   # cool beans
print("abc".append("").append(""))          # abc
print("".append("abc"))                     # abc
print("".append(""))                        #
```

## Repeat String

```cl
use "std/string"

print("a".repeat(1))    # a
print("".repeat(1))     #
print("".repeat(100))   # 
print("".repeat(0))     #
print("a".repeat(0))    #
print("a".repeat(2))    # aa
print("ab".repeat(4))   # abababab
```

## Join String

```cl
use "std/string"

print(["a"].join(""))           # a
print(["a", "b"].join(""))      # ab
print(["a", "b"].join(", "))    # a, b
print(["", "", ""].join("ab"))  # abab
string[] empty: []
print(empty.join("abc"))        # 
```