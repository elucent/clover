bool startswith?(i8[] string, i8[] prefix):
    return false if |string| < |prefix|
    return false if string[i] != prefix[i] for i < |prefix|
    return true

bool endswith?(i8[] string, i8[] suffix):
    return false if |string| < |suffix|
    return false if string[|string| - i - 1] != suffix[|suffix| - i - 1] for i < |suffix|
    return true

type StringSplitter:
    i8[] string
    i8[] pattern
    i64 nextOffset

StringSplitter split(i8[] string, i8[] pattern):
    StringSplitter(string, pattern, -1)

StringSplitter lines(i8[] string):
    string.split("\n")

StringSplitter iter(StringSplitter iter):
    iter

i8[] read(StringSplitter* iter):
    if iter.nextOffset >= 0:
        return iter.string[:iter.nextOffset]
    for i < |iter.string|:
        bool found: true
        for j < |iter.pattern| if iter.string[i + j] != iter.pattern[j]:
            found = false then break
        if found:
            iter.nextOffset = i
            return iter.string[:i]
        if i == |iter.string| - 1:
            iter.nextOffset = i + 1
    return iter.string

i8[] read!(StringSplitter* iter):
    var result: iter.read(), bump: iter.nextOffset
    bump += |iter.pattern| if bump < |iter.string|
    iter.string = iter.string[bump:]
    iter.nextOffset = -1
    return result

StringSplitter next(StringSplitter iter):
    iter.read()
    var bump: iter.nextOffset
    bump += |iter.pattern| if bump < |iter.string|
    iter.string = iter.string[bump:]
    iter.nextOffset = -1
    return iter

bool done(StringSplitter iter):
    |iter.string| == 0
