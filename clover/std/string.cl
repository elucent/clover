bool startswith?(i8[] string, i8[] prefix):
    return false if |string| < |prefix|
    return false if string[i] != prefix[i] for i < |prefix|
    return true

bool endswith?(i8[] string, i8[] suffix):
    return false if |string| < |suffix|
    return false if string[|string| - i - 1] != suffix[|suffix| - i - 1] for i < |suffix|
    return true