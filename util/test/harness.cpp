#include "util/test/harness.h"

const i8* current_test_name;

map<const i8*, void(*)(TestResults&)> test_map;
vec<entry<const i8*, void(*)(TestResults&)>> test_list;