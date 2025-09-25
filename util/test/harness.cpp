#include "util/test/harness.h"

const_slice<i8> current_test_name;

map<const_slice<i8>, void(*)(TestResults&)> test_map;
vec<entry<const_slice<i8>, void(*)(TestResults&)>> test_list;