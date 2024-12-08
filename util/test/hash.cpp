#include "util/test/harness.h"
#include "util/hash.h"

TEST(hash_empty_table) {
    set<u32> table;
    ASSERT(table.size() == 0);
    ASSERT(!table.contains(0));
    ASSERT(!table.contains(1));
    ASSERT(!table.contains(0xffffffffu));
}

TEST(hash_table_insert) {
    set<u32> table;
    ASSERT(table.size() == 0);
    table.insert(1);
    ASSERT(table.size() == 1);
    ASSERT(table.contains(1));
    ASSERT(table.find(1) != table.end());
    ASSERT(*table.find(1) == 1);
}