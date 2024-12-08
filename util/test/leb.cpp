#include "util/test/harness.h"
#include "util/io.h"

void writeULEB(slice<i8> buffer, u64 u) {
    format(buffer, uleb(u));
}

u64 readULEB(slice<i8> buffer) {
    uleb u;
    parse(buffer, u);
    return u.value;
}

void writeLEB(slice<i8> buffer, i64 u) {
    format(buffer, leb(u));
}

i64 readLEB(slice<i8> buffer) {
    leb u;
    parse(buffer, u);
    return u.value;
}

TEST(uleb_small) {
    array<i8, 16> buffer;
    writeULEB(buffer, 42);
    ASSERT_EQUAL(readULEB(buffer), 42);
}

TEST(leb_small_positive) {
    array<i8, 16> buffer;
    writeLEB(buffer, 42);
    ASSERT_EQUAL(readLEB(buffer), 42);
}

TEST(leb_small_negative) {
    array<i8, 16> buffer;
    writeLEB(buffer, -42);
    ASSERT_EQUAL(readLEB(buffer), -42);
}

TEST(uleb_medium) {
    array<i8, 16> buffer;
    writeULEB(buffer, 6561);
    ASSERT_EQUAL(readULEB(buffer), 6561);
}

TEST(leb_medium_positive) {
    array<i8, 16> buffer;
    writeLEB(buffer, 6561);
    ASSERT_EQUAL(readLEB(buffer), 6561);
}

TEST(leb_medium_negative) {
    array<i8, 16> buffer;
    writeLEB(buffer, -6561);
    ASSERT_EQUAL(readLEB(buffer), -6561);
}

TEST(uleb_large) {
    array<i8, 16> buffer;
    writeULEB(buffer, 987654321);
    ASSERT_EQUAL(readULEB(buffer), 987654321);
}

TEST(leb_large_positive) {
    array<i8, 16> buffer;
    writeLEB(buffer, 987654321);
    ASSERT_EQUAL(readLEB(buffer), 987654321);
}

TEST(leb_large_negative) {
    array<i8, 16> buffer;
    writeLEB(buffer, -987654321);
    ASSERT_EQUAL(readLEB(buffer), -987654321);
}

TEST(uleb_zero) {
    array<i8, 16> buffer;
    writeULEB(buffer, 0);
    ASSERT_EQUAL(readULEB(buffer), 0);
}

TEST(leb_zero) {
    array<i8, 16> buffer;
    writeLEB(buffer, 0);
    ASSERT_EQUAL(readLEB(buffer), 0);
}

TEST(uleb_max) {
    array<i8, 16> buffer;
    writeULEB(buffer, 0xffffffffffffffffull);
    ASSERT_EQUAL(readULEB(buffer), u64(0xffffffffffffffffull));
}

TEST(leb_max) {
    array<i8, 16> buffer;
    writeLEB(buffer, 0x7fffffffffffffffll);
    ASSERT_EQUAL(readLEB(buffer), i64(0x7fffffffffffffffll));
}

TEST(leb_min) {
    array<i8, 16> buffer;
    writeLEB(buffer, -0x8000000000000000ll);
    ASSERT_EQUAL(readLEB(buffer), i64(-0x8000000000000000ll));
}
