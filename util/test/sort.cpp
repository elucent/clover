#include "util/test/harness.h"
#include "util/vec.h"
#include "util/sort.h"
#include "util/io.h"

template<typename T, typename... Args>
struct ArrayComparator : array<T, 1 + sizeof...(Args)> {
    ArrayComparator(const T& t, const Args&... args):
        array<T, 1 + sizeof...(Args)>(t, args...) {}
};

template<typename SliceLike, typename T, typename... Args>
bool operator==(const SliceLike& s, ArrayComparator<T, Args...> cmp) {
    constexpr u32 N = sizeof...(Args) + 1;
    const_slice<T> src = (const_slice<T>)s;
    if (src.size() != N)
        return false;
    for (u32 i = 0; i < N; i ++)
        if (src[i] != cmp[i])
            return false;
    return true;
}

#define array_eq == ArrayComparator

TEST(heapsort_empty) {
    vec<i32> nums;
    heapsort(nums);
    ASSERT(nums.size() == 0);
}

TEST(heapsort_single) {
    vec<i32> nums;
    nums.push(1);
    heapsort(nums);
    ASSERT(nums array_eq { 1 });
}

TEST(heapsort_inorder) {
    vec<i32> nums;
    nums.push(1, 2);
    heapsort(nums);
    ASSERT(nums array_eq { 1, 2 });
}

TEST(heapsort_random) {
    vec<i32> nums;
    nums.push(4, 1, 2, 3);
    heapsort(nums);
    ASSERT(nums array_eq { 1, 2, 3, 4 });
}

TEST(heapsort_reverse) {
    vec<i32> nums;
    nums.push(5, 4, 3, 2, 1);
    heapsort(nums);
    ASSERT(nums array_eq { 1, 2, 3, 4, 5 });
}

struct rng {
    u32 seed;

    inline rng(u32 s): seed(s * 3664215349u) {}

    inline u32 next() {
        return seed = seed * 1583904617u + 3301676347u;
    }
};

TEST(heapsort_inorder1000) {
    vec<i32> nums;
    for (u32 i = 0; i < 1000; i ++)
        nums.push(i);
    heapsort(nums);
    for (u32 i = 0; i < 1000; i ++)
        ASSERT(nums[i] == i);
}

TEST(heapsort_reverse1000) {
    vec<i32> nums;
    for (u32 i = 0; i < 1000; i ++)
        nums.push(999 - i);
    heapsort(nums);
    for (u32 i = 0; i < 1000; i ++)
        ASSERT(nums[i] == i);
}

TEST(heapsort_random1000) {
    vec<i32> nums;
    rng gen(31);
    for (u32 i = 0; i < 1000; i ++)
        nums.push(gen.next());
    heapsort(nums);
    for (u32 i = 1; i < 1000; i ++)
        ASSERT(nums[i] >= nums[i - 1]);
}

TEST(mergesort_empty) {
    vec<i32> nums;
    mergesort(nums);
    ASSERT(nums.size() == 0);
}

TEST(mergesort_single) {
    vec<i32> nums;
    nums.push(1);
    mergesort(nums);
    ASSERT(nums array_eq { 1 });
}

TEST(mergesort_inorder) {
    vec<i32> nums;
    nums.push(1, 2);
    mergesort(nums);
    ASSERT(nums array_eq { 1, 2 });
}

TEST(mergesort_random) {
    vec<i32> nums;
    nums.push(4, 1, 2, 3);
    mergesort(nums);
    ASSERT(nums array_eq { 1, 2, 3, 4 });
}

TEST(mergesort_reverse) {
    vec<i32> nums;
    nums.push(5, 4, 3, 2, 1);
    mergesort(nums);
    ASSERT(nums array_eq { 1, 2, 3, 4, 5 });
}

TEST(mergesort_inorder1000) {
    vec<i32> nums;
    for (u32 i = 0; i < 1000; i ++)
        nums.push(i);
    mergesort(nums);
    for (u32 i = 0; i < 1000; i ++)
        ASSERT(nums[i] == i);
}

TEST(mergesort_reverse1000) {
    vec<i32> nums;
    for (u32 i = 0; i < 1000; i ++)
        nums.push(999 - i);
    mergesort(nums);
    for (u32 i = 0; i < 1000; i ++)
        ASSERT(nums[i] == i);
}

TEST(mergesort_random1000) {
    vec<i32> nums;
    rng gen(31);
    for (u32 i = 0; i < 1000; i ++)
        nums.push(gen.next());
    mergesort(nums);
    for (u32 i = 1; i < 1000; i ++)
        ASSERT(nums[i] >= nums[i - 1]);
}

TEST(quicksort_empty) {
    vec<i32> nums;
    quicksort(nums);
    ASSERT(nums.size() == 0);
}

TEST(quicksort_single) {
    vec<i32> nums;
    nums.push(1);
    quicksort(nums);
    ASSERT(nums array_eq { 1 });
}

TEST(quicksort_inorder) {
    vec<i32> nums;
    nums.push(1, 2);
    quicksort(nums);
    ASSERT(nums array_eq { 1, 2 });
}

TEST(quicksort_random) {
    vec<i32> nums;
    nums.push(4, 1, 2, 3);
    quicksort(nums);
    ASSERT(nums array_eq { 1, 2, 3, 4 });
}

TEST(quicksort_reverse) {
    vec<i32> nums;
    nums.push(5, 4, 3, 2, 1);
    quicksort(nums);
    ASSERT(nums array_eq { 1, 2, 3, 4, 5 });
}

TEST(quicksort_inorder1000) {
    vec<i32> nums;
    for (u32 i = 0; i < 1000; i ++)
        nums.push(i);
    quicksort(nums);
    for (u32 i = 0; i < 1000; i ++)
        ASSERT(nums[i] == i);
}

TEST(quicksort_reverse1000) {
    vec<i32> nums;
    for (u32 i = 0; i < 1000; i ++)
        nums.push(999 - i);
    quicksort(nums);
    for (u32 i = 0; i < 1000; i ++)
        ASSERT(nums[i] == i);
}

TEST(quicksort_random1000) {
    vec<i32> nums;
    rng gen(31);
    for (u32 i = 0; i < 1000; i ++)
        nums.push(gen.next());
    quicksort(nums);
    for (u32 i = 1; i < 1000; i ++)
        ASSERT(nums[i] >= nums[i - 1]);
}