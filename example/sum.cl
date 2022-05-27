var items: [1, 2, 3, 4, 5, 6, 7, 8]

fun sum(int[] nums):
    int sum: 0
    for x in nums:
        sum += x
    print(sum)

sum(items[:4])  # 1 + 2 + 3 + 4 = 10
