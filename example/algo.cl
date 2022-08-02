type Range: int begin, end, incr

fun Range.iter(): this
fun Range.empty(): 
    if this.incr >= 0: return this.begin >= this.end
    else return this.begin <= this.end
fun Range.read(): this.begin
fun Range.next(): Range(this.begin + this.incr, this.end, this.incr)

# fun min(nums):
#     var min: nums[0]
#     for i in nums[1:]:
#         if i < min: min = i
#     return min
# 
# fun max(nums):
#     var max: nums[0]
#     for i in nums[1:]:
#         if i > max: max = i
#     return max

fun insort(T?[] nums):
    for i in Range(1, |nums|, 1):
        for j in Range(i, 0, -1):
            if nums[j - 1] > nums[j]:
                var t: nums[j]
                nums[j] = nums[j - 1]
                nums[j - 1] = t
            else: break

int[3] arr: [3, 2, 1]
insort(arr)
for i in arr: print(i)

# fun meld(int[] a, int[] b, int[] out):
#     int[] temp = new int[|out|]
#     int ai: 0, bi: 0, i: 0
#     while ai < |a| or bi < |b|:
#         if ai == |a|: 
#             temp[i ++] = b[bi ++]
#         else if bi == |b|: 
#             temp[i ++] = a[ai ++]
#         else if b[bi] < a[ai]: 
#             temp[i ++] = b[bi ++]
#         else:
#             temp[i] = a[ai ++]
#     for j in 0..|out|:
#         out[j] = temp[j]
# 
# fun sort(int[] nums):
#     int i: 1
#     while i < |nums|:
#         for j in 0 .. |nums| / i:
#             var begin: j * i * 2
#             var end: j
#             meld(nums[begin:begin + i], nums[begin + i:end])
#         i *= 2
