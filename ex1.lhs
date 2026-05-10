Exercises #1
1. Give another possible calculation for result of double (double 2)
  1. double (2+2)
  2. (2+2) + (2+2)
  3. 4 + 4
  4. 8
2. Show that sum [x] = x for any number x
  1. sum[x] = x + sum([])
    or x + 0
    using identity property of addition this means it is equal to x
3. Define a function 'product' which produces product of a list of Int (ex: [2,3,4]=24)

> product :: [Int] -> Int
> product [] = 1
> product (x:xs) = x * Main.product xs

4. How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?
  swapping definition of smaller and larger in the 'where' keyword while leaving everything else the same
5. What would be the effect of replacing <= by < in the original definition of qsort? Hint: consider the example qsort [2,2,3,1,1]
  then x would be included and duplicated in subsequent calculations of the larger halve