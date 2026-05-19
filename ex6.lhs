Exercises #6

1. How does recursive version of factorial function behave w/ a negative number, modify to prohibit negative numbers
        It causes the function to run forever as there is it is never 0 or 1

> fib :: Int -> Int
> fib 0 = 0
> fib 1 = 1
> fib n | n > 0 = fib(n-1) + fib(n-2)
>       | otherwise = 0

2. Define a recursive function sumdown :: Int -> Int which returns the sum of all values down to zero

> sumdown :: Int -> Int
> sumdown n | n > 0 = n + sumdown (n - 1)
>           | otherwise = 0

3. Define a ^ operator for non negative numbers

> (^) :: Int -> Int -> Int
> (^) _ 0 = 1
> (^) n m = n * (n Main.^ (m - 1))

4. Define recursive function euclid :: Int -> Int -> Int which implements Euclid's algorithmn for calculating greatest common divisors of two non-negative integers

> euclid :: Int -> Int -> Int
> euclid n 0 = n
> euclid n m | n == m = n
>            | (m-n) < 0 = 1
>            | otherwise = euclid n (m-n)

5. w/ recursive functions in this chapter (6) show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated

> length' :: [a] -> Int
> length' [] = 0
> length' (_:xs) = 1 + length' xs

length' [1,2,3]
pattern matches [2,3]
1 + length' [2,3]
pattern matches [3]
1 + (1 + length' [3])
pattern matches [] which is 0
1 + (1 + (1 + 0))
All adds up to 3

> drop' :: Int -> [a] -> [a]
> drop' _ [] = []
> drop' 0 xs = xs
> drop' n (x:xs) = drop' (n-1) xs

drop' 3 [1,2,3,4,5]
Evaluates as
drop' 2 [2,3,4,5]
Evaluates as
drop' 1 [3,4,5]
Evaluates as
drop' 0 [4,5]
Pattern matches 0 and returns list as [4,5]

> init' :: [a] -> [a]
> init' [] = []
> init' [_] = []
> init' (x:xs) = x : init' xs

init' [1,2,3]
Evaluates as
1 : init' [2,3]
Evaluates as
1 : 2 : init' [3]
Pattern matches with single element which returns []
1 : 2 : []
or
[1,2]

6. Define following w/ recursion

> and' :: [Bool] -> Bool
> and' [False] = False
> and' [n] = n
> and' (x:xs) = x && and' xs

> concat' :: [[a]] -> [a]
> concat' [] = []
> concat' [n] = n
> concat' (x:xs) = x ++ concat' xs

> replicate' :: Int -> a -> [a]
> replicate' 0 _ = []
> replicate' n x = x : replicate' (n-1) x

> (!!) :: [a] -> Int -> a
> (x:_) !! 0 = x
> (_:xs) !! n = xs Main.!! (n-1)

> elem' :: Eq a => a -> [a] -> Bool
> elem' _ [] = False
> elem' n (x:xs) | n == x = True
>               | otherwise = elem' n xs

> merge :: Ord a => [a] -> [a] -> [a]
> merge [] xs = xs
> merge ns [] = ns
> merge (n:ns) (x:xs) | n >= x = x : merge (n:ns)  xs
>                     | otherwise = n : merge ns (x:xs)

8. w/ merge define msort which Merge Sorts

> halve :: [a] -> ([a],[a])
> halve xs = (take half xs, drop half xs)
>       where half = length xs `div` 2
> msort :: Ord a => [a] -> [a]
> msort [] = []
> msort [n] = [n]
> msort xs = merge (msort (fHalf split)) (msort (sHalf split))
>       where
>        fHalf (x,_) = x 
>        sHalf (_, y) = y
>        split = halve xs

9. Resursively construct functions that...
        A. calculate sum of a list of numbers

> sum' :: [Int] -> Int
> sum' [] = 0
> sum' (x:xs) = x + sum' xs

        B. Take a given number of items from the start of a list

> take' :: Int -> [a] -> [a]
> take' 0 n = []
> take' n (x:xs) = x : take' (n-1) xs

        C. Select the last element of a non-empty list

> last' :: [a] -> a
> last' [n] = n 
> last' (n:ns) = last' ns 