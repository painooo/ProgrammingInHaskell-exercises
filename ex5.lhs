Exercises #5

> import Data.Char
> import Data.List


1. w/ list comprehension give an exp that calculates sum of 1^2 + 2^2 + .. 100^2

> sumSquare :: Int
> sumSquare = sum [n^2 | n <- [2..100]]

2. define a function grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid of a given size.

> grid :: Int -> Int -> [(Int, Int)]
> grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

3. w/ list comprehension define square :: Int -> [(Int,Int)] which returns coords of a square excluding 0,0 and (n,n)

> square :: Int -> [(Int, Int)]
> square n = [(x,y) | (x,y) <- grid n n, x /= y]

4. define replicate' :: Int :: a -> [a] w/ list comprehension

> replicate' :: Int -> a -> [a]
> replicate' n v = [v | _ <- [1..n]]

5. define pythag :: int -> [(Int, Int, Int)] w/ list comprehension

> pythag :: Int -> [(Int, Int, Int)]
> pythag n = [(x,y,z)| x <- list, y <- list, z <- list, x^2 + y^2 == z^2]
>       where list = [1..n]

6. Define perfect :: Int -> [Int] which returns numbers which equal sum of it's factors excluding self

> factors :: Int -> [Int]
> factors n = [x | x <- [1..n], mod n x == 0]

> perfect :: Int -> [Int]
> perfect n = [x | x <- [1..n], sum (take (length (factors x) - 1) (factors x)) == x]

Kinda infuriating that I can't figure out how to define a var to factors x

7. w/ list comprehensions show how two generators can be condensed into one w/ two comprehensions

> gen xs = [(x,y) | x <- [0..5], y <- [0..5]]
> gen' xs =  concat [[(x,y) |y <- [0..5]] |x <- [0..5]]

8. Redefine positions w/ find

> position :: Char -> [Char] -> [Int]
> position c xs = [i | (x,i) <- zip xs [0..], x == c]

> position' c xs = [i | (x,i) <- zip xs [0..], find (== c) [x] == Just c]

9. define scalarproduct :: [Int] -> [Int] -> Int which returns the scalar product of the two lists w/ list comprehensions

> scalarproduct :: [Int] -> [Int] -> Int
> scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

10. Modify ceasar cipher to support capital characters

> chr2int :: Char -> Int
> chr2int c = ord c - ord 'a'
> int2chr :: Int -> Char
> int2chr n = chr (ord 'a' + n)
> lower :: Char -> Char
> lower c 
>       | chrnum >= chr2int 'a' && chrnum <= chr2int 'z' = int2chr chrnum
>       | otherwise = c
>       where chrnum = chr2int c + 32
> upper :: Char -> Char
> upper c 
>       | lower c == c = int2chr ((-32)+chr2int c)
>       | otherwise = c
> shift :: Int -> Char -> Char
> shift n c = int2chr (mod (chr2int c + n) 26)
> shiftCap :: Int -> Char -> Char
> shiftCap n c = upper (shift n (lower c))
> encode :: Int -> [Char] -> [Char]
> encode n s = [if lower c == c then shift n c else shiftCap n c | c <- s]