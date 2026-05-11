Exercises #4
1. Define a function halve :: [a] -> ([a],[a]) which splits an even-lengthed into two

> halve :: [a] -> ([a], [a])
> halve xs = (take half xs, reverse (take half (reverse xs)))
>       where half = length xs `div` 2

2. Define a function third :: [a] -> a which takes third element which contains atleast 3 elements using head, tail, !! indexing, and pattern matching

> third :: [a] -> a
> third xs = head (tail (tail xs))
> third xs = xs !! 2
> third [_, _, x] = x

3. Define safetail :: [a] -> [a] which is similar to tail, but transforms empty list to itself use mappings tail and null
Create using conditional statements, guarded equations, and pattern matching

> safetail :: [a] -> [a]
> safetail xs = if null xs then [] else tail xs
> safetail xs 
>       | (null xs) = []
>       | otherwise = tail xs
> safetail [] = []
> safetail (_:xs) = xs

4. Show how || can be defined in 4 different ways

> (||) :: Bool -> Bool -> Bool
> True || True = True
> False || True = False
> True || False = False
> False || False = False

5. w/o library functions: show how following pattern matching definitions can be formalised w/ conditionals
        True && True = True => a && b = if a == b then a else False
        _ && _ = False = > a && b = if a == _ then False else if b == _ then False  
6. Same as 5
        True && b = b => a && b = if a then b else False
        False && _ = False => Same as above
7. Show following with lambda expressions

> mult :: Int -> Int -> Int -> Int
> mult x y z = x*y*z
> mult' :: Int -> (Int -> (Int -> Int))
> mult' x = \y -> (\z -> x*y*z) 

8. Luhn algorithm
        A. Define luhnDouble which doubles a digits & subtracts 9 if is bigger than 9

> luhnDouble :: Int -> Int
> luhnDouble x
>       | double > 9 = double - 9
>       | otherwise = double
>       where double = 2 * x

        B. Define luhn w/ luhnDouble

> luhn :: Int -> (Int -> (Int -> (Int -> Bool)))
> luhn a b c d = (\n -> mod n 10 == 0) (sum ([luhnDouble x | x <- [a,c]] ++ [d,b]))

Dunno why, but seeing luhnDouble a + b + luhnDouble c + d really bothered me