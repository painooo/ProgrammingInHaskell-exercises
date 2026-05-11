Exercises #3
1. Types of following values
        [Char]
        (Char, Char, Char)
        [(Bool, Char)]
        ([Bool],[Char])
        [[a] -> [a]]
2. Write down definitions of types

> bools :: [Bool]        
> bools = [True]

> nums :: [[Int]]
> nums = [[1]]

> add :: Int -> Int -> Int -> Int
> add x y z = x + y + z

> copy :: a -> (a,a)
> copy x = (x,x)

> apply :: (a -> b) -> a -> b
> apply x y = x y

  ^^^^^^^^^^^^^^^ wut?
3. Find types of following functions

> second :: [a] -> a
> second xs = head (tail xs)

> swap :: (a,b) -> (b,a)
> swap (x,y) = (y,x)

> pair :: a -> b -> (a,b)
> pair x y = (x,y)

> double :: Num a => a -> a
> double x = x*2

> palindrome :: Eq a => [a] -> Bool  
> palindrome xs = reverse xs == xs

4. Check answers ^^
        Done.
5. (Paraphrased)
        A. Why can't use Eq class for general use? 
                Because generally one can't compare two different types ("a" == 1 won't work)
        B. When to use?
                When checking equality w/ (==) or (/=)
