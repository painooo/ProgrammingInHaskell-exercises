Exercises #2
1. Work through examples
        Done
2. Parenthesise following expressions
        2A. 2^3*4 -> (2^3) * 4
        2B. 2*3+4*5 -> (2*3) + (4*5)
        2C. 2+3*4^5 -> 2 + (3 * (4^5))
3. Identify errors
        Original:
 n' = a `div` length xs 
       where
       a = 10
      xs = [1,2,3,4,5]
        Solution:

> n = a `div` length xs
>       where
>             a = 10
>             xs = [1,2,3,4,5]

4. Redefine `last` w/ other library functions

> last' :: [a] -> a
> last' xs = head (reverse xs)

5. Redifine `init` w/ other library functions

> init' :: [a] -> [a]
> init' [] = []
> init' xs = reverse ( tail (reverse xs))