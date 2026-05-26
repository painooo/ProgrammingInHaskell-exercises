Exercises #8

1. Similar to add (in this chapter) define a mult :: Nat -> Nat -> Nat

> data Nat = Zero | Succ Nat
> add :: Nat -> Nat -> Nat
> add Zero m = m
> add (Succ n) m = Succ (add n m)
> mult :: Nat -> Nat -> Nat
> mult n Zero = Zero
> mult n (Succ m) = add n (mult n m)

2. With compare redefine occurs :: Ord a => a -> Tree a -> Bool for search trees

> data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
> occurs :: Ord a => a -> Tree a -> Bool
> occurs x (Leaf n) = x == n
> occurs x (Node l m r) | val == EQ = True
>                       | val == LT = occurs x l
>                       | otherwise = occurs x r
>         where val = compare x m

This is more efficient because we are checking less branches

3. Define a function which checks if a tree is balanced

> data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show
> len :: Tree' a -> Int
> len (Leaf' _) = 1
> len (Node' x y) = len x + len y
> isBalanced :: Tree' a -> Bool
> isBalanced (Leaf' _) = True
> isBalanced (Node' x y) = b1 == b2 || b1 + 1 == b2 || b1 == b2 + 1
>               where b1 = len x
>                     b2 = len y

4. Define a balance :: [a] -> Tree which takes a list and creates a balanced tree

> balance :: [a] -> Tree' a
> balance [x] = Leaf' x
> balance xs = Node' (balance l) (balance r)  
>       where half = length xs `div` 2
>             l = take half xs
>             m = xs !! half
>             r = drop half xs

5. Given type declaration define folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

> data Expr = Val Int | Add Expr Expr
> folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
> folde f _ (Val n) = f n
> folde f g (Add n m) = g (folde f g n) (folde f g m)

6. With folde define function eval :: Expr -> Int which simplifies expression down to integer and size :: Expr -> Int which counts amount of vals

> eval' :: Expr -> Int
> eval' = folde id (+)
> size :: Expr -> Int
> size = folde (\_ -> 1) (+)

7. Complete following instance declarations

> instance Eq a => Eq (Maybe a) where
>       Just a == Just b = not (a /= b)
>       Nothing == Nothing = not (Nothing /= Nothing)
> instance Eq a => Eq [a] where
>       [a] == [b] = and [ a==b |(a,b) <- zip [a] [b]]
>       [a] /= [b] = not ([a] == [b])

I do not know if these two declarations are correct

8. Extend tautology checker to support Or and Equiv

> data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Or Prop Prop | Imply Prop Prop | Equiv Prop Prop
> type Assoc k v = (k,v)
> type Map = [Assoc Char Bool]

> lookup' :: Char -> Map -> Bool
> lookup' k (x:xs) = (\(k',v) -> if k' == k then v else lookup' k xs) x

> eval :: Map -> Prop -> Bool
> eval _ (Const b) = b
> eval m (Var c) = lookup' c m
> eval m (Not n) = not (eval m n)
> eval m (And x y) = eval m x && eval m y 
> eval m (Or x y) = eval m x || eval m y
> eval m (Imply x y) = eval m (Or (Not x) y)
> eval m (Equiv x y) = eval x == eval y

> vars :: Prop -> [Char]
> vars (Var c) = [c]
> vars (Not p) = vars p
> vars (And p p') = vars p ++ vars p'
> vars (Or p p') = vars p ++ vars p'
> vars (Imply p p') = vars p ++ vars p'

> rmdup :: Eq a => [a] -> [a]
> rmdup [] = []
> rmdup (x:xs) = x : rmdup (filter (/=x) xs)

> allMaps :: [Char] -> [Map]
> allMaps xs = [zip xs [x == 1 | x <- binMap n] | n <- range]
>         where
>                 len = length xs
>                 total = fact len -- Permutation
>                 range = [0..total+1]
>                 binMap n = reverse (take len (reverse (int2bin n) ++ repeat 0))
> checkTautolo :: Prop -> Bool
> checkTautolo p = and [eval m p | m <- (allMaps . rmdup . vars) p]

9. Extend abstract machine to support multiplication

> data Expr = Val Int | Add Expr Expr | Mult Expr Expr

> value :: Expr -> Int
> value (Val n) = n
> value (Add a b) = value a + value b
> value (Mult a b) = value a * value b