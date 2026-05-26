{- I used with exercise 9 -}

data Op = Add | Sub | Mult | Div
instance Show Op where 
        show Add = "+"
        show Sub = "-"
        show Mult = "*"
        show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mult _ _ = True
valid Div x y = mod x y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mult x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
        show (Val n) = show n
        show (App o x y) = side x ++ show o ++ side y
                where side (Val n) = show n
                      side x = "(" ++ show x ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) | valid o x y = [apply o x y]
                 | otherwise = []
        where x = head (eval l) 
              y = head (eval r)