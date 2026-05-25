> import Data.Char

Exercises #7

1. Show how [f x | x <- xs, p x] can be re-expressed w/ higher-ordered function using map and filter

> filterAndApply :: Eq a => (a -> b) -> (a -> Bool) -> [a] -> [b]
> filterAndApply f p xs = map f (filter p xs)

2. Define the following higher-order library functions

> all' :: (a -> Bool) -> [a] -> Bool
> all' p xs = length xs == length (filter p xs)

> any' :: (a -> Bool) -> [a] -> Bool
> any' p xs = not (null (filter p xs))

> takeWhile' :: (a -> Bool) -> [a] -> [a]
> takeWhile' _ [] = []
> takeWhile' p (x:xs) | p x = x : takeWhile' p xs
>                     | otherwise = []  

> dropWhile' :: (a -> Bool) -> [a] -> [a]
> dropWhile' _ [] = []
> dropWhile' p (x:xs) | p x = dropWhile' p xs
>                     | otherwise = x : xs

3. Redefine map f and filter p w/ foldr

> map' f = foldr (\n xs -> f n : xs) []

> filter' p = foldr (\n xs -> if p n then n : xs else xs) []

4. w/ foldl define dec2int :: [Int] -> Int

> dec2int :: [Int] -> Int
> dec2int ns = sum (foldl (\xs x -> x * 10 ^ (length ns - length (x:xs)) : xs) [] ns) 

5. Define function curry and uncurry

> curry :: ((a,b) -> b) -> a -> b -> b
> curry f n x = f (n,x)
> uncurry :: (a -> b -> b) -> (a,b) -> b
> uncurry f (x,y) = f x y

6. Redefine chop8, map f, iterate f w/ unfold

> unfold p h t x | p x = []
>                | otherwise = h x : unfold p h t (t x)

> chop8 = unfold null (take 8) (drop 8)
> map'' f = unfold null (\(x:_) -> f x) (drop 1)
> iterate' = unfold (\_ -> False) id

7. Modify binary transmitter so it detects errors w/ parity bits

> type Bit = Int

> bin2int :: [Bit] -> Int
> bin2int xs = sum [a*b |(a,b) <- zip weights bits]
>         where weights = iterate (*2) 1
>               bits = reverse xs

> int2bin :: Int -> [Bit]
> int2bin 0 = []
> int2bin n = int2bin q ++ [r]
>         where q = div n 2
>               r = mod n 2

> make8 :: [Bit] -> [Bit]
> make8 xs = reverse (take 8 (reverse xs ++ repeat 0))

> split :: Int -> [Bit] -> [[Bit]]
> split _ [] = []
> split n bs = take n bs : split n (drop n bs)

> amtOfOnes :: [Bit] -> Int
> amtOfOnes xs = length (filter (==1) xs)

> intToBool :: Int -> Bool
> intToBool = (==1)

> getParityBit :: [Bit] -> Bit
> getParityBit xs | odd (amtOfOnes xs) = 1
>                 | otherwise = 0

> addParityBit :: [Bit] -> [Bit]
> addParityBit xs = getParityBit xs : xs

> validate :: [Bit] -> [Bit]
> validate xs = concatMap (\ns -> if check ns then tail ns else error "Parity bit is either missing or wrong") bits
>       where bits = split 9 xs
>             check [] = False
>             check (n:ns) = length ns == 8 && getParityBit ns == n
>             removeParityBit = tail

> encode :: [Char] -> [Bit]
> encode [] = []
> encode (c:cs) = (addParityBit . make8 . int2bin . ord) c ++ encode cs

> decode :: [Bit] -> [Char]
> decode xs = map (chr . bin2int) (split 8 xs)

> transmit :: [Char] -> [Char]
> transmit = decode . validate . id . encode

8. Check 7 worked
        Yep

9. Define altmap :: (a -> b) -> (a -> b) -> [a] -> [b] which works like map, but alternatively applies function

> altmap :: (a -> b) -> (a -> b) -> [a] -> [b]
> altmap _ _ [] = []
> altmap f f' (x:xs) = f x : altmap f' f xs

10. With altmap define luhn algorithm (from chapter 4) again for card numbers of any length

> luhnDouble :: Int -> Int 
> luhnDouble n | n > 9 = n-9
>              | otherwise = n

> luhn :: [Int] -> Bool
> luhn xs = (\xs -> mod (sum xs) 10 == 0) (altmap luhnDouble (*1) xs)