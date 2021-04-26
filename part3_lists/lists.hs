import Data.Char
import GHC.Float

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a1 a2 lst = a1 : a2 : lst

-- nTimes = flip replicate
nTimes:: a -> Int -> [a]
nTimes a n = helper [] n where
    helper lst 0  = lst
    helper lst n = helper (a : lst) (n - 1) 

-- matching
-- sh ((_, x) : _) = x
-- sh ((:) ((,) _ x) y) = x
-- sh ((,) y x : z) =x


oddsOnly :: Integral a => [a] -> [a]
--oddsOnly = filter odd 
oddsOnly [] = []
oddsOnly (x:xs) = if odd x then x : oddsOnly xs else oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == reverse a

sum2 :: Num a => [a] -> [a] -> [a]
sum2 (a:as) (b:bs) = (a+b) : sum2 as bs
sum2 [] (b:bs) = b : bs
sum2 (a:as) [] = a : as
sum2 _ _ = []

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (a:as) (b:bs) (c:cs) = (a + b + c) : sum3 as bs cs
sum3 [] (b:bs) (c:cs) = (b + c) : sum2 bs cs
sum3 (a:as) [] (c:cs) = (a + c) : sum2 as cs
sum3 (a:as) (b:bs) [] = (a + b) : sum2 as bs
sum3 _ _ _ = []

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (h: hs) = helper [h] hs where
    helper h [] = [h]
    helper h (x:xs)
        | head h == x = helper (x : h) xs
        | otherwise = h : helper [x] xs

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f1 f2 = filter (\x -> f1 x || f2 x)

qsort :: Ord a => [a] -> [a]
qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)
qsort xs = xs

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

{-
My brain is not ready for this yet, taking mostly understandable version from:
http://stackoverflow.com/a/24564307/2289640
-}
perms :: [a] -> [[a]]
perms xxs     = xxs : perms' xxs
perms' []     = []
perms' (x:xs) = interleave' xs ++ concatMap interleave (perms' xs)
  where
   interleave yss = (x:yss) : interleave' yss
   interleave' [] = []
   interleave' (y:ys) = map (y:) (interleave ys)

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower ) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x1 x2 x3 -> max (max x1 x2) x3)

fibStream :: [Integer]
fibStream = fibStream' [0] [1] where
    fibStream' a1 a2 = head a1 : fibStream' (zipWith (+) a1 a2) a1

repeat = iterate repeatHelper
repeatHelper = id

coins = [2, 3, 7]
change n = change' [] [x:y | x <- coins, y <-[[]]] where
    change' res [] = res
    change' res act = change' (res ++ filter (\l -> sum l == n) act) (filter (\l -> sum l <= n) [x:y | x <- coins, y <-act])

concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\_ s -> s + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then s + x else s) 0

{-
foldr (-) x [2,1,5]
foldl (-) x [2,1,5]
x = ?
2 - (1 - (5 - x)) = x - 2 - 1 - 5
2 - 1  + 5 - x = x -8
6 -x = x -8
2x = 14
x = 7
-}

meanList :: [Double] -> Double
meanList p = (foldr (+) 0 p) / (int2Double (length p))

evenOnly :: [a] -> [a]
evenOnly p = foldr (\(idx, x) lst -> if even idx then x:lst else lst) [] (zip [1..] p)

lastElem :: [a] -> a
lastElem = foldl1 (\x y -> y)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f init = helper (f init) where
    helper (Just (x, ini')) = x : unfoldr f ini'
    helper Nothing          = []

revRange :: (Char,Char) -> [Char]
revRange (beginC, endC) = unfoldr g endC
  where g x = if x < beginC then Nothing else Just(x, (pred x))