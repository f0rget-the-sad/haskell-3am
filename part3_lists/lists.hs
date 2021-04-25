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