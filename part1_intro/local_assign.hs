seqA :: Integer -> Integer
seqA k
    | k >= 0 = let
        helper a1 a2 a3 0 = a1
        helper a1 a2 a3 k = helper a2 a3 ((a3 + a2) - 2 * a1) (k - 1)
    in helper 1 2 3 k
    | otherwise = error "k must be >= 0"


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
    | x == 0 = (0, 1)
    | x < 0 = helper 0 0 (-x)
    | otherwise = helper 0 0 x
    where
        helper dsum dcount 0 = (dsum, dcount)
        helper dsum dcount x = helper (dsum + (mod x 10)) (dcount + 1) (div x 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + (fsum 0 n))
    where
        n = 1000000
        h = (b - a) / n
        fsum s 0 = s
        fsum s n = fsum (s + f (n * h + a)) (n - 1)
