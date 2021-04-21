doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = n * doubleFact(n - 2)

{- fibonacci non optimized
 - :set +s
 - *Main> fibonacci (30)
 - (2.19 secs, 781,576,560 bytes)
 - 832040
-}
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 0  = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0  = fibonacci (n + 2) - fibonacci (n + 1)

{- fibonacci optimized
 - fibonacci 30
 - (0.00 secs, 78,024 bytes)
 - main idea is to accumulate two values
 - a b -> c
 - b c -> d
 - 1 0 -1 -2
 - 1 0  1 -1
 - -}

fibonacci_op :: Integer -> Integer
fibonacci_op n | n >= 0    = helper_pos 0 1 n
               | otherwise = helper_neg 1 0 (-n)

helper_pos f1 f2 n | n == 0 = f1
                   | otherwise = helper_pos f2 (f1 + f2) (n - 1)

helper_neg f1 f2 n | n == 0 = f2
                   | otherwise = helper_neg f2 (f1 - f2) (n - 1)
