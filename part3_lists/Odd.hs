data Odd = Odd Integer deriving (Eq,Show)
instance Enum Odd where
    succ (Odd x) = Odd (x + 2)
    pred (Odd x) = Odd (x - 2)
    toEnum n = Odd (toInteger n)
    fromEnum (Odd n) = fromInteger n

    enumFrom (Odd x) = Odd x : enumFrom (Odd(x + 2))

    enumFromThen (Odd x) (Odd y) = Odd x : 
        enumFromThen (Odd (x + (y-x))) (Odd (y + (y -x)))

    enumFromTo (Odd x) (Odd lim) = if x > lim then [] 
        else Odd x : enumFromTo (Odd(x + 2)) (Odd lim)

    enumFromThenTo (Odd x) (Odd y) (Odd lim) = 
        if ((x < y) && (x > lim)) || ((x > y) && (x < lim)) then [] 
        else Odd x : enumFromThenTo (Odd(x + (y-x))) (Odd(y + (y-x))) (Odd lim)

-- Number greater then Int
baseVal = 9900000000000000000

-- Genrator for tasting
testVal n = Odd $ baseVal + n
testValsmall n = Odd $ 0 + n

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)
-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- enumFromTo
-- -- Up
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- Down
test4 = take 3 [testVal 7..testVal 1] == []
-- enumFromThen
-- -- Up
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- Down
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- enumFromThenTo
-- -- Up
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- Down
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test15 = toEnum (fromEnum (Odd 3)) == Odd 3

testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, 
            test11, test12, test13, test14, test15]
allTests = zip [0..] testList
badTests = map fst $ filter (not . snd) allTests