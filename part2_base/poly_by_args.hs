import Data.Function

getSecondFrom :: p1 -> p2 -> p3 -> p2
getSecondFrom p1 p2 p3 = p2

-- f :: a -> a -> b -> a -> a
-- f a1 a2 a3 a4 = a1
-- f a1 a2 a3 a4 = a2
-- f a1 a2 a3 a4 = a4
-- so answer is 3

multSecond = g `on` h
g = (*)
h = snd

-- thinking 10 minutes where to use lambda here, but seems 
-- it's not intended to be lambda-task
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)