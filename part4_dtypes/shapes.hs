data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r)      = pi * (r)^0
area (Rectangle a b) = a * b

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False