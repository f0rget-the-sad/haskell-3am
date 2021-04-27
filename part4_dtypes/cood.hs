data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = 
    sqrt ((x1 - x2)^2 + (y1 - y2)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) =
    (abs (x1 - x2)) + (abs (y1 - y2))

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = 
    (Coord (calc (fromIntegral x)) (calc (fromIntegral y))) where
    half = size / 2.0
    calc a = size * a + half

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (calc x) (calc y) where
    calc a = floor (a / size)