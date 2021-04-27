import Data.Char

data Color = Red | Green | Blue

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

stringToColor :: String -> Color
stringToColor "Red"   = Red
stringToColor "Green" = Green
stringToColor "Blue"  = Blue

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp x y = compare (ord x) (ord y) where
   ord Error = 3
   ord Warning = 2
   ord Info = 1

{-
processData :: SomeData -> String
processData p = 
    case doSomeWork p of
        (Success, _) -> "Success"
        (Fail, n)    -> "Fail: " ++ show n
-}

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

data Result = Fail | Success

data Result' = FailR Int | SuccessR

instance Show Result' where
    show (FailR n) = "Fail: " ++ show n
    show SuccessR = "Success"

{-
doSomeWork :: SomeData -> (Result,Int)
doSomeWork' :: SomeData -> Result'
doSomeWork' p =
    case doSomeWork p of
        (Success, _) -> SuccessR
        (Fail, n)    -> FailR n
-}

findDigit :: [Char] -> Maybe Char
findDigit a = findDigit' flt where
    flt = filter isDigit a
    findDigit' []    = Nothing
    findDigit' (x:_) = Just x

findDigitOrX :: [Char] -> Char
findDigitOrX str = case findDigit str of
    Nothing -> 'X'
    Just d -> d

maybeToList :: Maybe a -> [a]
maybeToList a = case a of
    Nothing -> []
    Just a ->[a]

listToMaybe :: [a] -> Maybe a
listToMaybe a = case a of
    [] -> Nothing
    (x:_) -> Just x

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing
