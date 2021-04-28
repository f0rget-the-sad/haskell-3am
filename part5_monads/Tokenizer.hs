import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

symbols = [("+", Plus), ("-", Minus), ("(", LeftBrace), (")", RightBrace)]
asToken :: String -> Maybe Token
asToken str = if all isDigit str then Just $ Number (read str) else
    lookup str symbols

tokenize :: String -> Maybe [Token]
tokenize input = mapM asToken $ words input
