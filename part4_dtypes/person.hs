import Data.List.Split
import Data.Char

data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving Show

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

updateLastName :: Person -> Person -> Person
updateLastName man mbman = mbman {lastName = lastName man}

abbrFirstName :: Person -> Person
abbrFirstName p@Person {firstName = name} = 
    let newName = if (length name) <= 2 then name else head name : "."
    in p {firstName = newName}

parseFirstName :: String -> Either Error String
parseFirstName str = parse (splitOn " = " str) where
    parse (x:xs) = case x of
        "firstName" -> Right (concat xs)
        _ -> Left ParsingError
    parse _ = Left ParsingError

parseLastName :: String -> Either Error String
parseLastName str = parse (splitOn " = " str) where
    parse (x:xs) = case x of
        "lastName" -> Right (concat xs)
        _ -> Left ParsingError
    parse [] = Left ParsingError

parseAge :: String -> Either Error Int
parseAge str = parse (splitOn " = " str) where
    parse (x:xs) = case x of
        "age" -> let sage = concat xs in
            if all isDigit sage then Right (read (sage)) else 
                Left (IncorrectDataError sage)
        _ -> Left ParsingError
    parse [] = Left ParsingError

-- @TODO: revrite this with chaining function together
parsePerson :: String -> Either Error Person
parsePerson str = parse (splitOn "\n" str) where
    parse lines = if (length lines) < 3 then Left IncompleteDataError
    else case parseFirstName (lines !! 0) of
        Right fname -> case parseLastName (lines !! 1) of
            Right lname -> case parseAge (lines !! 2) of 
                Right age -> Right Person{firstName = fname, lastName = lname, age = age}
                Left e -> Left e
            Left e -> Left e
        Left e -> Left e