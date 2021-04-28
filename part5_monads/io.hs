module Main where

main :: IO ()
main = do
    putStr "What is your name?\nName: "
    name <- getLine
    if length name == 0 then main else
        putStrLn $ "Hi, " ++ name ++ "!"
