{-# OPTIONS_GHC -Wall #-}
module Practice05 where

countWords :: String -> String
countWords str = show (length (words str))

countChars :: String -> String
countChars str = show (length str)

countIn :: IO()
countIn = do
    putStr ">>..."
    str <- getLine
    putStrLn ((countWords str) ++ " " ++ (countChars str))

countLines :: String -> String
countLines str = show (length (lines str))

countAllIn :: IO()
countAllIn = do
    str <- readFile "Practice05.hs"
    putStrLn ((countLines str) ++ " " ++ (countWords str) ++ " " ++ (countChars str))
