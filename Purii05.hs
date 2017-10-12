{-# OPTIONS_GHC -Wall #-}
module Purii05 where

-- Задача 1 -----------------------------------------
getFirstN :: String -> String -> String
getFirstN file ns
    | null n
        || not (null (filter (not . (`elem` " ")) (snd $ head n))) = "error"
    | otherwise = unlines $ take (fst $ head n) (lines file)
    where n = reads ns::[(Int, String)]

firstN :: IO()
firstN = do
    putStr "File> "
    fileName <- getLine
    putStr "N> "
    ns <- getLine
    file <- readFile fileName
    putStrLn $ getFirstN file ns

-- Задача 2 -----------------------------------------
getTailN :: String -> String -> String
getTailN file ns
    | null n
        || not (null (filter (not . (`elem` " ")) (snd $ head n))) = "error"
    | otherwise = unlines $ lastN $ lines file
    where
        n = reads ns::[(Int, String)]
        lastN xs = drop (length xs - (fst $ head n)) xs

tailN :: IO()
tailN = do
    putStr "File> "
    fileName <- getLine
    putStr "N> "
    ns <- getLine
    file <- readFile fileName
    putStrLn $ getTailN file ns

-- Задача 3 -----------------------------------------

getSum2 :: String -> String -> String
getSum2 s1 s2
    | null arg1
        || null arg2
            || not (null (filter (not . (`elem` " ")) (snd $ head arg1)))
                || not (null (filter (not . (`elem` " ")) (snd $ head arg2))) = "error"
    | otherwise = show $ (fst $ head arg1) + (fst $ head arg2)
    where
        arg1 = reads s1::[(Int, String)]
        arg2 = reads s2::[(Int, String)]

sum2 :: IO()
sum2 = do
    putStr "> "
    s1 <- getLine
    putStr "> "
    s2 <- getLine
    putStrLn $ getSum2 s1 s2

-- Задача 4 -----------------------------------------
computeEquation :: Double -> Double -> Double -> String
computeEquation a b c
    | a == 0 && b == 0 && c == 0 = "many"
    | a == 0 && b == 0 = "error"
    | a == 0 = show (-c / b)
    | b == 0 && c == 0 = "x = 0"
    | c == 0 = "x1 = 0   x2 = " ++ show (-b / a)
    | b == 0 && (-c/a) > 0 = "x1 = " ++ (show $ sqrt (-c / a)) ++ "   x2 = " ++ (show $ -1 * sqrt (-c /a))
    | b == 0 = "no"
    | d >= 0 = "x1 = " ++ (show $ (-b + sqrt d) / 2 / a) ++ "   x2 = " ++ (show $ (-b - sqrt d) / 2 / a)
    | d < 0 = "no"
    | otherwise = "error"
    where d = b * b - 4 * a * c

getEquation :: String -> String
getEquation args
    | (length splitedArgs) /= 3 || null arg1 || null arg2 || null arg3 || not (null (snd $ head arg1)) || not (null (snd $ head arg2)) || not (null (snd $ head arg3)) = "error"
    | otherwise = computeEquation (fst $ head arg1) (fst $ head arg2) (fst $ head arg3)
    where
        splitedArgs = words args
        arg1 = reads (head splitedArgs)::[(Double, String)]
        arg2 = reads (head $ tail splitedArgs)::[(Double, String)]
        arg3 = reads (head $ tail $ tail splitedArgs)::[(Double, String)]

equation :: IO()
equation = do
    putStr "> "
    args <- getLine
    putStrLn $ getEquation args

-- Задача 5 -----------------------------------------
checkBalance :: String -> Int -> String
checkBalance file blnc
    | blnc < 0 = "Not balanced"
    | null file && blnc == 0 = "Is balanced"
    | null file = "Not balanced"
    | head file == '(' = checkBalance (tail file) (blnc + 1)
    | head file == ')' = checkBalance (tail file) (blnc - 1)
    | otherwise = checkBalance (tail file) blnc

balance :: IO()
balance = do
    putStr "File> "
    fileName <- getLine
    file <- readFile fileName
    putStrLn $ checkBalance file 0
