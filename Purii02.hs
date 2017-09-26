{-# OPTIONS_GHC -Wall #-}
module Purii02 where

-- Task 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl ix = foldl (+) 0 ix

-- Task 2 -----------------------------------------
productFr :: [Integer] -> Integer
productFr ix = foldr (*) 1 ix

-- Task 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr i1x i2x = foldr (:) i2x i1x


-- Task 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert ix v =
    if null ix
        then (v : [])
        else
            if (head ix) > v
                then (v : ix)
                else ((head ix) : insert (tail ix) v)

sortInsert :: [Int] -> [Int]
sortInsert lx = foldl insert [] lx

-- Task 5 -----------------------------------------
findIndices :: (Int -> Bool) -> [Int] -> [Int]
findIndices p lx = map fst $ filter (p.snd) indexed
    where
        indexed = zip [0..] lx
-- list former
-- findIndices p lx = [p1 | (p1,p2) <- zip [0..] lx, p p2]

-- Task 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse ls = reverse $ map reverse ls

-- Task 7  -----------------------------------------
noDigits :: String -> String
noDigits s = filter (`notElem` "0123456789") s

-- Task 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood px v =
    if null px
        then 0
        else
            if (head px) v
                then 1 + cntGood (tail px) v
                else cntGood (tail px) v
--cntGood px v = length(filter ($v) px)
--cntGood px v = length(filter (\p -> p v) px)

-- Task 9 ------------------------------------------
triangle :: [Integer]
triangle = scanl (+) 1 [2..]

-- Task 10 -----------------------------------------
piramid :: [Integer]
piramid  = scanl (\x y -> x + y*y) 1 [2..]

-- Task 11 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : 2 : zipWith (*) [3..] (tail factorialsM)
