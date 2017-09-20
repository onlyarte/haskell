{-# OPTIONS_GHC -Wall #-}
module Purii01 where

-- Task 1 -----------------------------------------
factorial :: Integer -> Integer
factorial n =
    if n < 0
        then error "n should be greater or equal than 0"
        else
            if n == 0
                then 1
                else n * factorial (n-1)

-- Task 2 -----------------------------------------
listSum :: [Int]-> [Int] -> [Int]
listSum lx1 lx2 =
    if null lx1
        then lx2
        else
            if null lx2
                then lx1
                else ((head lx1) + (head lx2) : listSum (tail lx1) (tail lx2))

-- Task 3 -----------------------------------------
oddEven :: [Int] -> [Int]
oddEven lx =
    if null lx
        then []
        else
            if null (tail lx)
                then lx
                else (head (tail lx) : (head lx : oddEven (tail (tail lx))))

-- Task 4 -----------------------------------------
power3 :: [Integer]
power3 = [x ^ (3 :: Integer) | x <- [1..]]

-- Task 5 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [(3 :: Integer) ^ (n :: Integer) | n <- [1..]]

-- Task 6 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n =
    if n <= 0
        then error "n should be greater than 0"
        else sum ([3 ^ i | i <- [1..n]])

-- Task 7 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n =
    if n <= 0
        then error "n should be greater than 0"
        else
            if m < 0
                then error "m should be greater than or equal to 0"
                else sum ([m ^ i | i <- [1..n]])

-- Task 8 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n =
    if n <= 0
        then error "n should be greater than 0"
        else
            if m < 0
                then error "m should be greater than or equal to 0"
                else sum ([fromIntegral (m ^ i) / fromIntegral (factorial i) | i <- [1..n]])

-- Task 9 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1..]]

-- Task 10 -----------------------------------------
count :: [Int] -> Int -> Int
count [] _ = 0
count xl x =
    if (head xl) == x
        then 1 + count (tail xl) x
        else count (tail xl) x

set :: [Int] -> [Int]
set [] = []
set xl =
    if (count (tail xl) (head xl)) == 0
        then (head xl : set (tail xl))
        else set (tail xl)

frequency :: [Int] -> [(Int,Int)]
frequency xl = [(x, count xl x) | x <- (set xl)]
