{-# OPTIONS_GHC -Wall #-}
module Purii06 where

newtype Poly a = P [a]

-- Задача 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Задача 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P l1) == (P l2) = (dropEndZeros l1) == (dropEndZeros l2)

dropEndZeros :: (Num a, Eq a) => [a] -> [a]
dropEndZeros ls
        | isEmpty ls = []
        | otherwise = (head ls) : dropEndZeros (tail ls)
        where
            isEmpty [] = True
            isEmpty (h:t) = h == 0 && isEmpty t

-- Задача 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P l)
        | null (dropEndZeros l) = "0"
        | otherwise = foldl (++) "" $ insBetween $ filter (not . null) showEvery
            where
                showEvery = map showOne (reverse $ index l)
                showOne (d, c)
                    | c == 0 = ""
                    | d == 0 = show c
                    | c == 1 && d == 1 = "x"
                    | c == -1 && d == 1 = "-x"
                    | c == 1 = "x^" ++ show d
                    | c == -1 = "-x^" ++ show d
                    | d == 1 = (show c) ++ "x"
                    | otherwise = (show c) ++ "x^" ++ (show d)
                insBetween [] = []
                insBetween (h:t)
                    | null t = [h]
                    | otherwise = (h : " + " : (insBetween t))

index :: Num a => [a] -> [(Integer, a)]
index [] = []
index l = zip [0 :: Integer ..] l

-- Задача 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P l1) (P l2) = P $ addLists l1 l2
    where
        addLists [] [] = []
        addLists [] s = s
        addLists f [] = f
        addLists (fh:ft) (sh:st) = (fh + sh) : (addLists ft st)

-- Задача 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P l1) (P l2) = foldl (+) (P [0]) (map (\y -> multByPoly y l2) (index l1))
        where
            riseDegree l d
                | d == 0 = l
                | otherwise = riseDegree (0:l) (d-1)
            multByPoly (d, c) l = P $ riseDegree (map (\y -> y*c) l) d

-- Задача 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P l) = P (map (\y -> 0-y) l)
    fromInteger i = P [(fromInteger i)]
    -- Розумних означень не існує
    abs    = undefined
    signum = undefined

-- Задача 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P l) x0 = foldl (+) 0 (map applyOne $ index l)
    where
        applyOne (d, c) = c*x0^d

-- Задача 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 p = p
    nderiv n p = deriv $ nderiv (n-1) p

-- Задача 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:[])) = P []
    deriv (P (_:t)) = x * deriv (P t) + (P t)
