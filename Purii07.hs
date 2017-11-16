{-# OPTIONS_GHC -Wall #-}
module Purii07 where

data Stream a = Cons a (Stream a)

-- Екземпляр Show виводить перші 20 елементів, за якими розташовані крапки продовження
instance Show a => Show (Stream a) where
    show xs =  (foldl (++) "[" 
                  $ map (\x -> (show x) ++ ", ") $ take 20 $ streamToList xs
                ) ++ "..."

--Задача 1 -----------------------------------------

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

-- Задача 2 -----------------------------------------

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

-- Задача 3 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat v = Cons v (sRepeat v)

sIterate :: (a -> a) -> a -> Stream a
sIterate f v = Cons v $ sIterate f $ f v

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons h1 t1) (Cons h2 t2) = Cons h1 $ Cons h2 $ sInterleave t1 t2

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons h t) = h : sTake (n - 1) t

-- Задача 4 -----------------------------------------

nats :: Stream Integer
nats = Cons 0 $ next 0
    where
        next n = Cons (n + 1) $ next $ n + 1

-- Задача 5 -----------------------------------------

ruler :: Stream Integer
ruler = Cons (maxExp 1 0 0) $ next 1
    where
        maxExp :: Integer -> Integer -> Integer -> Integer
        maxExp n curr lmax
            | 2^curr > n = lmax
            | mod n (2^curr) == 0 = maxExp n (curr + 1) curr
            | otherwise = maxExp n (curr + 1) lmax
        next :: Integer -> Stream Integer
        next n = Cons (maxExp (n + 1) 0 0) $ next $ n + 1

-- Задача 6 -----------------------------------------

rand :: Integer -> Stream Integer
rand r0 = Cons nextRand $ rand nextRand
    where
        nextRand = mod ((1103515245 * r0) + 12345) 2147483648

-- Задача 7 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib $ n-1) + (fib $ n-2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-- Задача 8 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) fibs2 $ tail fibs2)

-- Задача 9 -----------------------------------------

data  Matrix a = M(a,a)(a,a)
         deriving (Show, Eq, Ord)
         
instance Num a => Num (Matrix a) where
    (M (x1,x2) (x3,x4)) + (M (y1,y2) (y3, y4)) = 
        M (x1+y1, x2+y2) (x3+y3, x4+y4)
    (M (x1,x2) (x3,x4)) * (M (y1,y2) (y3, y4)) = 
        M (x1*y1 + x2*y3, x1*y2 + x2*y4) (x3*y1 + x4*y3, x3*y2 + x4*y4)
    negate (M (x1,x2) (x3,x4)) = 
        M (negate x1, negate x2) (negate x3, negate x4) 
    fromInteger x = 
        M (fromInteger x, fromInteger x) (fromInteger x, fromInteger x)
    -- Реалізовувати не потрібно
    abs    = undefined
    signum = undefined

-- Задача 10 ----------------------------------------

fastFib :: Integer -> Integer
fastFib n = fs $ (M (1,1) (1,0))^n
        where
            fs (M _ x) = fst x
