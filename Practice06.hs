{-# OPTIONS_GHC -Wall #-}
module Practice06 where

data Complex = 
    C Double Double
    deriving (Eq)

instance Num Complex where
    (C x1 y1) + (C x2 y2) = C (x1 + x2) (y1 + y2)
    (C x1 y1) - x2 y2) = C (x1 - x2) (y1 - y2)
    (*) = undefined
    fromInteger i = C (fromInteger i) 0
    abs (C x y) = C (sqrt $ x*x+y*y) 0
    signum c = undefined

instance Show Complex where
    show (C x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Ord Complex where
    (C x1 y1) <= (C x2 y2) = undefined
