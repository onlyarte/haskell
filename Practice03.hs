{-# OPTIONS_GHC -Wall #-}
module Practice03 where

-- а - тип значення вузла
data OrdTree a = OrdTree a [OrdTree a]
               deriving (Show)

data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving (Show, Eq)

-- Приклад 1: порахувати кількість елементів у дереві
tr :: OrdTree Integer
tr = OrdTree 1 [OrdTree 2 [OrdTree 3 []], OrdTree 5 [OrdTree 10 []]];

cntO :: OrdTree a -> Int
cntO (OrdTree _ otl) = 1 + sum (map cntO otl)

-- Приклад 2: порахувати кількість елементів у бінарному дереві
bt1 :: BinTree Integer
bt1 = Node 1 Empty (Node 2 Empty Empty)

cntB :: BinTree a -> Int
cntB (Empty) = 0
cntB (Node _ btl btr) = 1 + cntB btl + cntB btr

-- Приклад 3: знайти найменшого сина в бінарному дереві пошуку
bt2 :: BinTree Integer
bt2 = Node 10 (Node 5 Empty (Node 7 Empty Empty)) (Node 14 Empty Empty)

minB :: Ord a => BinTree a -> a
minB (Empty) = error "Empty tree. Nothing to find"
minB (Node v Empty _) = v
minB (Node _ btl _) = minB btl

-- Задача 1 -----------------------------------------
dfsTree ::  [OrdTree a] -> [a]
dfsTree = undefined

-- Задача 2 -----------------------------------------
bfsTree ::  [OrdTree a] -> [a]
bfsTree = undefined

-- Задача 3 -----------------------------------------
equalTree ::(Eq a)=> OrdTree a -> OrdTree a -> Bool
equalTree = undefined

-- Задача 4 -----------------------------------------
toBinTree :: [OrdTree a] -> BinTree a
toBinTree = undefined

-- Задача 5 -----------------------------------------
toOrdTree :: BinTree a -> [OrdTree a]
toOrdTree = undefined

-- Задача 6 -----------------------------------------
isSearch :: (Ord a) => BinTree a -> Bool
isSearch = undefined

-- Задача 7  -----------------------------------------
elemSearch ::(Ord a) => BinTree a -> a -> Bool
elemSearch = undefined

-- Задача 8 ------------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a
insSearch = undefined

-- Задача 9 ------------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a
delSearch  = undefined

-- Задача 10 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList = undefined

---------------------------------
otx :: [OrdTree Int]
otx =  [ OrdTree 1 [OrdTree 2 [],
                    OrdTree 3 [OrdTree 10 []] ] ,
         OrdTree 4 [OrdTree 5 [OrdTree 8 []],
                               OrdTree 6 [OrdTree 9 []],
                               OrdTree 7 []]
       ]

bt :: BinTree Int
bt = Node 1(Node 2 Empty
                    (Node 3 (Node 10 Empty Empty)
                            Empty)
            )
            (Node 4  (Node 5 (Node 8  Empty Empty)
                             (Node 6  (Node 9 Empty Empty)
                                      (Node 7 Empty Empty)
                             )
                      )
             Empty
            )
