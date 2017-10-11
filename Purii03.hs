{-# OPTIONS_GHC -Wall #-}
module Purii03 where

data OrdTree a = OrdTree a [OrdTree a]
               deriving (Show)

data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving (Show, Eq)

-- Задача 1 -----------------------------------------
dfsTree ::  [OrdTree a] -> [a]
dfsTree [] = []
dfsTree (OrdTree value children : siblings) = (value : ((dfsTree children) ++ (dfsTree siblings)))

-- Задача 2 -----------------------------------------
getValueOT :: OrdTree a -> a
getValueOT (OrdTree value _) = value

getChildrenOT :: OrdTree a -> [OrdTree a]
getChildrenOT (OrdTree _ []) = []
getChildrenOT (OrdTree _ children) = children

bfsTree ::  [OrdTree a] -> [a]
bfsTree [] = []
bfsTree list = (map getValueOT list) ++ (bfsTree (concat (map getChildrenOT list)))

-- Задача 3 -----------------------------------------
equalTreeList :: (Eq a) => [OrdTree a] -> [OrdTree a] -> Bool
equalTreeList [] [] = True
equalTreeList [] (_:_) = False
equalTreeList (_:_) [] = False
equalTreeList (headTree1 : siblings1) (headTree2 : siblings2) =
    if equalTree headTree1 headTree2
        then equalTreeList siblings1 siblings2
        else False

equalTree :: (Eq a)=> OrdTree a -> OrdTree a -> Bool
equalTree (OrdTree value1 children1) (OrdTree value2 children2) =
    if value1 == value2
        then equalTreeList children1 children2
        else False

-- Задача 4 -----------------------------------------
toBinTree :: [OrdTree a] -> BinTree a
toBinTree [] = Empty
toBinTree (OrdTree value children : siblings) = Node value (toBinTree children) (toBinTree siblings)

-- Задача 5 -----------------------------------------
toOrdTree :: BinTree a -> [OrdTree a]
toOrdTree Empty = []
toOrdTree (Node value left right) = (OrdTree value (toOrdTree left) : (toOrdTree right))

-- Задача 6 -----------------------------------------
getValueBT :: BinTree a -> a
getValueBT (Node value _ _) = value

isSearch :: (Ord a) => BinTree a -> Bool
isSearch Empty = True
isSearch (Node _ Empty Empty) = True
isSearch (Node value left Empty) = value > (getValueBT left) && isSearch left
isSearch (Node value Empty right) = value < (getValueBT right) && isSearch right
isSearch (Node value left right) = value > (getValueBT left)
                                    && value < (getValueBT right)
                                    && isSearch left
                                    && isSearch right

-- Задача 7  -----------------------------------------
elemSearch ::(Ord a) => BinTree a -> a -> Bool
elemSearch Empty _ = False
elemSearch (Node value left right) query =
    if query == value
        then True
        else if query < value
            then elemSearch left query
            else elemSearch right query

-- Задача 8 ------------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a
insSearch Empty x = Node x Empty Empty
insSearch (Node value left right) x =
    if x < value
        then Node value (insSearch left x) right
        else if x > value
            then Node value left (insSearch right x)
            else Node value left right

-- Задача 9 ------------------------------------------
isEmptyBT :: BinTree a -> Bool
isEmptyBT Empty = True
isEmptyBT (Node _ _ _) = False

getLeftBT :: BinTree a -> BinTree a
getLeftBT Empty = Empty
getLeftBT (Node _ Empty _) = Empty
getLeftBT (Node _ left _) = left

getRightBT :: BinTree a -> BinTree a
getRightBT Empty = Empty
getRightBT (Node _ _ Empty) = Empty
getRightBT (Node _ _ right) = right

getDeepLeftBT :: BinTree a -> BinTree a
getDeepLeftBT Empty = Empty
getDeepLeftBT (Node value left right) =
    if not (isEmptyBT left) then getDeepLeftBT left
    else Node value left right

delSearch :: (Ord a) => BinTree a -> a -> BinTree a
delSearch Empty _ = Empty
delSearch (Node value left right) x =
    if x < value then Node value (delSearch left x) right
    else if x > value then Node value left (delSearch right x)
    else
        if isEmptyBT left && isEmptyBT right then Empty
        else if isEmptyBT right then left
        else if isEmptyBT left then right
        else
            if isEmptyBT (getLeftBT right) then Node (getValueBT right) left (getRightBT right)
            else Node deepLeftValue left (delSearch right deepLeftValue)
                where deepLeftValue = getValueBT (getDeepLeftBT right)


-- Задача 10 -----------------------------------------
infixTraverse :: (Ord a) => BinTree a -> [a]
infixTraverse Empty = []
infixTraverse (Node value left right) = (infixTraverse left) ++ [value] ++ (infixTraverse right)

sortList :: (Ord a) => [a] -> [a]
sortList list = infixTraverse (foldl insSearch Empty list)

---------------------------------
otx :: [OrdTree Int]
otx =  [ OrdTree 1 [OrdTree 2 [],
                    OrdTree 3 [OrdTree 10 []] ] ,
         OrdTree 4 [OrdTree 5 [OrdTree 8 []],
                    OrdTree 6 [OrdTree 9 []],
                    OrdTree 7 []]
       ]

-- ord trees for equality test
ot1 :: OrdTree Int
ot1 = OrdTree 1 [OrdTree 2 [], OrdTree 3 [OrdTree 4 []]]
ot2 :: OrdTree Int
ot2 = OrdTree 1 [OrdTree 2 [], OrdTree 3 [OrdTree 4 []]]
ot3 :: OrdTree Int
ot3 = OrdTree 1 [OrdTree 2 [], OrdTree 4 [OrdTree 5 []]]

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

-- search binary tree
bts :: BinTree Int
bts = Node 10 (Node 5 (Node 3 Empty (Node 4 Empty Empty)) (Node 8 Empty (Node 9 Empty Empty))) (Node 15 (Node 12 (Node 11 Empty Empty) Empty) Empty)

-- search binary tree
bts1 :: BinTree Int
bts1 = Node 10 (Node 5 (Node 1 Empty Empty) (Node 7 Empty (Node 8 Empty Empty))) (Node 15 (Node 11 Empty (Node 13 (Node 12 Empty Empty) Empty)) (Node 20 Empty Empty))
