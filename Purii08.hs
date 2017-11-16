{-# OPTIONS_GHC -Wall #-}
module Purii08 where

type Graph = [[Int]]

-- Задача 1 ------------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay [] _ _ = Nothing
longWay graph start finish
    | isAnyGraph graph == False = Nothing
    | start < 1 = Nothing
    | start > (length graph) = Nothing
    | finish < 1 = Nothing
    | finish > (length graph) = Nothing
    | null getAllWays = Nothing
    | otherwise = Just $ getLongest getAllWays
    where
        getAllWays = allWays graph start start finish []
        getLongest ways = snd $ maximum $ [(length way, way) | way <- ways]

allWays :: Graph -> Int -> Int -> Int -> [Int] -> [[Int]]
allWays [] _ _ _ _ = []
allWays graph start curr finish path
    | start == finish = allCycles graph start []
    | curr == finish = [(path ++ [finish])]
    | otherwise = foldl (++) [] $ map (\neighbour -> allWays (cutGr graph curr) start neighbour finish (path ++ [curr])) (graph!!(curr-1))

allCycles :: Graph -> Int -> [Int] -> [[Int]]
allCycles graph start path = foldl (++) [] $ map (\neighbour -> allWays graph neighbour neighbour start (path ++ [start])) (graph!!(start-1))

cutGr :: Graph -> Int -> Graph
cutGr [] _ = []
cutGr graph target = map (filter (/=target)) graph

-- Задача 2 -----------------------------------------  
isNoCycle :: Graph -> Bool
isNoCycle graph = isAnyGraph graph && (foldl (&&) True (map (\vert -> (allCycles graph vert []) == []) [1..(length graph)]))
   
-- Задача 3 -----------------------------------------
isTransitive :: Graph -> Bool
isTransitive graph = isAnyGraph graph && (foldl (&&) True (map (\vert1 -> foldl (&&) True (map (\vert2 -> isEdge graph vert1 vert2) $ allConnected graph vert1)) [1..(length graph)]))

allConnected :: Graph -> Int -> [Int]
allConnected graph start = uniq (filter (/=start) $ foldl (++) [] $ foldl (++) [] (map (\vert -> allWays graph start start vert []) [1..(length graph)])) []

isEdge :: Graph -> Int -> Int -> Bool
isEdge graph vert1 vert2 = vert2 `elem` (graph!!(vert1-1))

uniq :: [Int] -> [Int] -> [Int]
uniq [] l2 = l2
uniq (h1:t1) l2
    | h1 `elem` l2 = uniq t1 l2
    | otherwise = uniq t1 (h1:l2)

-- Задача 4 -----------------------------------------
isGraph :: Graph -> Bool
isGraph graph = isAnyGraph graph && (foldl (&&) True $ map (\vert -> foldl (&&) True (map (\neighbour -> isEdge graph neighbour vert) (graph!!(vert-1)))) [1..(length graph)])

isAnyGraph :: Graph -> Bool
isAnyGraph graph = foldl (&&) True $ map (\vert1 -> foldl (&&) True (map (\vert2 -> vert2 /= vert1 && vert2 > 0 && vert2 <= (length graph)) (graph!!(vert1-1)))) [1..(length graph)]

-- Задача 5 -----------------------------------------
shortWay :: Graph -> Int -> Int -> Maybe [Int]
shortWay [] _ _ = Nothing
shortWay graph start finish
    | isGraph graph == False = Nothing
    | start < 1 = Nothing
    | start > (length graph) = Nothing
    | finish < 1 = Nothing
    | finish > (length graph) = Nothing
    | null getAllWays = Nothing
    | otherwise = Just $ getShortest getAllWays
    where
        getAllWays = allWays graph start start finish []
        getShortest ways = snd $ minimum $ [(length way, way) | way <- ways]

-- Задача 6 -----------------------------------------
isConnecting :: Graph -> Bool
isConnecting graph = (isGraph graph) && ((length graph - 1) == (length $ allConnected graph 1))

-- Задача 7 -----------------------------------------
components :: Graph -> [[Int]]
components graph = getComps graph [1..(length graph)] []

getComps :: Graph -> [Int] -> [[Int]] -> [[Int]]
getComps _ [] comps = comps
getComps graph (h:t) comps = getComps graph (filter (\neighbour -> neighbour `notElem` connections) t) ((h:connections):comps)
    where
        connections = allConnected graph h

-- Задача 8 -----------------------------------------
topolSorting :: Graph -> Maybe[Int]
topolSorting [] = Nothing
topolSorting graph
    | isAnyGraph graph == False = Nothing
    | otherwise = getTopol graph []

getTopol :: Graph -> [Int] -> Maybe [Int]
getTopol graph topol
    | (length topol) == (length graph) = Just $ reverse topol
    | otherwise = 
        if hasNext
            then getTopol dropThis (fNext : topol)
            else Nothing
     where
        hasNext = (length next) > 0
        fNext = head next
        next = filter (\vert -> vert `notElem` topol && null (filter (vert `elem`) graph)) [1..(length graph)]
        
        dropThis = fst splited ++ ([] : (tail $ snd splited))
        splited = splitAt (fNext-1) graph
