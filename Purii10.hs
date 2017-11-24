--{-# OPTIONS_GHC -Wall #-}
module HW10 where
    
import Data.List

-- Task 1 ------------------------------------------
lucky ::   Int ->  [String]
lucky n = filter (\x -> (summ (firstN x)) == (summ (lastN x))) all
    where
        all :: [String]
        all = map show [(10 ^ (n*2 - 1))..(10 ^ (n*2) - 1)]
        firstN :: String -> String
        firstN n0 = take n n0
        lastN :: String -> String
        lastN n0 = drop n n0
        summ n1 = sum $ map (\x -> read [x] :: Int) n1


-- Task 2 -----------------------------------------  
queens :: Int -> [[Int]]
queens n = filter (\x -> foldl (&&) True $ map (\(v,i) -> (chck x (i-1) v [(v-1),(v-2)..] [(v+1),(v+2)..] (-1)) && (chck x (i+1) v [(v-1),(v-2)..] [(v+1),(v+2)..] 1)) (zip x [0..])) $ permutations [1..n]

chck :: [Int] -> Int -> Int -> [Int] -> [Int] -> Int -> Bool
chck brd index home blck1 blck2 stp
    | index < 0 = True
    | index > (length brd - 1) = True
    | otherwise =
        foldl (&&) True ((brd!!index /= home) : (brd!!index /= (head blck1)) : (brd!!index /= (head blck2)) : [(chck brd (index + stp) home (tail blck1) (tail blck2) stp)])
    
-- Task 3 -----------------------------------------
maxLen ::  [Int] -> Int
maxLen seq = length $ snd $ maximum $ [(length seq0, seq0) | seq0 <- (allSeq seq)]

allSeq :: [Int] -> [[Int]]
allSeq l = filter chck (subsequences l)
        where
            chck seq0 = (sort seq0) == seq0 && isUnique seq0
            isUnique [] = True
            isUnique (h:[]) = True
            isUnique seq0 = (head seq0) /= (head $ tail seq0) && (isUnique $ tail seq0)
    
-- Task 4 -----------------------------------------
maxSeq ::  [Int] ->  [Int]
maxSeq seq = snd $ maximum $ [(length seq0, seq0) | seq0 <- (allSeq seq)]

-- Task 5 -----------------------------------------
allMaxSeq ::  [Int] -> [[Int]]
allMaxSeq seq = filter (\seq0 -> (length seq0) == (maxLen seq)) (allSeq seq)

-- Task 6 -----------------------------------------
genExpr ::  Int -> Int -> [String]
genExpr a b = map (\z -> toInfix (filter (/=' ') z) (nOpers + nOpers + 1)) $ filter (\y -> poliz y == b) $ map (\x -> getPoliz (show a) x) (combinations nOpers "+-*")
        where
            combinations n _ | n <= 0 = [[]]
            combinations 1 xs = map (:[]) xs
            combinations n xs = (:) <$> xs <*> combinations (n-1) xs
            nOpers = (length $ show a) - 1
            getPoliz [] opers = opers
            getPoliz nums opers = 
                if length nums == (nOpers + 1)
                    then (head nums) : ' ' : getPoliz (tail nums) opers
                    else (head nums) : ' ' : (head opers) : ' ' : getPoliz (tail nums) (tail opers)

-- 54+3-2* -> 5+4-3*2
toInfix :: String -> Int -> String
toInfix [] _ = []
toInfix plz l = 
    if length plz == l
        then (head plz) : toInfix (tail plz) l
        else (head $ tail plz) : (head plz) : toInfix (tail $ tail plz) l

-- 5 4 + 3 - 2 * -> 12
poliz :: String -> Int
poliz input = head $ foldl foldElems [] (words input)
        where
            foldElems (x:y:ys) "*" = (x * y):ys
            foldElems (x:y:ys) "+" = (x + y):ys
            foldElems (x:y:ys) "-" = (y - x):ys
            foldElems xs nmbr = read nmbr:xs

-- Task 7 -----------------------------------------
genExprBracket ::  Int -> Int -> [String]
genExprBracket = undefined