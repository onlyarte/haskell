{-# OPTIONS_GHC -Wall #-}
module Practice02 where

delA :: String -> String
delA xs = filter (not . (`elem` "aA")) xs
-- delA xs = filter (`notElem` "aA") xs

fstL :: [String] -> String
fstL xs = map head (filter (not . null) xs)
-- fstL xs = map head (filter (/="") xs)

-- take filter and apply to xs, take map and apply to prev result
-- fstL xs = ((map head).(filter (/=""))) xs

-- fstL = ((map head).(filter (/="")))
