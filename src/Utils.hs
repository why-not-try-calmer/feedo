module Utils where

import Data.List (foldl', sort)

partitionEither :: [Either a b] -> ([a], [b])
partitionEither = foldl' partition ([],[])
    where
        partition (ls, rs) val = case val of
            Left l -> (l:ls, rs)
            Right r -> (ls, r:rs)

maybeUserIdx :: [a] -> Int -> Maybe a
maybeUserIdx [] _ = Nothing
maybeUserIdx ls i
    |   i < 1 = Nothing
    |   i > length ls = Nothing
    |   i == 1 = Just $ head ls
    |   i == length ls = Just $ last ls
    |   otherwise = Just $ ls !! (i-1)

removeByIdx :: [a] -> [Int] -> Maybe [a]
removeByIdx [] _ = Nothing
removeByIdx _ [] = Nothing
removeByIdx ls is = 
    if any (\i -> i >= length ls || i < 1) is then Nothing
    else go [] 1 ls (sort is)
    where
        go acc _ _ [] = Just acc
        go acc _ [] _ = Just acc
        go !acc !n (l:ls') (i:is') = 
            if n == i then go acc (n+1) ls' is' 
            else go (acc++[l]) (n+1) ls' (i:is')