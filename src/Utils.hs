module Utils where

import Data.List (foldl')

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