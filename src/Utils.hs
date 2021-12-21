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

removeByUserIdx :: [a] -> [Int] -> Maybe [a]
removeByUserIdx [] _ = Nothing
removeByUserIdx _ [] = Nothing
removeByUserIdx ls is =
    let adjusted = sort . map (\i -> i-1) $ is
    in  if any (\i -> i >= length ls || i < 0) adjusted then Nothing
        else go [] 0 ls adjusted
        where
            go acc _ rest [] = Just (acc++rest)
            go acc _ [] _ = Just acc
            go !acc !n (l:ls') (i:is') =
                if n == i then go acc (n+1) ls' is'
                else go (acc++[l]) (n+1) ls' (i:is')
