module Utils where

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
