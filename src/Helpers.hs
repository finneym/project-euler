module Helpers where

import Data.List

digits :: Integer -> [Integer]
digits x = reverse.digits' $ x
    where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x  `div` 10)

undigits :: [Integer] -> Integer
undigits [] = 0
undigits (x:xs) = x * (10 ^ length xs) + (undigits xs)

factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

iterateWhile :: ((a, a, a) -> (a, a, a)) -> (a, a, a) -> ((a, a, a) -> Bool) -> [(a, a, a)]
iterateWhile f x g = takeWhile g $ iterate f x

binarySearch :: (Integer -> Integer) -> Integer -> Integer -> Integer -> Maybe Integer
binarySearch f l r t
    | l > r = Nothing
    | (f m) == t = Just m
    | f m < t = binarySearch (f) (m + 1) r t
    | otherwise = binarySearch (f) l (m - 1) t
    where
    m = (l + r) `div` 2