-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

module P005 where

main :: Int -> Int -> Int
main min max = main' min max max

main' :: Int -> Int -> Int -> Int
main' min max cur
    | divides cur [min..max] = cur
    | otherwise = main' min max (cur + max)

divides :: Int -> [Int] -> Bool
divides n [] = True
divides n (x:xs)
    | n `mod` x == 0 = divides n xs
    | otherwise = False