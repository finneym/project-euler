-- The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.
--
-- Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

module P037 where

import Data.Numbers.Primes

import Debug.Trace


brute = sum [x | x <- [10..1000000], isTruncatablePrime x]

solution = sum $ solution' 10 []
solution' n acc
    | length acc >= 11 = acc
    | otherwise = solution' (n * 10) (acc ++ nextLayer n)

nextLayer n = [n * y + x | x <- xs, y <- [1..9], isTruncatablePrime (n * y + x)]
    where
        xs = filter (\p -> p >= (n `div` 10)) $ takeWhile (\ p -> p < n) primes

isTruncatablePrime 0 = True
isTruncatablePrime n = (isTruncatablePrime' n moveRight) && isTruncatablePrime' n moveLeft
    where
    isTruncatablePrime' 0 _ = True
    isTruncatablePrime' n move = (isPrime n) && (isTruncatablePrime' (move n) move)

moveRight, moveLeft :: Integer -> Integer
moveRight n
    | n < 10 = 0
    | otherwise = read.tail.show $ n
moveLeft n
    | n < 10 = 0
    | otherwise = read.init.show $ n

