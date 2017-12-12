-- The first two consecutive numbers to have two distinct prime factors are:
--
-- 14 = 2 × 7
-- 15 = 3 × 5
--
-- The first three consecutive numbers to have three distinct prime factors are:
--
-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.
--
-- Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?

module P047 where

import Data.Numbers.Primes
import Data.List
import Data.List.Unique

firstN (xs) n
    |(correctNumberFactors) && (allDistinctFactors)   = take n (xs)
    | otherwise = firstN (drop 1 xs) n
    where
    correctNumberFactors = all (\x -> (length x) == n ) factorsList
    allDistinctFactors = null.uniqFactorList $ factorsList
    factorsList = map (sortUniq.primeFactors) $ take n (xs)

uniqFactorList xs = foldl1 (intersect) xs


