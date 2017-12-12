-- The prime 41, can be written as the sum of six consecutive primes:
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
--
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.
--
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
--
-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

-- [2,3,5 ,7 ,11,13,17,19,23 ,29 ,31 ,37 ,41 ,43 ,47 ,53 ,59 ,61 ,67 ,71 ,73 ,79 ,83 ,89 ,97]
-- [2,5,10,17,28,41,58,77,100,129,160,197,238,281,328,381,440,501,568,639,712,791,874,963,1060]


module P050 where

import Data.List
import Data.Numbers.Primes
import Helpers
import Data.Function (on)

maxRange = 1000000

consecutive :: Int -> [Int]
consecutive p = dropWhile (not . isPrime) $ reverse sums where
    sums = takeWhile (< 1000000) $ scanl1 (+) $ dropWhile (< p) primes

solution = head $ maximumBy (compare `on` length) $ map (consecutive) $ take 10 primes