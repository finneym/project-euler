-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
--
-- What is the largest n-digit pandigital prime that exists?

module P041 where

import Data.Numbers.Primes
import Data.List
import Data.List.Unique

largestPandigital = 987654321

allPandigital = map (undigits) . permutations $ [1,2,3,4,5,6,7]



isPandigital :: Integer -> Bool
isPandigital n = (digits n) == (sortUniq.digits $ n) && (notElem 0 $ digits n)

solution = maximum $ filter (isPrime) allPandigital

digits :: Integer -> [Integer]
digits x = sort.reverse.digits' $ x
    where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x  `div` 10)

undigits :: [Integer] -> Integer
undigits [] = 0
undigits (x:xs) = x * (10 ^ length xs) + (undigits xs)