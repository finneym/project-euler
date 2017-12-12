-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

module P003 where

import Data.Numbers.Primes

main :: Int -> Int
main n = maximum $ primeFactors n