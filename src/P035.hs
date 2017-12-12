-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?


module P035 where

import Data.List
import Data.Numbers.Primes


circularPrimes n = [x | x <- [2..n], isCircularPrime x]

isCircularPrime n = all (isPrime) $ rotations n

rotations n = take (length $ show n) $ iterate (rotate) n
rotate n = read ((last $ show n):(init $ show n))
