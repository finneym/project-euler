-- It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
--
-- 9 = 7 + 2×1^2
-- 15 = 7 + 2×2^2
-- 21 = 3 + 2×3^2
-- 25 = 7 + 2×3^2
-- 27 = 19 + 2×2^2
-- 33 = 31 + 2×1^2
--
-- It turns out that the conjecture was false.
--
-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?


module P046 where
import Data.Numbers.Primes

isntConjecture = [x | x <- [1,3..], not.isPrime $ x, not.isConjecture $ x]
isConjecture n = not.null $ filter (isSquare) $ map (\x -> (fromIntegral (n - x)) / (fromIntegral 2) ) $ takeWhile (< n) primes
isSquare n = (ceiling.sqrt $ n) == (floor.sqrt $ n)