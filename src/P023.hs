-- A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
--
-- A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.


module P023 where

import Data.List.Unique
import Data.Numbers.Primes
import Data.Array
import Data.List

upperBound = 28123


abundantsArray :: Array Int Bool
abundantsArray = listArray (1, upperBound) $ map isAbundant [1..upperBound]

abundants :: [Int]
abundants = filter (abundantsArray !) [1..upperBound]

remainders :: Int -> [Int]
remainders x = map (x-) $ takeWhile (<= x `quot` 2) abundants

sums :: [Int]
sums = filter (any (abundantsArray !) . remainders) [1..upperBound]

main :: IO ()
main = print $ sum [1..upperBound] - sum sums


-- main n =[x | x <- [1..n], elem x (getAllAbundantSums n)]

-- getAllAbundantSums n = getSums (getAllAbundant n) (getAllAbundant n)
--
-- getSums :: [Int] -> [Int] -> [Int]
-- getSums [] _ = []
-- getSums (x:xs) [] = getSums xs xs
-- getSums (x:xs) (y:ys) = (x + y) : getSums (x:xs) (ys)
--
-- getAllAbundant :: Int -> [Int]
-- getAllAbundant n = [x | x <- (possibleAbundants n), isAbundant x]
--
-- possibleAbundants :: Int -> [Int]
-- possibleAbundants n = [x | x <- [12..n], x `mod` 2 == 0 || x `mod` 5 == 0]


isAbundant :: Int -> Bool
isAbundant n = n < (sum $ getFactors n)


factorize :: Int -> [Int]
factorize n = primeFactors n

primePowers :: Int -> [(Int, Int)]
primePowers n = [(head x, length x) | x <- group $ factorize n]

getFactors :: Int -> [Int]
getFactors n = filter (<n) $ map product $ sequence
    [take (k+1) $ iterate (p*) 1 | (p, k) <- primePowers n]
