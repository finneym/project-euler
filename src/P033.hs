-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
--
-- If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

module P033 where

import Data.List
import Data.Function (on)
import Data.Ord (comparing)
import Data.Numbers.Primes
import Data.List.Unique


numerators, denominators :: [Int]
numerators = [10..99]
denominators = [10..99]

solution = nubBy (\ (_,_,x) (_,_,y) -> x == y) nonTrivials
    where
    nonTrivials = [(n,d,divide n d) | n <- numerators, d <- denominators, isNonTrivial n d]
    divide n d = ((fromIntegral n) / (fromIntegral d))


isNonTrivial :: Int -> Int -> Bool
isNonTrivial n d = (not $ null sharedNumbers) && (simpleDiv == advDiv) && lessOne && isTrivial
    where
        simpleDiv = (fromIntegral $ undigits $ (digits n) \\ (digits d)) / (fromIntegral $ undigits $ (digits d) \\ (digits n))
        advDiv = (fromIntegral n) / (fromIntegral d)
        lessOne = ((fromIntegral n) / (fromIntegral d)) < 1
        factors = intersect (getFactors n) (getFactors d)
        sharedNumbers =  map (upgradeZero) $ intersect (digits n) (digits d)
        isTrivial = null $ intersect (factors) (sharedNumbers)
        upgradeZero 0 = 10
        upgradeZero x = x


digits :: Int -> [Int]
digits x = reverse.digits' $ x
    where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x  `div` 10)

undigits :: [Int] -> Int
undigits [] = 0
undigits (x:xs) = x * (10 ^ length xs) + (undigits xs)

factorize :: Int -> [Int]
factorize n = primeFactors n

primePowers :: Int -> [(Int, Int)]
primePowers n = [(head x, length x) | x <- group $ factorize n]

getFactors :: Int -> [Int]
getFactors n = filter (<=n) $ map product $ sequence
    [take (k+1) $ iterate (p*) 1 | (p, k) <- primePowers n]