-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this sequence?

module P049 where

import Data.List
import Data.Numbers.Primes
import Helpers
import Data.List.Unique


primePermutations n = sortUniq . filter (isPrime) . map (undigits) . permutations . digits $ n

findAllPrimePermutations [] acc = acc
findAllPrimePermutations (x:xs) acc
    | ((length.primePermutations $ x) >= 3) && (null $ intersect (primePermutations x) acc) = findAllPrimePermutations xs (x:acc)
    | otherwise = findAllPrimePermutations xs acc

solution = filter (/= Nothing) $ map  (\x -> findTrio x 0 []) $ map (primePermutations) $ findAllPrimePermutations [1000..10000] []

findTrio [] _ [] = Nothing
findTrio (x:xss) _ [] = findTrio xss x xss
findTrio xss current (x:xs)
    | (x + (x - current)) `elem` xs = Just (current, x, (x + (x - current)))
    | otherwise = findTrio xss current xs