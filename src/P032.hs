-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
--
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
--
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

-- List products = new List();
-- long sum = 0;
-- long prod, compiled;
--
-- for (long m = 2; m < 100; m++) {
--     long nbegin = (m > 9) ? 123 : 1234;
--     long nend = 10000 / m + 1;
--
--     for (long n = nbegin; n < nend; n++) {
--         prod = m * n;
--         compiled = concat(concat(prod, n), m);
--         if (compiled >= 1E8 &&
--             compiled < 1E9 &&
--             isPandigital(compiled)) {
--             if (!products.Contains(prod)) {
--                 products.Add(prod);
--             }
--         }
--     }
-- }
--
-- for (int i = 0; i < products.Count; i++) {
--     sum += products[i];
-- }


module P032 where

import Data.List
import Data.List.Unique

solution = sum $ sortUniq $ outerLoop [2..99]
outerLoop [] = []
outerLoop (m:ms) = (innerLoop m [checkM..(10000 `div` m)]) ++ outerLoop ms
    where
    checkM = if (m > 9) then 123 else 1234
innerLoop _ [] = []
innerLoop m (n:ns)
    | isPandigitalList compiled = prod : innerLoop m ns
    | otherwise = innerLoop m ns
    where
    prod = m * n
    compiled = concat' prod n m
    (minV, maxV) = (100000000, 1000000000)

numbers = [1..9]

concat' :: Integer -> Integer -> Integer -> [Integer]
concat' x y z = concat [(digits x),(digits y),(digits z)]

isPandigitalList :: [Integer] -> Bool
isPandigitalList n = (sort n) == numbers

isPandigital :: Integer -> Bool
isPandigital n = (digits n) == numbers

digits :: Integer -> [Integer]
digits x = sort.reverse.digits' $ x
    where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x  `div` 10)

