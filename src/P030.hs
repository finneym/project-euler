-- Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
--
--     1634 = 1^4 + 6^4 + 3^4 + 4^4
--     8208 = 8^4 + 2^4 + 0^4 + 8^4
--     9474 = 9^4 + 4^4 + 7^4 + 4^4
--
-- As 1 = 1^4 is not a sum it is not included.
--
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.


module P030 where

findSumPowers :: [Int] -> Int -> [Int]
findSumPowers [] _ = []
findSumPowers (x:xs) p
    | x == (sumPowers x 0 p) = x : findSumPowers xs p
    | otherwise = findSumPowers xs p

sumPowers :: Int -> Int -> Int -> Int
sumPowers 0 acc p = acc
sumPowers x acc p = sumPowers (x `div` 10) (acc + ((x `mod` 10) ^ p)) p

generateMin :: Int -> Int -> Int
generateMin 0 acc = acc
generateMin n 0 = generateMin (n - 1) 1
generateMin n acc = generateMin (n - 1) (acc * 10 + 1)

