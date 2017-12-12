-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.


module P034 where

digits :: Integer -> [Integer]
digits x = reverse.digits' $ x
    where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x  `div` 10)

undigits :: [Integer] -> Integer
undigits [] = 0
undigits (x:xs) = x * (10 ^ length xs) + (undigits xs)

factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

isCurious n = n == (sum $ map (factorial) (digits n))

allCurious = [x | x <- [9..], isCurious x]