-- n! means n × (n − 1) × ... × 3 × 2 × 1
--
-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
--
-- Find the sum of the digits in the number 100!


module P020 where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n-1))

sumd :: Integer -> Integer -> Integer
sumd 0 acc = acc
sumd x acc = sumd (x `div` 10) (acc + (x `mod` 10))

main :: Integer -> Integer
main n = sumd (factorial n) 0