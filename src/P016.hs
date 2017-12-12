-- 21^5 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
-- What is the sum of the digits of the number 2^1000?


module P016 where

addPowerDigits :: Integer -> Integer -> Integer
addPowerDigits n p = sumd (n ^ p) 0

sumd :: Integer -> Integer -> Integer
sumd 0 acc = acc
sumd x acc = sumd (x `div` 10) (acc + (x `mod` 10))