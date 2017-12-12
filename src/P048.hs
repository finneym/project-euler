-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
--
-- Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.


module P048 where

solution n = sum $ map (\x -> x ^ x) [1,2..n]

