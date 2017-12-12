-- An irrational decimal fraction is created by concatenating the positive integers:
--
-- 0.123456789101112131415161718192021...
--
-- It can be seen that the 12th digit of the fractional part is 1.
--
-- If dn represents the nth digit of the fractional part, find the value of the following expression.
--
-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000


module P040 where

allDigits = (concat $ map (digits) $ iterate (\x -> x + 1) 1)

solution = product [allDigits !! (n-1) | n <- [1,10,100,1000,10000,100000,1000000]]



digits :: Integer -> [Integer]
digits x = reverse.digits' $ x
    where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x  `div` 10)

undigits :: [Integer] -> Integer
undigits [] = 0
undigits (x:xs) = x * (10 ^ length xs) + (undigits xs)