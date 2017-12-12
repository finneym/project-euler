-- Take the number 192 and multiply it by each of 1, 2, and 3:
--
--     192 × 1 = 192
--     192 × 2 = 384
--     192 × 3 = 576
--
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
--
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
--
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?


module P038 where

import Data.List
import Data.List.Unique


maxDigits :: Int
maxDigits = 9

maxStarter :: Integer
maxStarter = read $ replicate (maxDigits `div` 2) '9'

maxResult :: Integer
maxResult =  read $ replicate maxDigits '9'

getPandigital :: Integer -> Integer
getPandigital n = getP.last $ iterateWhile nextPan (n,n,2) condition
    where
        getP (_,p,_) = p
        nextPan (base,current,number) = (base, concatInteger current (base * number), (number + 1))
        concatInteger x y = read (show x ++ show y)
        condition (_,y,_) = y <= maxResult && isPandigital y

solution = maximum [getPandigital x | x <- [1..maxStarter], isPandigital x ]

iterateWhile :: ((a, a, a) -> (a, a, a)) -> (a, a, a) -> ((a, a, a) -> Bool) -> [(a, a, a)]
iterateWhile f x g = takeWhile g $ iterate f x


isPandigital :: Integer -> Bool
isPandigital n = (digits n) == (sortUniq.digits $ n) && (notElem 0 $ digits n)

digits :: Integer -> [Integer]
digits x = sort.reverse.digits' $ x
    where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x  `div` 10)
