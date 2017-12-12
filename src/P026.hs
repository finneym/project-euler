-- A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
--
--     1/2	= 	0.5
--     1/3	= 	0.(3)
--     1/4	= 	0.25
--     1/5	= 	0.2
--     1/6	= 	0.1(6)
--     1/7	= 	0.(142857)
--     1/8	= 	0.125
--     1/9	= 	0.(1)
--     1/10	= 	0.1
--
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.


module P026 where

import Debug.Trace

findRecurring :: Integer -> Integer
findRecurring x = findRecurring' 1 x (replicate (fromInteger x) 0) 1
    where
    findRecurring' :: Integer -> Integer -> [Integer] -> Integer -> Integer
    findRecurring' n x y position
        | modded == 0 = 0
        | y !! moddedInt /= 0 = position - y !! moddedInt
        | otherwise = findRecurring'  (modded * 10) x (insertAt moddedInt position y) (position + 1)
            where
                modded = (n `mod` x)
                moddedInt = ( fromInteger modded)

insertAt :: Int -> Integer-> [Integer] -> [Integer]
insertAt z y xs = as ++ (y:( tail bs))
                  where (as,bs) = splitAt z xs

solution :: Integer -> (Integer, Integer) -> Integer
solution 0 (wn, _) = wn
solution n (wn, wl)
    | (wn - 1) == wl = wn
    | findRecurring (traceShow (n) (toInteger n)) > wl = solution (n-1) (n, findRecurring (toInteger n))
    | otherwise = solution (n - 1) (wn, wl)

main n = solution n (0,0)

