-- If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
--
-- {20,48,52}, {24,45,51}, {30,40,50}
--
-- For which value of p ≤ 1000, is the number of solutions maximised?
-- P = a + b + sqrt(a^2 + b^2)
module P039 where

import Data.List
import Data.Ord (comparing)

perimeter :: (Double, Double) -> Double
perimeter (a,b) = a + b + (sqrt $ ((a^2) + (b^2)))

brute = last . sortBy (comparing snd) $ map (\x -> ([head x], length x)) . group . sort . filter (\p -> p <= 1000) $ [perimeter (a,b) | a <- [1..1000], b <- [a..1000]]