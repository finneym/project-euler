-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.


module P009 where

main :: Int -> [(Int,Int,Int)]
main x = [(a,b,getC(a,b))|a<-[1..x], b<-[1..x], applyFilters (a,b) x]

applyFilters :: (Int, Int) -> Int -> Bool
applyFilters (a,b) x = isSquare (squareSum (a,b)) && a < b && a + b + getC (a,b) == x

getC :: (Int, Int) -> Int
getC (a, b) = floor $ sqrt $ (fromIntegral (squareSum (a, b))::Double)

squareSum :: (Int, Int) -> Int
squareSum (a,b) = (a^2)+(b^2)

isSquare :: Int -> Bool
isSquare n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double)