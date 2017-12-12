-- Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
-- Triangle 	  	Tn=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
-- Pentagonal 	  	Pn=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
-- Hexagonal 	  	Hn=n(2n−1) 	  	1, 6, 15, 28, 45, ...
--
-- It can be verified that T285 = P165 = H143 = 40755.
--
-- Find the next triangle number that is also pentagonal and hexagonal.


module P045 where

-- t^2 + t
-- 3p^2 - p
-- 4h^2 - 2h

import Data.List
import Helpers
-- hexagonal are a subset of triangle
triangle, pentagonal, hexagonal :: Integer -> Integer
triangle n = n * (n + 1) `div` 2
pentagonal n = n * (3 * n - 1) `div` 2
hexagonal n = n * (2 * n - 1)

triangleList, pentagonalList, hexagonalList :: [Integer]
triangleList = map (triangle) [285..]
pentagonalList = map (pentagonal) [165..]
hexagonalList = map (hexagonal) [143..]

brute = intersect pentagonalList hexagonalList

solution p = case binarySearch (hexagonal) 1 p (pentagonal p) of
    Nothing -> solution (p + 1)
    Just h -> hexagonal h