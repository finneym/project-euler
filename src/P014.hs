-- The following iterative sequence is defined for the set of positive integers:
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.


module P014 where

main :: Int -> (Int, Int)
main n = longestCollatzUnder n (0,0)

longestCollatzUnder :: Int -> (Int, Int) -> (Int, Int)
longestCollatzUnder 0 winning = winning
longestCollatzUnder n (wn,wl)
    | length (collatz n) > wl = longestCollatzUnder (n-1) (n, length (collatz n))
    | otherwise = longestCollatzUnder (n-1) (wn,wl)

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : (collatz (collatzNext n))

collatzNext :: Int -> Int
collatzNext n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = (3 * n) + 1