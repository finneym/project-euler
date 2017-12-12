-- The Fibonacci sequence is defined by the recurrence relation:
--
--     Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
--
-- Hence the first 12 terms will be:
--
--     F1 = 1
--     F2 = 1
--     F3 = 2
--     F4 = 3
--     F5 = 5
--     F6 = 8
--     F7 = 13
--     F8 = 21
--     F9 = 34
--     F10 = 55
--     F11 = 89
--     F12 = 144
--
-- The 12th term, F12, is the first term to contain three digits.
--
-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?



module P025 where
import Debug.Trace
import Data.List

-- Slow
fibonacciNext :: Int -> Int
fibonacciNext 1 = 1
fibonacciNext 2 = 1
fibonacciNext n = (fibonacciNext (n - 1)) + (fibonacciNext (n - 2))

fibonacci :: Int -> [Int]
fibonacci n = [fibonacciNext x | x <- [1..n]]


-- Fast

fibNext :: [Integer] -> Integer
fibNext [] = 1
fibNext [1] = 1
fibNext (x:y:_) = x + y

-- fib :: (Integer -> Bool) -> Integer -> [Integer] -> Integer
fib f n xs
    | length xs == 0 = fib f (n + 1) ((fibNext xs) : xs)
    | (traceShow (head xs) (f xs)) = n
    | otherwise = fib f (n + 1) ((fibNext xs) : xs)




main n = fib (\x -> length ( show ((head x))) >= n) 0 []
