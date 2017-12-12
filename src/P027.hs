-- Euler discovered the remarkable quadratic formula:
--
-- n^(2)+n+41
--
-- It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39
-- . However, when n=40,40^2+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,41^2+41+41
--
-- is clearly divisible by 41.
--
-- The incredible formula n^2−79n+1601
-- was discovered, which produces 80 primes for the consecutive values 0≤n≤79
--
-- . The product of the coefficients, −79 and 1601, is −126479.
--
-- Considering quadratics of the form:
--
--     n2+an+b
--
-- , where |a|<1000 and |b|≤1000
--
-- where |n|
-- is the modulus/absolute value of n
-- e.g. |11|=11 and |−4|=4
--
-- Find the product of the coefficients, a
-- and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n=0.

module P027 where
import Data.Numbers.Primes
import Data.List
import Data.Ord

bruteForce :: [Integer] -> [Integer] -> [Integer] -> [(Integer, (Integer, Integer))]
bruteForce [] _ _ = []
bruteForce (x:xs) [] yss = bruteForce xs yss yss
bruteForce (x:xs) (y:ys) yss = primeLength x y (0, 0) : (bruteForce (x:xs) ys yss)
    where
        primeLength :: Integer -> Integer -> (Integer, Integer) -> (Integer, (Integer, Integer))
        primeLength a b (n, maxN)
            | n > maxN = primeLength a b (n, n)
            | isPrime $ formula a b n = primeLength a b (n + 1, maxN)
            | otherwise = (maxN, (a, b))
        formula :: Integer -> Integer -> Integer -> Integer
        formula a b n = abs $ n * n + a * n + b

solution :: [Integer] -> [Integer] -> [(Integer, (Integer, Integer))]
solution xs ys = sortBy (comparing $ fst) (bruteForce xs ys ys)

main :: [Integer] -> [Integer] -> (Integer, (Integer, Integer))
main xs ys = last $ solution xs ys