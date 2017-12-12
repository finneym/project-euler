-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

module P004 where

main :: Int -> Int
main digits = maximum $ findPalindromes (findMax digits) (findMin digits) (findMax digits) (findMax digits)

findPalindromes :: Int -> Int -> Int -> Int -> [Int]
findPalindromes max min x y
    | x < min = []
    | y < min = findPalindromes max min (x - 1) max
    | isPalindrome $ show (x * y) = x * y : findPalindromes max min x (y - 1)
    | otherwise = findPalindromes max min x (y - 1)

findMax :: Int -> Int
findMax n = (10 ^ n) - 1

findMin :: Int -> Int
findMin n = 10 ^ (n - 1)

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs
