-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.
--
-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?


module P042 where

import Data.CSV
import Text.ParserCombinators.Parsec
import Data.List
import Data.Char

triangleNumbers = [triangleNumber $ x | x <- [1..]]

triangleNumber n = floor (0.5 * n * (n + 1))

isTriangleNumber n = find' n triangleNumbers

find' :: Integer -> [Integer] -> Bool
find' n (x:xs)
    | x > n = False
    | x == n = True
    | otherwise = find' n xs

wordValue :: [Char] -> Integer
wordValue [] = 0
wordValue (x:xs) = toInteger ((ord x) - (ord 'A') + 1) + wordValue xs

isTriangleWord x = isTriangleNumber $ wordValue x

findNumberOfTriangleWords xs = length $ filter (\x -> isTriangleWord x) xs


main = do
    result <- parseFromFile csvFile "src/data/P042/words.txt"
    let solution = findNumberOfTriangleWords.concat.concat $ result
    print solution