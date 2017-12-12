-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.



module P017 where

main :: Int -> Int
main n = length.concat $ [ convertToWord x | x <- [1..n]]

digits,tens,zeros :: [[Char]]
digits = ["zero", "one","two","three","four","five","six","seven","eight","nine", "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen", "nineteen"]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
zeros = ["", "", "hundred", "thousand"]

convertToWord :: Int -> [Char]
convertToWord 0 = ""
convertToWord n
    | n >= 1000 = (digits !! (n `div` 1000)) ++ (zeros !! 3) ++ (and' (n `mod` 1000)) ++ (convertToWord (n `mod` 1000))
    | n >= 100 = (digits !! (n `div` 100)) ++ (zeros !! 2) ++ (and' (n `mod` 100)) ++ (convertToWord (n `mod` 100))
    | n >= 20 = (tens !! (n `div` 10)) ++ (zeros !! 1) ++ (convertToWord (n `mod` 10))
    | otherwise = (digits !! n)

and' :: Int -> [Char]
and' n
    | n == 0 = ""
    | otherwise = "and"
