-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?


module P022 where

import Data.CSV
import Text.ParserCombinators.Parsec
import Data.List
import Data.Char

main = do
    result <- parseFromFile csvFile "src/data/P022/names.txt"
    let concatted = concat.concat $ result
    let sorted = sort concatted
    let total = getTotalScore 1 sorted
    print total

getTotalScore :: Int -> [[Char]] -> Int
getTotalScore _ [] = 0
getTotalScore i (x:xs) = (i * score x) + getTotalScore (i+1) xs

score :: [Char] -> Int
score [] = 0
score (x:xs) = ((ord x)  - (ord 'A') + 1) + score xs
