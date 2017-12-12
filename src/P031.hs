-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
--
--     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
--
-- It is possible to make £2 in the following way:
--
--     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
--
-- How many different ways can £2 be made using any number of coins?

-- int target = 200;
-- int[] coinSizes = { 1, 2, 5, 10, 20, 50, 100, 200 };
-- int[] ways = new int[target+1];
-- ways[0] = 1;
--
-- for (int i = 0; i < coinSizes.Length; i++) {
--     for (int j = coinSizes[i]; j <= target; j++) {
--         ways[j] += ways[j - coinSizes[i]];
--     }
-- }

module P031 where

import Debug.Trace

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

findCombination :: Int -> [Int]
findCombination target = findCombination' target (insertAtReplace 0 1 (replicate (target + 1) 0)) coins (head coins)
    where
    findCombination' :: Int -> [Int] -> [Int] -> Int -> [Int]
    findCombination' _ ways [] _ = ways
    findCombination' target ways (c:[]) x
            | c > target = ways
            | (traceShow (target, ways, (c:[]), x) (x)) <= target = findCombination' target (insertAtReplace x ((ways !! x) + (ways !! (x - c))) ways) (c:[]) (x + 1)
            | otherwise = ways
    findCombination' target ways (c:cs:css) x
        | c > target = ways
        | (traceShow (target, ways, (c:cs:css), x) (x)) <= target = findCombination' target (insertAtReplace x ((ways !! x) + (ways !! (x - c))) ways) (c:cs:css) (x + 1)
        | otherwise = findCombination' target ways (cs:css) cs

insertAtReplace index value arr = as ++ (value:(tail bs))
            where (as,bs) = splitAt index arr

main :: Int -> Int
main n = last $ findCombination n