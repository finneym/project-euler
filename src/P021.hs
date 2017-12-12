-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
--
-- Evaluate the sum of all the amicable numbers under 10000.


module P021 where


main :: Int -> Int
main n = sum $ [x | x <- [1..n], isAmicable x]

isAmicable :: Int -> Bool
isAmicable n = n == (d (d n)) && (d n) /= n

d :: Int -> Int
d n = (sum $ getFactors n)

getFactors :: Int -> [Int]
getFactors n = init (getFactors' n 1 (getMax n) ++ getFactors' n (getMax n) n)

getFactors' :: Int -> Int -> Int -> [Int]
getFactors' n cur max
    | cur > max = []
    | isFactor cur n = [cur] ++ getFactors' n (cur + 1) max
    | otherwise = getFactors' n (cur + 1) max

getMax :: Int -> Int
getMax = floor . sqrt . fromIntegral

isFactor :: Int -> Int -> Bool
isFactor f n = n `mod` f == 0

