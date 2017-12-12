-- Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
--
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
-- right, down, left, left, up, up, right right, right, down, down, down, left, left, left , left, up, up, up, up, right, right, right ,right
-- 0 1 2 2 3 3 0 0 0 1 1 1 2 2 2 2 3 3 3 3
--     right 1 1 1 1
--     left 1 1 1 1
--     down 1 1 1 1
--     up 1 1 1 1
-- 1 2
-- 4 3
-- It can be verified that the sum of the numbers on the diagonals is 101.
--
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?


module P028 where

import Debug.Trace

fast :: Int -> Int
fast n = sum . scanl (+) 1 . concat . map (replicate 4) $ [2,4..(n-1)]


generateSp :: Int -> [[Int]]
generateSp n = (generateSp' (spiralPattern n) (replicate (sides n) (replicate (sides n) 0)) (middle (sides n)))
    where
        generateSp' :: [((Int, Int), Int)] -> [[Int]] -> (Int, Int) -> [[Int]]
        generateSp' [] ys middles = ys
        generateSp' ((ij,v):xs) ys middles = generateSp' xs (insert2D ij middles v ys) middles
        sides n = floor $ sqrt $ fromIntegral n
        middle n = ((n `div` 2), (n `div` 2))

spiralPattern :: Int -> [((Int, Int), Int)]
spiralPattern n = spiralPattern' 1 n (0,0) 0
    where
    spiralPattern' :: Int -> Int -> (Int, Int) -> Int -> [((Int, Int), Int)]
    spiralPattern' n maxN coords direction
     | n > maxN = []
     | otherwise = (coords, n) : spiralPattern' (n + 1) maxN newCoords (direction + 1)
        where
            newCoords = (clockwise direction) coords

-- generateSpiral :: Int -> [[Int]]
-- generateSpiral n = generateSpiral' 2 n [[1]] (0,0) clockwise 0
--     where
--         generateSpiral' :: Int -> Int -> [[Int]] -> (Int, Int) -> [((Int,Int) -> (Int,Int))] -> Int -> [[Int]]
--         generateSpiral' n maxN xs coords directions direction
--             | (traceShow ("generateSpiral'", n, maxN, coords, direction) n) > maxN = xs
--             | otherwise = generateSpiral' (n+1) maxN updatedXs (dir coords) directions (direction + 1)
--                 where
--                 updatedXs = (insert2D (dir coords) n xs)
--                 dir = directions !! direction


insert2D :: (Int, Int) -> (Int, Int) -> Int -> [[Int]] -> [[Int]]
insert2D (i,j) (midI, midJ) n xs =  (insertAtReplace (midI + i) (insertAtReplace (midJ + j) n (xs !! (midI + i))) xs)
    where
        insertAtReplace index value arr = as ++ (value:(tail bs))
            where (as,bs) = splitAt index arr

-- 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
-- 0 1 2 2 3 3 4 4 4 5 5  5  6  6  6  6  7  7  7  7
-- 0 1 2 2 3 3 0 0 0 1 1  1  2  2  2  2  3  3  3  3
-- 0 0 1 1 2 2 3 3 4 4 5  5  6  6  7  7  8  8  9  9
clockwise :: Int -> ((Int,Int) -> (Int,Int))
clockwise n = directions !! (forumla n)
    where
    forumla n = floor ((sqrt $ fromIntegral (4 * n + 1)) - 1)
    directions = cycle [right, down, left, up]

clockwise' :: Int -> Int
clockwise' n = floor ((sqrt $ fromIntegral (4 * n + 1)) - 1)


up, right, down, left:: (Int,Int) -> (Int,Int)
up (x,y) = (x - 1, y )
right (x,y) = (x, y + 1)
down (x,y) = (x + 1, y)
left (x,y) = (x , y - 1)


