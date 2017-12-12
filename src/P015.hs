-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
--
-- How many such routes are there through a 20×20 grid?

module P015 where

-- Very slow
lattice :: Int -> Int -> Int
lattice 0 0 = 1
lattice x 0 = lattice (x-1) 0
lattice 0 y = lattice (y-1) 0
lattice x y
  | x >= y    = (lattice (x-1) y) + (lattice x (y-1))
  | otherwise = (lattice y (x-1)) + (lattice (y-1) x)


lattice' x y = grid !! y !! x
grid = [row y | y <- [0..]]
  where
    row 0 = repeat 1 :: [Integer]
    row y = 1:[count x y | x <- [1..]]
    count x y = (lattice' (x-1) y) + (lattice' x (y-1))