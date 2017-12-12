-- You are given the following information, but you may prefer to do some research for yourself.
--
--     1 Jan 1900 was a Monday.
--     Thirty days has September,
--     April, June and November.
--     All the rest have thirty-one,
--     Saving February alone,
--     Which has twenty-eight, rain or shine.
--     And on leap years, twenty-nine.
--     A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?


module P019 where


days :: [[Char]]
days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

months :: [([Char], (Int -> Int))]
months = [("January", (\year -> 31)), ("February", getFebDays), ("March", (\year -> 31)), ("April", (\year -> 30)), ("May", (\year -> 31)), ("June", (\year -> 30)), ("July", (\year -> 31)), ("August", (\year -> 31)), ("September", (\year -> 30)), ("October", (\year -> 31)), ("November", (\year -> 30)), ("December", (\year -> 31))]

getFebDays :: Int -> Int
getFebDays year
    | year `mod` 100 == 0 && year `mod` 400 /= 0 = 28
    | year `mod` 4 == 0 = 29
    | otherwise = 28


main = length [m | y <- [1901..2000], m <- [0..11], d <- [0], findDayOfWeek (y,m,d) == 6]

findDayOfWeek :: (Int, Int, Int) -> Int
findDayOfWeek date = ((((findNumberOfDaysSince (1900, 0, 0) date) `mod` (length days))) `mod` (length days))

findNumberOfDaysSince :: (Int, Int, Int) -> (Int, Int, Int)  -> Int
findNumberOfDaysSince (fromY, fromM, fromD) (toY, toM, toD) = (findNumberOfDaysYears fromY toY) + (findNumberOfDaysMonths toY fromM toM) + (toD - fromD)

findNumberOfDaysMonths :: Int ->  Int -> Int -> Int
findNumberOfDaysMonths year from to
    | from == to = 0
    | otherwise = (findNumberOfDaysInMonth' year (months !! from)) + findNumberOfDaysMonths year (from + 1) to
        where
            findNumberOfDaysInMonth' year (_,yearF) = yearF year


findNumberOfDaysYears :: Int -> Int -> Int
findNumberOfDaysYears from to
    | from == to = 0
    | otherwise = (findNumberOfDaysInYear from) + findNumberOfDaysYears (from + 1) to

findNumberOfDaysInYear :: Int -> Int
findNumberOfDaysInYear year = findNumberOfDaysInYear' year months
    where
        findNumberOfDaysInYear' year [] = 0
        findNumberOfDaysInYear' year ((month,yearF):ms) = (yearF year) + findNumberOfDaysInYear' year ms