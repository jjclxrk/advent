-- Day 1: Report Repair, Part Two 
-- 
-- The Elves in accounting are thankful for your help; one of them even offers
-- you a starfish coin they had left over from a past vacation. They offer you
-- a second one if you can find three numbers in your expense report that meet
-- the same criteria.
--
-- In your expense report, what is the product of the three entries that sum 
-- to 2020?

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [Int]
parse = map read . lines

solve :: [Int] -> Int
solve xs = head [ x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020 ]

