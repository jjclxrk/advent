-- Day 10: Adapter Array, Part One
--
-- Patched into the aircraft's data port, you discover weather forecasts of a 
-- massive tropical storm. Before you can figure out whether it will impact 
-- your vacation plans, however, your device suddenly turns off!
--
-- Its battery is dead.
--
-- You'll need to plug it in. There's only one problem: the charging outlet 
-- near your seat produces the wrong number of jolts. Always prepared, you make
-- a list of all of the joltage adapters in your bag.
--
-- Each of your joltage adapters is rated for a specific output joltage (your 
-- puzzle input). Any given adapter can take an input 1, 2, or 3 jolts lower 
-- than its rating and still produce its rated output joltage.
--
-- In addition, your device has a built-in joltage adapter rated for 3 jolts 
-- higher than the highest-rated adapter in your bag. (If your adapter list 
-- were 3, 9, and 6, your device's built-in adapter would be rated for 12 
-- jolts.)
--
-- Treat the charging outlet near your seat as having an effective joltage 
-- rating of 0.
--
-- Since you have some time to kill, you might as well test all of your 
-- adapters. Wouldn't want to get to your resort and realize you can't even 
-- charge your device!
--
-- If you use every adapter in your bag at once, what is the distribution of 
-- joltage differences between the charging outlet, the adapters, and your 
-- device?

import Data.List (sort)

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [Int]
parse = (\xs -> zipWith subtract (0:xs) xs) . sort . map read . lines

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

solve :: [Int] -> Int
solve xs = count 1 xs * (1 + count 3 xs)

