-- Day 9: Enocoding Error, Part Two
--
-- The final step in breaking the XMAS encryption relies on the invalid number 
-- you just found: you must find a contiguous set of at least two numbers in 
-- your list which sum to the invalid number from step 1. 
--
-- To find the encryption weakness, add together the smallest and largest 
-- number in this contiguous range.
--
-- What is the encryption weakness in your XMAS-encrypted list of numbers?

import Data.List (tails)
import Data.List.Split (divvy)

main = readFile "input.txt" >>= print . solve . parse

target :: Integer
target = 90433990

parse :: String -> [[Integer]]
parse = concatMap (tails . reverse) . tails . map read . init . lines

solve :: [[Integer]] -> Integer
solve xss = head $ [ minimum xs + maximum xs | xs <- xss, sum xs == target ]

