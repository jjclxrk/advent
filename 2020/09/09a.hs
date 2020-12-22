-- Day 9: Enocoding Error, Part One
--
-- With your neighbor happily enjoying their video game, you turn your 
-- attention to an open data port on the little screen in the seat in front of 
-- you.
--
-- Though the port is non-standard, you manage to connect it to your computer 
-- through the clever use of several paperclips. Upon connection, the port 
-- outputs a series of numbers (your puzzle input).
--
-- The data appears to be encrypted with the eXchange-Masking Addition System 
-- (XMAS) which, conveniently for you, is an old cypher with an important 
-- weakness.
--
-- XMAS starts by transmitting a preamble of 25 numbers. After that, each 
-- number you receive should be the sum of any two of the 25 immediately 
-- previous numbers. The two numbers will have different values, and there 
-- might be more than one such pair.
--
-- The first step of attacking the weakness in the XMAS data is to find the 
-- first number in the list (after the preamble) which is not the sum of two of 
-- the 25 numbers before it. What is the first number that does not have this 
-- property?

import Data.List.Split (divvy)

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [[Integer]]
parse = tail . divvy 26 1 . reverse .  map read . lines

solve :: [[Integer]] -> Integer
solve = head . last . filter (\(x:xs) -> x `notElem` twoSums xs)

twoSums :: [Integer] -> [Integer]
twoSums []       = []
twoSums (x : xs) = map (+x) xs ++ twoSums xs

