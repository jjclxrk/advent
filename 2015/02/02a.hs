-- Day 2: I Was Told There Would Be No Math, Part One

-- The elves are running low on wrapping paper, and so they need to submit an 
-- order for more. They have a list of the dimensions (length l, width w, and 
-- height h) of each present, and only want to order exactly as much as they 
-- need.

-- Fortunately, every present is a box (a perfect right rectangular prism), 
-- which makes calculating the required wrapping paper for each gift a little 
-- easier: find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. 
-- The elves also need a little extra paper for each present: the area of the 
-- smallest side.

-- All numbers in the elves' list are in feet. How many total square feet of 
-- wrapping paper should they order?

import Data.List.Split (splitOn)

main = readFile "input.txt" >>= print . solve . parse

-- input dimensions are in the form "LxWxH" where L, W, H are the length, width
-- and height of the presents
parse :: String -> [[Int]]
parse = map (map read . splitOn "x") . lines


solve :: [[Int]] -> Int
solve = sum . map surfaceArea
    where
      sides [l,w,h] = [l*w, w*h, h*l]
      surfaceArea   = (\ss -> 2 * sum ss + minimum ss) . sides 

