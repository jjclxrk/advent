-- Day 2: I Was Told There Would Be No Math, Part TWO

-- The elves are also running low on ribbon. Ribbon is all the same width, so 
-- they only have to worry about the length they need to order, which they 
-- would again like to be exact.

-- The ribbon required to wrap a present is the shortest distance around its 
-- sides, or the smallest perimeter of any one face. Each present also requires 
-- a bow made out of ribbon as well; the feet of ribbon required for the 
-- perfect bow is equal to the cubic feet of volume of the present. Don't ask 
-- how they tie the bow, though; they'll never tell.

-- How many total feet of ribbon should they order?

import Data.List (sort)
import Data.List.Split (splitOn)

main = readFile "input.txt" >>= print . solve . parse

-- input dimensions are in the form "LxWxH" where L, W, H are the length, width
-- and height of the presents
parse :: String -> [[Int]]
parse = map (map read . splitOn "x") . lines

solve :: [[Int]] -> Int
solve = sum . map ribbon
    where
      minPerimeter = (*2) . sum . init . sort
      ribbon       = \ss -> product ss + minPerimeter ss


