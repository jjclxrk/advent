-- Day 6: Custom Customs, Part Two
--
-- As you finish the last group's customs declaration, you notice that you 
-- misread one word in the instructions:
--
-- You don't need to identify the questions to which anyone answered "yes"; you 
-- need to identify the questions to which everyone answered "yes"!
--
-- For each group, count the number of questions to which everyone answered 
-- "yes". What is the sum of those counts?

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [[String]]
parse = splitOn [""] . lines

solve :: [[String]] -> Int
solve = sum . map (S.size . foldr1 S.intersection . map S.fromList)

