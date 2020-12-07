-- Day 6: Custom Customs, Part One
--
-- As your flight approaches the regional airport where you'll switch to a much
-- larger plane, customs declaration forms are distributed to the passengers.
-- 
-- The form asks a series of 26 yes-or-no questions marked a through z. All you 
-- need to do is identify the questions for which anyone in your group answers 
-- "yes". Since your group is just you, this doesn't take very long.
--
-- However, the person sitting next to you seems to be experiencing a language 
-- barrier and asks if you can help. For each of the people in their group, you 
-- write down the questions for which they answer "yes", one per line.
--
-- Another group asks for your help, then another, and eventually you've 
-- collected answers from every group on the plane (your puzzle input). Each 
-- group's answers are separated by a blank line, and within each group, each 
-- person's answers are on a single line.
--
-- For each group, count the number of questions to which anyone answered 
-- "yes". What is the sum of those counts?

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [String]
parse = map concat . splitOn [""] . lines

solve = sum . map (S.size . S.fromList)

