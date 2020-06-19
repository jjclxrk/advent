-- Day 5: Doesn't He Have Intern-Elves For This? Part One

-- Santa needs help figuring out which strings in his text file are naughty or 
-- nice. A nice string is one with all of the following properties:
--    - It contains at least three vowels (aeiou only), like aei, xazegov, or 
--      aeiouaeiouaeiou.
--    - It contains at least one letter that appears twice in a row, like xx, 
--      abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
--    - It does not contain the strings ab, cd, pq, or xy, even if they are 
--      part of one of the other requirements.

-- How many strings are nice?

import Data.List (group)
import Data.List.Split (divvy)

main = readFile "input.txt" >>= print . solve . lines

solve :: [String] -> Int
solve = length . filter nice

-- does the string meet all of the criteria
nice :: String -> Bool
nice xs = vowels xs && doubled xs && okPairs xs 

-- does the string contain at least 2 vowels
vowels :: String -> Bool
vowels = (>2) . length . filter (`elem` "aeiou")

-- does the string contain doubled letters
doubled :: String -> Bool
doubled = any ((>1) . length) . group

-- does the string not contain any of the disallowed letter pairs
okPairs :: String -> Bool
okPairs = all (`notElem` ["ab","cd","pq","xy"]) . divvy 2 1

