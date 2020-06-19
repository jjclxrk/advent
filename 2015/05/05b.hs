-- Day 5: Doesn't He Have Intern-Elves For This? Part Two

-- Realizing the error of his ways, Santa has switched to a better model of 
-- determining whether a string is naughty or nice. None of the old rules 
-- apply, as they are all clearly ridiculous. Now, a nice string is one with 
-- all of the following properties:
--     - It contains a pair of any two letters that appears at least twice in 
--       the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but
--       not like aaa (aa, but it overlaps).
--     - It contains at least one letter which repeats with exactly one letter 
--       between them, like xyx, abcdefeghi (efe), or even aaa.

-- How many strings are nice under these new rules?

import Data.List (isInfixOf)

main = readFile "input.txt" >>= print . solve . lines

solve :: [String] -> Int
solve = length . filter nice

nice :: String -> Bool
nice xs = repeat1 xs && repeat2 xs 

-- contains a pair of any two letters that appears at least twice without
-- overlapping. O(n^2) ? -- there is perhaps a smarter way to check this.
repeat1 :: String -> Bool
repeat1 (x:y:xs) = [x,y] `isInfixOf` xs || repeat1 (y:xs)
repeat1 _        = False 

-- contains at least one letter which repeats with exactly one letter between
-- them.
repeat2 :: String -> Bool
repeat2 xs@(x:_:y:_) = x == y || repeat2 (tail xs)
repeat2 _            = False

