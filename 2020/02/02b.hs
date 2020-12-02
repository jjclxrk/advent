-- Day 2: Password Philosophy, Part Two 
--
-- While it appears you validated the passwords correctly, they don't seem to
-- be what the Official Toboggan Corporate Authentication System is expecting.
--
-- The shopkeeper suddenly realizes that he just accidentally explained the 
-- password policy rules from his old job at the sled rental place down the 
-- street! The Official Toboggan Corporate Policy actually works a little 
-- differently.
-- 
-- Each policy actually describes two positions in the password, where 1 means 
-- the first character, 2 means the second character, and so on. (Be careful; 
-- Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of
-- these positions must contain the given letter. Other occurrences of the 
-- letter are irrelevant for the purposes of policy enforcement.
--
-- How many passwords are valid according to the new interpretation of the 
-- policies?

import Data.Char (isDigit)

data Policy = Policy Char Int Int

main = readFile "input.txt" >>= print . solve . map parse . lines

parse :: String -> (Policy, String)
parse line = case words line of
  [is, cs, pw] -> (uncurry (Policy (head cs)) $ parseIndices is, pw)

parseIndices :: String -> (Int, Int)
parseIndices rs = let (i, _:j) = break (== '-') rs in (read i - 1, read j - 1)

valid :: Policy -> String -> Bool
valid (Policy c i j) pw = (i' == c || j' == c) && i' /= j'
  where
    i' = pw !! i
    j' = pw !! j

solve :: [(Policy, String)] -> Int
solve = length . filter (uncurry valid)

