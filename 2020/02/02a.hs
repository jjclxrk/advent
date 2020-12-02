-- Day 2: Password Philosophy, Part One
--
-- Your flight departs in a few days from the coastal airport; the easiest way
-- down to the coast from here is via toboggan.
--
-- The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. 
-- "Something's wrong with our computers; we can't log in!" You ask if you can 
-- take a look.
--
-- Their password database seems to be a little corrupted: some of the 
-- passwords wouldn't have been allowed by the Official Toboggan Corporate 
-- Policy that was in effect when they were chosen.
--
-- To try to debug the problem, they have created a list (your puzzle input) of
-- passwords (according to the corrupted database) and the corporate policy 
-- when that password was set.
--
-- Each line gives the password policy and then the password. The password 
-- policy indicates the lowest and highest number of times a given letter must 
-- appear for the password to be valid. For example, 1-3 a means that the 
-- password must contain a at least 1 time and at most 3 times.
--
-- How many passwords are valid according to their policies?

import Data.Char (isDigit)

data Policy = Policy Char Int Int

main = readFile "input.txt" >>= print . solve . map parse . lines

parse :: String -> (Policy, String)
parse line = case words line of
  [rs, cs, pw] -> (uncurry (Policy (head cs)) $ parseRange rs, pw)

parseRange :: String -> (Int, Int)
parseRange rs = let (lo, _:hi) = break (== '-') rs in (read lo, read hi)

valid :: Policy -> String -> Bool
valid (Policy c lo hi) = (`elem` [lo..hi]) . length . filter (== c)

solve :: [(Policy, String)] -> Int
solve = length . filter (uncurry valid)

