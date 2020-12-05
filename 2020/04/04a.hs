-- Day 4: Passport Processing, Part One
--
-- You arrive at the airport only to realize that you grabbed your North Pole 
-- Credentials instead of your passport. While these documents are extremely 
-- similar, North Pole Credentials aren't issued by a country and therefore 
-- aren't actually valid documentation for travel in most of the world.
--
-- It seems like you're not the only one having problems, though; a very long 
-- line has formed for the automatic passport scanners, and the delay could 
-- upset your travel itinerary.
--
-- Due to some questionable network security, you realize you might be able to 
-- solve both of these problems at the same time.
--
-- The automatic passport scanners are slow because they're having trouble 
-- detecting which passports have all required fields. The expected fields are 
-- as follows:
--
--   - byr (Birth Year)
--   - iyr (Issue Year)
--   - eyr (Expiration Year)
--   - hgt (Height)
--   - hcl (Hair Color)
--   - ecl (Eye Color)
--   - pid (Passport ID)
--   - cid (Country ID)
--
-- Passport data is validated in batch files (your puzzle input). Each passport
-- is represented as a sequence of key:value pairs separated by spaces or 
-- newlines. Passports are separated by blank lines.
--
-- Count the number of valid passports - those that have all required fields. 
-- Treat cid as optional. In your batch file, how many passports are valid?

import Data.List (delete, sort)
import Data.List.Split (splitOn)

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [[String]]
parse = map (map (take 3) . words . unwords) . splitOn [""] . lines

solve :: [[String]] -> Int
solve = length . filter valid

valid :: [String] -> Bool
valid = (==) ["byr","ecl","eyr","hcl","hgt","iyr","pid"] . delete "cid" . sort

