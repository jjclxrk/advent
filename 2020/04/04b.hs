-- Day 4: Passport Processing, Part Two
--
-- The line is moving more quickly now, but you overhear airport security 
-- talking about how passports with invalid data are getting through. Better 
-- add some data validation, quick!
--
-- You can continue to ignore the cid field, but each other field has strict 
-- rules about what values are valid for automatic validation:
--
--   - byr (Birth Year) - four digits; at least 1920 and at most 2002.
--   - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
--   - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
--   - hgt (Height) - a number followed by either cm or in:
--       - If cm, the number must be at least 150 and at most 193.
--       - If in, the number must be at least 59 and at most 76.
--   - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
--   - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
--   - pid (Passport ID) - a nine-digit number, including leading zeroes.
--   - cid (Country ID) - ignored, missing or not.
--
-- Your job is to count the passports where all required fields are both 
-- present and valid according to the above rules.
--
-- Count the number of valid passports - those that have all required fields 
-- and valid values. Continue to treat cid as optional. In your batch file, how 
-- many passports are valid?

import Data.Char (isDigit, isHexDigit)
import Data.List (sort)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

main = readFile "input.txt" >>= print . solve . parse 

parse :: String -> [[String]]
parse = map values . splitOn [""] . lines

values :: [String] -> [String] 
values = map (drop 4) . sort . filter ((/="cid") . take 3) . words . unwords 

solve :: [[String]] -> Int
solve = length . filter valid

valid :: [String] -> Bool
valid = all (uncurry id) . zip criteria . cycle

criteria :: [String -> Bool]
criteria = [validByr,validEcl,validEyr,validHcl,validHgt,validIyr,validPid]

between :: (Read a, Ord a) => a -> a -> String -> Bool
between lo hi xs = case readMaybe xs of
                     Just x  -> x >= lo && x <= hi
                     Nothing -> False

validByr :: String -> Bool
validByr = between 1920 2002

validIyr :: String -> Bool
validIyr = between 2010 2020

validEyr :: String -> Bool
validEyr = between 2020 2030

validHgt :: String -> Bool
validHgt hgt = let (digits, units) = span isDigit hgt
               in case units of
                 "in" -> between 59 76 digits
                 "cm" -> between 150 193 digits
                 _    -> False

validEcl :: String -> Bool
validEcl ecl = ecl `elem` ["amb","blu","brn","gry","grn","hzl","oth"]

validHcl :: String -> Bool
validHcl ('#':hex) = length hex == 6 && all isHexDigit hex
validHcl _         = False

validPid :: String -> Bool
validPid pid = length pid == 9 && all isDigit pid

