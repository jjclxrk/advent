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
import Data.List (findIndices)
import Text.Read (readMaybe)

main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve = length . filter valid . lines

valid :: String -> Bool
valid line = 
  case words line of
    [indexStr, c:_, pw] -> 
      case parseIndices indexStr of
        Just (i, j) -> let ixs = map (+1) $ findIndices (==c) pw 
                       in  elem i ixs /= elem j ixs
        _           -> False
    _               -> False

parseIndices :: String -> Maybe (Int, Int)
parseIndices indexStr =
  case break (== '-') indexStr of
    (i, _:j) -> (,) <$> readMaybe i <*> readMaybe j
    _        -> Nothing

