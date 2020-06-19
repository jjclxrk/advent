-- Day 4: The Ideal Stocking Stuffer, Part Two

-- Now find one that starts with six zeroes.

import Data.List (isPrefixOf)
import qualified Data.Hash.MD5 as MD5

main = readFile "input.txt" >>= print . solve . init

-- first natural with the required prefix
solve :: String -> Int
solve key = head [i | i <- [1..], "000000" `isPrefixOf` digest (key ++ show i)]

-- returns the MD5 hash of a String
digest :: String -> String
digest = MD5.md5s . MD5.Str

