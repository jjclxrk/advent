-- Day 4: The Ideal Stocking Stuffer, Part One

-- Santa needs help mining some AdventCoins (very similar to bitcoins) to use 
-- as gifts for all the economically forward-thinking little girls and boys.

-- To do this, he needs to find MD5 hashes which, in hexadecimal, start with at 
-- least five zeroes. The input to the MD5 hash is some secret key (your puzzle 
-- input, given below) followed by a number in decimal. To mine AdventCoins, 
-- you must find Santa the lowest positive number (no leading zeroes: 1,2,3...)
-- that produces such a hash.

import Data.List (isPrefixOf)
import qualified Data.Hash.MD5 as MD5

main = readFile "input.txt" >>= print . solve . init

-- first natural with the required prefix
solve :: String -> Int
solve key = head [i | i <- [1..], "00000" `isPrefixOf` digest (key ++ show i)]

-- returns the MD5 hash of a String
digest :: String -> String
digest = MD5.md5s . MD5.Str

