-- Day 7: Handy Haversacks, Part One
--
-- You land at the regional airport in time for your next flight. In fact, it 
-- looks like you'll even have time to grab some food: all flights are 
-- currently delayed due to issues in luggage processing.
--
-- Due to recent aviation regulations, many rules (your puzzle input) are being
-- enforced about bags and their contents; bags must be color-coded and must 
-- contain specific quantities of other color-coded bags. Apparently, nobody 
-- responsible for these regulations considered how long they would take to 
-- enforce!
--
-- You have a shiny gold bag. If you wanted to carry it in at least one other 
-- bag, how many different bag colors would be valid for the outermost bag? 
-- (In other words: how many colors can, eventually, contain at least one shiny
-- gold bag?)
--
-- How many bag colors can eventually contain at least one shiny gold bag? 
-- (The list of rules is quite long; make sure you get all of it.)

import Data.List (nub)
import Data.Graph (graphFromEdges, reachable, transposeG)

main = readFile "input.txt" >>= print . solve . map parse . lines

parse :: String -> (String, String, [String])
parse input = (container, container, contained) 
  where
    split     = words input
    container = unwords . take 2 $ split
    contained = if   split!!4 == "no"
                then []
                else map unwords . containees . drop 5 $ split

containees :: [String] -> [[String]]
containees [] = []
containees xs = take 2 xs : containees (drop 4 xs)

solve :: [(String, String, [String])] -> Maybe Int
solve input = let (graph, _, getKey) = graphFromEdges input
                  graph'             = transposeG graph
              in  (pred . length . reachable graph') <$> getKey "shiny gold"

