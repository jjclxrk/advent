-- Day 7: Handy Haversacks, Part Two 
--
-- It's getting pretty expensive to fly these days - not because of ticket 
-- prices, but because of the ridiculous number of bags you need to buy!
--
-- ... be sure to count all of the bags, even if the nesting becomes 
-- topologically impractical!
--
-- How many individual bags are required inside your single shiny gold bag?

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

main = readFile "input.txt" >>= print . solve . map parse . lines

parse :: String -> (String, [(Int, String)])
parse input = (container, contained) 
  where
    split     = words input
    container = unwords . take 2 $ split
    contained = if   split!!4 == "no"
                then []
                else containees . drop 4 $ split

containees :: [String] -> [(Int, String)]
containees []     = []
containees (x:xs) = let count = read x
                        bag   = unwords . take 2 $ xs
                        rest  = drop 3 xs
                    in  (count, bag) : containees rest

solve :: [(String, [(Int, String)])] -> Int
solve = solve' 0 [(1, "shiny gold")] . HM.fromList

solve' :: Int -> [(Int, String)] -> HashMap String [(Int, String)] -> Int
solve' total []                 _    = total
solve' count ((qty, bag):queue) bags = 
  let children = HM.findWithDefault [] bag bags
      count'   = count + qty * sum (map fst children)
      queue'   = [(qty' * qty, bag') | (qty', bag') <- children] ++ queue
  in  solve' count' queue' bags 

