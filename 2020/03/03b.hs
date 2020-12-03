-- Day 3: Toboggan Trajectory, Part 2
--
-- Time to check the rest of the slopes - you need to minimize the probability 
-- of a sudden arboreal stop, after all.
--
-- Determine the number of trees you would encounter if, for each of the 
-- following slopes, you start at the top-left corner and traverse the map all 
-- the way to the bottom:
--
--  - Right 1, down 1.
--  - Right 3, down 1. (This is the slope you already checked.)
--  - Right 5, down 1.
--  - Right 7, down 1.
--  - Right 1, down 2.

-- What do you get if you multiply together the number of trees encountered on 
-- each of the listed slopes?

main = readFile "input.txt" >>= print . solve . lines

solve :: [String] -> Int
solve rows = product $ solve' (skip rows) 1 : map (solve' rows) [1,3,5,7]

solve' :: [String] -> Int -> Int
solve' rows@(r:_) slope = length . filter (== '#') $ zipWith (!!) rows indices
  where 
    width   = length r
    indices = map (`mod` width) [0, slope ..]

skip :: [a] -> [a]
skip (x:_:xs) = x : skip xs
skip xs       = xs

