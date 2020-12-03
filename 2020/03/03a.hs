-- Day 3: Toboggan Trajectory, Part 1
--
-- With the toboggan login problems resolved, you set off toward the airport. 
-- While travel by toboggan might be easy, it's certainly not safe: there's 
-- very minimal steering and the area is covered in trees. You'll need to see 
-- which angles will take you near the fewest trees.
--
-- Due to the local geology, trees in this area only grow on exact integer 
-- coordinates in a grid. You make a map (your puzzle input) of the open 
-- squares (.) and trees (#) you can see.
--
-- These aren't the only trees, though; due to something you read about once 
-- involving arboreal genetics and biome stability, the same pattern repeats to 
-- the right many times.
--
-- You start on the open square (.) in the top-left corner and need to reach 
-- the bottom (below the bottom-most row on your map).
-- 
-- The toboggan can only follow a few specific slopes (you opted for a cheaper 
-- model that prefers rational numbers); start by counting all the trees you 
-- would encounter for the slope right 3, down 1.
--
-- Starting at the top-left corner of your map and following a slope of right 3
-- and down 1, how many trees would you encounter?

main = readFile "input.txt" >>= print . solve . lines

solve :: [String] -> Int
solve rows@(r:_) = let width   = length r
                       indices = map (`mod` width) [0, 3 ..]
                   in  length . filter (== '#') $ zipWith (!!) rows indices

