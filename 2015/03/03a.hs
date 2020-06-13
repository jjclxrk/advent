-- Day 3: Perfectly Spherical Houses in a Vacuum, Part One

-- Santa is delivering presents to an infinite two-dimensional grid of houses.

-- He begins by delivering a present to the house at his starting location, and
-- then an elf at the North Pole calls him via radio and tells him where to 
-- move next. Moves are always exactly one house to the north (^), south (v), 
-- east (>), or west (<). After each move, he delivers another present to the 
-- house at his new location.

-- However, the elf back at the north pole has had a little too much eggnog, 
-- and so his directions are a little off, and Santa ends up visiting some 
-- houses more than once. How many houses receive at least one present?

import Data.List (foldl')
import Data.Set (Set, singleton, size, insert)
import qualified Data.Set as Set

main = readFile "input.txt" >>= print . solve . init

-- how many houses receive at least one present (what is the cardinality of 
-- the set of points visited)?
solve :: String -> Int
solve = size . solve' (0,0) (singleton (0,0))

-- follow a series of arrows keeping track of novel coordinates (the Set ps)
solve' :: (Int,Int) -> Set (Int, Int) -> String -> Set (Int, Int)
solve' _ ps ""     = ps
solve' p ps (c:cs) = solve' p' (insert p' ps) cs
    where
      p' = move c p 

-- follow an arrow from the current coordinate to the next
move :: Char -> (Int, Int) -> (Int, Int)
move '^' (x,y) = (x, y+1)
move '>' (x,y) = (x+1, y)
move 'v' (x,y) = (x, y-1)
move '<' (x,y) = (x-1, y)
 
