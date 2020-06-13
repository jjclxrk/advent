-- Day 3: Perfectly Spherical Houses in a Vacuum, Part Two

-- The next year, to speed up the process, Santa creates a robot version of 
-- himself, Robo-Santa, to deliver presents with him.

-- Santa and Robo-Santa start at the same location (delivering two presents to 
-- the same starting house), then take turns moving based on instructions from 
-- the elf, who is eggnoggedly reading from the same script as the previous 
-- year.

-- This year, how many houses receive at least one present?<Paste>

import Data.Set (Set, singleton, size, insert)
import qualified Data.Set as Set
import Data.List (foldl')

main = readFile "input.txt" >>= print . solve . init

-- follow an arrow from the current coordinate to the next
move :: Char -> (Int, Int) -> (Int, Int)
move '^' (x,y) = (x, y+1)
move '>' (x,y) = (x+1, y)
move 'v' (x,y) = (x, y-1)
move '<' (x,y) = (x-1, y)

-- how many houses receive at least one present (what is the cardinality of 
-- the set of points visited)?
solve :: String -> Int
solve = size . solve' (0,0) (0,0) (singleton (0,0))

-- follow a series of arrows keeping track of novel coordinates (the Set ps), 
-- the points s and r keep track of the current position of Santa and 
-- Robo-Santa, repectively. They swap position as arguments in each recursive
-- call to model "taking turns" reading instructions.
solve' :: (Int,Int) -> (Int, Int) -> Set (Int, Int) -> String -> Set (Int, Int)
solve' _ _ ps ""     = ps
solve' s r ps (c:cs) = solve' r s' (insert s' ps) cs
    where
      s' = move c s 
 
