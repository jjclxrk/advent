-- Day Five: Binary Boarding, Part Two 
--
-- Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
--
-- It's a completely full flight, so your seat should be the only missing 
-- boarding pass in your list. However, there's a catch: some of the seats at 
-- the very front and back of the plane don't exist on this aircraft, so 
-- they'll be missing from your list as well.
-- 
-- Your seat wasn't at the very front or back, though; the seats with IDs +1 
-- and -1 from yours will be in your list.
-- 
-- What is the ID of your seat?

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [(String, String)]
parse = map (splitAt 7) . lines

narrow :: String -> (Int, Int) -> Int
narrow []     (lo, _ ) = lo
narrow (c:cs) (lo, hi)
  | c `elem` "BR" = narrow cs (mid + 1, hi)
  | otherwise     = narrow cs (lo, mid - 1)
  where mid = (lo + hi) `div` 2

seatID :: (String, String) -> Int
seatID (rows, cols) = 8 * narrow rows (0, 127) + narrow cols (0, 7)

solve :: [(String, String)] -> [Int]
solve xs = [ x | x <- [1..937], missing x seats ]
  where seats = IntSet.fromList . map seatID $ xs

missing :: Int -> IntSet -> Bool
missing x seats = IntSet.lookupLE x seats == Just (x - 1) 
               && IntSet.lookupGE x seats == Just (x + 1)

