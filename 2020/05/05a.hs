-- Day 5: Binary Boarding, Part One
--
-- You board your plane only to discover a new problem: you dropped your 
-- boarding pass! You aren't sure which seat is yours, and all of the flight 
-- attendants are busy with the flood of people that suddenly made it through 
-- passport control.
--
-- You write a quick program to use your phone's camera to scan all of the 
-- nearby boarding passes (your puzzle input); perhaps you can find your seat 
-- through process of elimination.
--
-- Instead of zones or groups, this airline uses binary space partitioning to 
-- seat people. A seat might be specified like FBFBBFFRLR, where F means 
-- "front", B means "back", L means "left", and R means "right".
--
-- The first 7 characters will either be F or B; these specify exactly one of 
-- the 128 rows on the plane (numbered 0 through 127). Each letter tells you 
-- which half of a region the given seat is in. Start with the whole list of 
-- rows; the first letter indicates whether the seat is in the front (0 through 
-- 63) or the back (64 through 127). The next letter indicates which half of 
-- that region the seat is in, and so on until you're left with exactly one 
-- row.
--
-- The last three characters will be either L or R; these specify exactly one 
-- of the 8 columns of seats on the plane (numbered 0 through 7). The same 
-- process as above proceeds again, this time with only three steps. L means to 
-- keep the lower half, while R means to keep the upper half.
--
-- Every seat also has a unique seat ID: multiply the row by 8, then add the 
-- column.
--
-- As a sanity check, look through your list of boarding passes. What is the 
-- highest seat ID on a boarding pass?

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

solve :: [(String, String)] -> Int
solve = maximum . map seatID

