-- Day 1: Not Quite Lisp, Part Two

-- Now, given the same instructions, find the position of the first character 
-- that causes him to enter the basement (floor -1). The first character in 
-- the instructions has position 1, the second character has position 2, and 
-- so on.

-- What is the position of the character that causes Santa to first enter the 
-- basement?

main = readFile "input.txt" >>= print . solve (0, 0)

-- follow the directions until the first time we enter the basement
-- return the index (1-based) of the instruction that causes us to enter
-- the basement (floor -1).
solve :: (Int, Int) -> String -> Int
solve (-1, i) _ = i
solve (n,  i) (x:xs)  
  | x == '('  = solve (n+1, i+1) xs
  | otherwise = solve (n-1, i+1) xs 

