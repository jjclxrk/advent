-- Day 1: Not Quite Lisp, Part One

-- Santa is trying to deliver presents in a large apartment building, but he 
-- can't find the right floor - the directions he got are a little confusing. 
-- He starts on the ground floor (floor 0) and then follows the instructions 
-- one character at a time.

-- An opening parenthesis, (, means he should go up one floor, and a closing 
-- parenthesis, ), means he should go down one floor.

-- The apartment building is very tall, and the basement is very deep; he will 
-- never find the top or bottom floors.

-- To what floor do the instructions take Santa?

main = readFile "input.txt" >>= print . solve . init

-- follow a list of directions to the destination floor
solve :: String -> Int
solve = foldl (\n p -> parse p n) 0 

-- turn a parenthesis into a direction: up or down
parse :: Enum a => Char -> (a -> a)  
parse '(' = succ
parse _   = pred
