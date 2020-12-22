-- Day 10: Adapter Array, Part Two
--
-- To completely determine whether you have enough adapters, you'll need to 
-- figure out how many different ways they can be arranged. Every arrangement 
-- needs to connect the charging outlet to your device. The previous rules 
-- about when adapters can successfully connect still apply.
--
-- You glance back down at your bag and try to remember why you brought so many
-- adapters; there must be more than a trillion valid ways to arrange them! 
-- Surely, there must be an efficient way to count the arrangements.
--
-- What is the total number of distinct ways you can arrange the adapters to 
-- connect the charging outlet to your device?

import Data.IntMap (IntMap, (!), insertWith, singleton)
import Data.List (sort, tails)

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [(Int, [Int])]
parse =  buildGraph . sort . map read . lines

buildGraph :: [Int] -> [(Int, [Int])]
buildGraph xs = zipWith adjacencies (0:xs) (tails xs)

adjacencies :: Int -> [Int] -> (Int, [Int])
adjacencies x xs = (x, takeWhile (\x' -> x' - x < 4) xs)

solve :: [(Int, [Int])] -> Int
solve graph = foldl paths (singleton 0 1) graph ! fst (last graph)

paths :: IntMap Int -> (Int, [Int]) -> IntMap Int
paths acc (u, vs) = let n = acc ! u
                    in  foldr (\k -> insertWith (+) k n) acc vs 

