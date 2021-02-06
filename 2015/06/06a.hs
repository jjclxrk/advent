-- Day 6: Probably a Fire Hazard, Part One
--
-- Because your neighbors keep defeating you in the holiday house decorating 
-- contest year after year, you've decided to deploy one million lights in a 
-- 1000x1000 grid.
--
-- Furthermore, because you've been especially nice this year, Santa has mailed
-- you instructions on how to display the ideal lighting configuration.
-- 
-- Lights in your grid are numbered from 0 to 999 in each direction; the lights 
-- at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions 
-- include whether to turn on, turn off, or toggle various inclusive ranges 
-- given as coordinate pairs. Each coordinate pair represents opposite corners 
-- of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore 
-- refers to 9 lights in a 3x3 square. The lights all start turned off.
--
-- To defeat your neighbors this year, all you have to do is set up your lights
-- by doing the instructions Santa sent you in order.
--
-- For example:
--   - turn on 0,0 through 999,999 would turn on (or leave on) every light.
--   - toggle 0,0 through 999,0 would toggle the first line of 1000 lights, 
--     turning off the ones that were on, and turning on the ones that were 
--     off.
--   - turn off 499,499 through 500,500 would turn off (or leave off) the 
--     middle four lights.
--
-- After following the instructions, how many lights are lit?

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Array.IO (IOUArray, getElems, newArray, readArray, writeArray)

main = do
  instrs <- lines <$> readFile "input.txt"
  lights <- newArray ((0,0), (999,999)) False
  mapM_ (solve lights) instrs
  result <- length . filter id <$> getElems lights
  print result

-- | Perform an instruction, updating the array of "lights". Lights are 
--   represented as Booleans: `True` meaning "on" and `False` meaning "off".
solve :: IOUArray (Int,Int) Bool -> String -> IO () 
solve lights xs = do
  let (f, (xmin,ymin), (xmax,ymax)) = parse xs
  let indices = [(x,y) | x <- [xmin..xmax], y <- [ymin..ymax]]
  mapM_ (\i -> readArray lights i >>= writeArray lights i . f) indices

-- | Map an instruction string to a triple of an action and start and 
--   "top-left" and "bottom-right" indices of the rectangle of lights this
--   action should be performed upon.
parse :: String -> (Bool -> Bool, (Int,Int), (Int,Int))
parse xs = (f, start, stop)
  where
    f            = command xs
    [start,stop] = map parseIndex . filter (isDigit . head) $ words xs

-- | Parenthesise a string of the form "x,y" and read it as an (Int,Int) value.
parseIndex :: String -> (Int,Int)
parseIndex xs = read $ '(' : xs ++ ")"

-- | Map instruction actions to functions on Booleans.
command :: String -> (Bool -> Bool)
command xs
  | "toggle"  `isPrefixOf` xs = not
  | "turn on" `isPrefixOf` xs = const True
  | otherwise                 = const False
 
