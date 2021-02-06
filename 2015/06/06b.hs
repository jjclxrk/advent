-- Day 6: Probably a Fire Hazard, Part One 
--
-- You just finish implementing your winning light pattern when you realize you
-- mistranslated Santa's message from Ancient Nordic Elvish.
--
-- The light grid you bought actually has individual brightness controls; each 
-- light can have a brightness of zero or more. The lights all start at zero.
--
-- The phrase turn on actually means that you should increase the brightness of
-- those lights by 1.
--
-- The phrase turn off actually means that you should decrease the brightness 
-- of those lights by 1, to a minimum of zero.
--
-- The phrase toggle actually means that you should increase the brightness of 
-- those lights by 2.
--
-- What is the total brightness of all lights combined after following Santa's 
-- instructions?

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Array.IO (IOUArray, getElems, newArray, readArray, writeArray)

main = do
  instrs <- lines <$> readFile "input.txt"
  lights <- newArray ((0,0), (999,999)) 0
  mapM_ (solve lights) instrs
  result <- sum <$> getElems lights
  print result

-- | Perform an instruction, updating the array of "lights". Light brightness 
--   is represented as an Int.
solve :: IOUArray (Int,Int) Int -> String -> IO () 
solve lights xs = do
  let (f, (xmin,ymin), (xmax,ymax)) = parse xs
  let indices = [(x,y) | x <- [xmin..xmax], y <- [ymin..ymax]]
  mapM_ (\i -> readArray lights i >>= writeArray lights i . f) indices

-- | Map an instruction string to a triple of an action and start and 
--   "top-left" and "bottom-right" indices of the rectangle of lights this
--   action should be performed upon.
parse :: String -> (Int -> Int, (Int,Int), (Int,Int))
parse xs = (f, start, stop)
  where
    f            = command xs
    [start,stop] = map parseIndex . filter (isDigit . head) $ words xs

-- | Parenthesise a string of the form "x,y" and read it as an (Int,Int) value.
parseIndex :: String -> (Int,Int)
parseIndex xs = read $ '(' : xs ++ ")"

-- | Map instruction actions to functions on integers.
command :: String -> (Int -> Int)
command xs
  | "toggle"  `isPrefixOf` xs = (+2)
  | "turn on" `isPrefixOf` xs = (+1)
  | otherwise                 = max 0 . subtract 1
 
