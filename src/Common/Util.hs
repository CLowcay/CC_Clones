module Common.Util where

import Char
import Data.Array
import Time

-- Compute the difference of two clock times to get a time delta
-- result is in picoseconds
clockTimeDiff :: ClockTime -> ClockTime -> Integer
clockTimeDiff time0 time1 = let timeDiff = diffClockTimes time1 time0 in
	max ((tdPicosec timeDiff) + (toInteger$ tdSec timeDiff) * 10^12) 0

-- Trim whitespace from a string
trim :: String -> String
trim = reverse.(dropWhile isSpace).reverse.(dropWhile isSpace)

-- Concatenate two arrays
arrayCat :: Array Int a -> Array Int a -> Array Int a
arrayCat x y =
	array (0, cx + cy) (xl ++ (map (\(i, e) -> (i + cx, e)) yl))
	where
		xl = assocs x
		yl = assocs y
		cx = length xl
		cy = length yl

-- Select the odd elements of a list
oddElems :: [a] -> [a]
oddElems [] = []
oddElems [x] = [x]
oddElems (x:_:rest) = x:(oddElems rest)

-- Select the even elements of a list
evenElems :: [a] -> [a]
evenElems [] = []
evenElems [_] = []
evenElems (_:y:rest) = y:(evenElems rest)

-- Return a sliding window of length n over a list
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
	| length xs < n = []
	| otherwise = (take n xs):(slidingWindow n (tail xs))

