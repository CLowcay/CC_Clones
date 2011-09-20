{-
CC_Clones - Classic games reimplemented
© Callum Lowcay 2006-2011

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Common.Util where

import Data.Char
import Data.Array
import Time

-- Compute the difference of two clock times to get a time delta
-- result is in picoseconds
clockTimeDiff :: ClockTime -> ClockTime -> Integer
clockTimeDiff time0 time1 = let timeDiff = diffClockTimes time1 time0 in
	max (tdPicosec timeDiff + toInteger (tdSec timeDiff) * 10^12) 0

-- Trim whitespace from a string
trim :: String -> String
trim = reverse.dropWhile isSpace.reverse.dropWhile isSpace

-- Concatenate two arrays
arrayCat :: Array Int a -> Array Int a -> Array Int a
arrayCat x y =
	array (0, cx + cy) (xl ++ map (\(i, e) -> (i + cx, e)) yl)
	where
		xl = assocs x
		yl = assocs y
		cx = length xl
		cy = length yl

-- Select the odd elements of a list
oddElems :: [a] -> [a]
oddElems [] = []
oddElems [x] = [x]
oddElems (x:_:rest) = x : oddElems rest

-- Select the even elements of a list
evenElems :: [a] -> [a]
evenElems [] = []
evenElems [_] = []
evenElems (_:y:rest) = y : evenElems rest

-- Return a sliding window of length n over a list
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
	| length window < n = []
	| otherwise = window : slidingWindow n (tail xs)
	where window = take n xs

