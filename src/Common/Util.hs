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

import Control.Monad.State
import Data.Array
import Data.Char
import System.Random

-- Run a function several times
times :: Int -> (a -> a) -> (a -> a)
times 0 _ = undefined
times 1 f = f
times i f = f.(times (i - 1) f)

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

-- Compute a random permutation of a list
permutation :: RandomGen r => [a] -> State r [a]
permutation [] = return []
permutation [x] = return [x]
permutation xs = do
	gen <- get
	let (i, gen1) = randomR (0, length xs - 1) gen
	let (x, xs') = pickOut i xs
	put gen1
	rest <- permutation xs' 
	return (x:rest)
	where
		pickOut i xs = let (l1, l2h:l2t) = splitAt i xs in (l2h, l1 ++ l2t)

