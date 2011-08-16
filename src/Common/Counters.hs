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

module Common.Counters (
	CounterState(..),
	initCounter, updateCounter,
	addCounter, subCounter, setCounter, resetCounter,
	renderCounter
) where

import Common.Graphics
import Control.Monad
import Graphics.UI.SDL

data CounterState = CounterState {
	cs_digits :: Animation,
	cs_display :: Int,
	cs_target :: Int,
	cs_ttFrameSwap :: Integer,
	cs_framesToAlignment :: Int,
	cs_nDigits :: Int,
	cs_changedDigits :: [Bool]
} deriving (Show)

-- Initialise a new CounterState
initCounter :: Animation -> Int -> CounterState
initCounter digits nDigits = CounterState {
	cs_digits = digits,
	cs_display = 0, cs_target = 0,
	cs_ttFrameSwap = 0, cs_framesToAlignment = 0,
	cs_nDigits = nDigits,
	cs_changedDigits = take nDigits $ repeat False
}

-- The delay for counter frames, in picoseconds
frameDelay :: Integer
frameDelay = ((1::Integer) * 10^12) `div` (4 * 18)

-- Update a counter state based on a time delta
updateCounter :: Integer -> CounterState -> CounterState
updateCounter delay state =
	let
		anidiff = (cs_ttFrameSwap state) - delay
		advanceFrames = fromInteger$ if anidiff < 0
			then ((abs anidiff) `div` frameDelay) + 1 else 0
		offset' = framesToAlignment - advanceFrames
		framesToAlignment' = if offset' < 0
			then offset' `mod` 18 else offset'
		advanceDigits = if offset' < 0
			then ((abs offset') `div` 18) + 1 else 0
		display' = if target > display
			then min target (display + advanceDigits)
			else if target < display
				then max target (display - advanceDigits)
				else display
	in state {
		cs_framesToAlignment =
			if target == display && framesToAlignment == 0
				then 0 else framesToAlignment',
		cs_ttFrameSwap = if anidiff < 0
			then frameDelay + (anidiff `mod` frameDelay)
			else anidiff,
		cs_display = display',
		cs_changedDigits = if framesToAlignment' > framesToAlignment
			then map
				(\(a, b) -> a /= b) $ zip
					(adjLength nDigits$ toDec display)
					(adjLength nDigits$ toDec display')
			else cs_changedDigits state
	}
	where
		framesToAlignment = cs_framesToAlignment state
		nDigits = cs_nDigits state
		target = cs_target state
		display = cs_display state

-- Add a number to the counter
addCounter :: Int -> CounterState -> CounterState
addCounter n (state@(CounterState {cs_target = target})) =
	state {cs_target = target + n}

-- Subtract a number from the counter
subCounter :: Int -> CounterState -> CounterState
subCounter n (state@(CounterState {cs_target = target})) =
	state {cs_target = target - n}

-- Set the value of the counter
setCounter :: Int -> CounterState -> CounterState
setCounter n state = state {cs_target = n}

-- Set the value of the counter, and do not animate the return
resetCounter :: Int -> CounterState -> CounterState
resetCounter n state = state {
	cs_target = n, cs_display = n,
	cs_ttFrameSwap = 0, cs_framesToAlignment = 0
}

-- Render the counter
renderCounter :: Int -> Int -> CounterState -> IO ()
renderCounter x y state = do
	let
		digits = adjLength nDigits $ toDec$ cs_display state
		digitOffsets = zip
			(reverse [0..(nDigits - 1)])
			(map (digitOffset) (zip changedDigits digits))

	display <- getVideoSurface

	forM_ digitOffsets (\(iDigit, offset) -> do
			renderAnimationLoopV display 0
				(x + (iDigit * 20)) y offset (cs_digits state)
		)

	return ()

	where
		framesToAlignment = cs_framesToAlignment state
		nDigits = cs_nDigits state
		changedDigits = cs_changedDigits state
		digitOffset (changed, d) = 
			((d * 18) - (if changed then framesToAlignment else 0)) `mod` 180
	
-- Adjust a list to a specified length
adjLength :: Int -> [Int] -> [Int]
adjLength n xs = (take n xs) ++ 
	if length xs < n then take (n - (length xs)) (repeat 0) else []

-- Convert an integer to decimal digits, little endian
toDec :: Int -> [Int]
toDec n
	| n < 10 = [n]
	| otherwise = (n `mod` 10):(toDec (n `div` 10))

