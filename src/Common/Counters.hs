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

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Counters (
	CounterState,
	initCounter, updateCounter,
	addCounter, subCounter, setCounter, resetCounter,
	renderCounter
) where

import Common.AniTimer
import Common.Graphics
import Control.Monad
import Control.Monad.State
import Graphics.UI.SDL

data CounterState = CounterState {
	digits :: Sprite,
	display :: Int,
	target :: Int,
	aniTimer :: AniTimer,
	framesToAlignment :: Int,
	nDigits :: Int,
	changedDigits :: [Bool]
} deriving (Show)

-- Initialise a new CounterState
initCounter :: Sprite -> Int -> CounterState
initCounter digits nDigits = CounterState {
	digits = digits,
	display = 0, target = 0,
	aniTimer = resetTimer, framesToAlignment = 0,
	nDigits = nDigits,
	changedDigits = replicate nDigits False
}

-- The delay for counter frames, in milliseconds
frameDelay :: Double
frameDelay = ((1 :: Double) * 10^3) / (4 * 18)

smartDelay :: Int -> Int -> Double
smartDelay display target = frameDelay `min`
	(((2 :: Double) * 10^3) / (fromIntegral (target - display) * 18))

-- Update a counter state based on a time delta
updateCounter :: Int -> CounterState -> CounterState
updateCounter delay (state@CounterState {..}) =
	let
		(frames, aniTimer') =
			runState (advanceFrames delay (smartDelay display target)) aniTimer
		offset' = framesToAlignment - frames
		framesToAlignment' = if offset' < 0
			then offset' `mod` 18 else offset'
		advanceDigits = if offset' < 0
			then abs offset' `div` 18 + 1 else 0
		display' = if target > display
			then min target (display + advanceDigits)
			else if target < display
				then max target (display - advanceDigits)
				else display
	in state {
		framesToAlignment =
			if target == display && framesToAlignment == 0
				then 0 else framesToAlignment',
		aniTimer = aniTimer',
		display = display',
		changedDigits = if framesToAlignment' > framesToAlignment
			then zipWith (/=)
				(fixedFieldLeft nDigits 0 $ toDec display)
				(fixedFieldLeft nDigits 0 $ toDec display')
			else changedDigits
	}

-- Add a number to the counter
addCounter :: Int -> CounterState -> CounterState
addCounter n (state@(CounterState {target})) =
	state {target = target + n}

-- Subtract a number from the counter
subCounter :: Int -> CounterState -> CounterState
subCounter n (state@(CounterState {target})) =
	state {target = target - n}

-- Set the value of the counter
setCounter :: Int -> CounterState -> CounterState
setCounter n state = state {target = n}

-- Set the value of the counter, and do not animate the return
resetCounter :: Int -> CounterState -> CounterState
resetCounter n state = state {
	target = n, display = n,
	aniTimer = resetTimer, framesToAlignment = 0
}

-- Render the counter
renderCounter :: (Int, Int) -> CounterState -> IO ()
renderCounter (x, y) (state@CounterState {..}) = do
	let
		ddigits = fixedFieldLeft nDigits 0 $ toDec display
		digitOffsets = zip
			(reverse [0..(nDigits - 1)])
			(map digitOffset (zip changedDigits ddigits))

	ddisplay <- getVideoSurface

	forM_ digitOffsets $ \(iDigit, offset) ->
		renderSpriteLoopV ddisplay 0
			(x + (iDigit * 20), y) offset digits

	return ()

	where
		digitOffset (changed, d) = 
			((d * 18) - (if changed then framesToAlignment else 0)) `mod` 180
	
-- Pad or crop a list to a certain length
fixedFieldLeft :: Int -> a -> [a] -> [a]
fixedFieldLeft n padding xs = take n (xs ++ repeat padding)

-- Convert an integer to decimal digits, little endian
toDec :: Int -> [Int]
toDec n
	| n < 10 = [n]
	| otherwise = (n `rem` 10) : toDec (n `div` 10)

