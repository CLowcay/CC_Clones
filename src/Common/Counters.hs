module Common.Counters (
	CounterState(..),
	initCounter, updateCounter,
	addCounter, subCounter, setCounter, resetCounter,
	renderCounter
) where

import Common.Graphics
import Debug.Trace
import Graphics.UI.SDL

data CounterState = CounterState {
	cs_digits :: Animation,
	cs_display :: Int,
	cs_target :: Int,
	cs_ttFrameSwap :: Integer,
	cs_framesToAlignment :: Int,
	cs_nDigits :: Int
} deriving (Show)

-- Initialise a new CounterState
initCounter :: Animation -> Int -> CounterState
initCounter digits nDigits = CounterState {
	cs_digits = digits,
	cs_display = 0, cs_target = 0,
	cs_ttFrameSwap = 0, cs_framesToAlignment = 0,
	cs_nDigits = nDigits
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
		offset' = (cs_framesToAlignment state) - advanceFrames
		framesToAlignment = if offset' < 0
			then offset' `mod` 18 else offset'
		advanceDigits = if offset' < 0
			then ((abs offset') `div` 18) + 1 else 0
	in traceShow (target, display) $ state {
		cs_framesToAlignment =
			if target == display then 0 else framesToAlignment,
		cs_ttFrameSwap = if anidiff < 0
			then frameDelay + (anidiff `mod` frameDelay)
			else anidiff,
		cs_display = if target > display
			then max target (display + advanceDigits)
			else if target < display
				then min target (display - advanceDigits)
				else display
	}
	where
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
resetCounter n state = state {cs_target = n, cs_display = n}

-- Render the counter
renderCounter :: Int -> Int -> CounterState -> IO ()
renderCounter x y state = do
	let
		framesToAlignment = cs_framesToAlignment state
		nDigits = cs_nDigits state
		digits = adjLength nDigits $ toDec$ cs_display state
	display <- getVideoSurface
	mapM_ (\(iDigit, offset) -> do
			renderAnimationLoopV display 0
				(x + (iDigit * 20)) y offset (cs_digits state)
		)$ zip
			(reverse [0..(nDigits - 1)])
			(map (\d -> (d * 18) - framesToAlignment) digits)
	putStrLn$ ""
	return ()
	
-- Adjust a list to a specified length
adjLength :: Int -> [Int] -> [Int]
adjLength n xs = (take n xs) ++ 
	if length xs < n then take (n - (length xs)) (repeat 0) else []

-- Convert an integer to decimal digits, little endian
toDec :: Int -> [Int]
toDec n
	| n < 10 = [n]
	| otherwise = (n `mod` 10):(toDec (n `div` 10))

