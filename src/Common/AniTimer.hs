module Common.AniTimer (
	AniTimer, resetTimer, setTimer, advanceFrames
) where

import Control.Monad.State

data AniTimer = AniTimer {
	ttFrameSwap :: Double
} deriving (Eq, Show)

resetTimer :: AniTimer
resetTimer = AniTimer {ttFrameSwap = 0}

setTimer :: Double -> AniTimer
setTimer delay = AniTimer {ttFrameSwap = delay}

advanceFrames :: Int -> Double -> State AniTimer Int
advanceFrames delay frameDelay = do
	timer <- get

	let anidiff = (ttFrameSwap timer) - (fromIntegral delay)
	put$ timer {ttFrameSwap = if anidiff < 0
		then frameDelay + (anidiff `realMod` frameDelay)
		else anidiff
	}

	return$ if anidiff < 0
		then (truncate(abs anidiff / frameDelay)) + 1 else 0

realMod :: Double -> Double -> Double
x `realMod` y = x - (y * (fromIntegral$truncate (x / y)))

