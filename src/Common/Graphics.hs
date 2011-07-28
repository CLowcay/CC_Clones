module Common.Graphics where

import Data.Array
import Graphics.UI.SDL

data Animation = Animation {
	surface :: Surface,
	frames :: Array Int Rect
} deriving (Show)

-- Render a frame from an animation
renderAnimation :: Surface -> Int -> Int -> Int -> Animation -> IO ()
renderAnimation dst frame x y animation = do
	blitSurface
		(surface animation) (Just$ (frames animation) ! frame)
		dst (Just$ Rect x y 0 0)
	return ()

-- Render a frame from an animation as a vertical loop
renderAnimationLoopV :: Surface -> Int ->
	Int -> Int -> Int -> Animation -> IO ()
renderAnimationLoopV dst frame x y offset animation = do
	let srect = (frames animation) ! frame
	blitSurface
		(surface animation) (Just$ srect {
			rectY = rectY srect + offset,
			rectH = rectH srect - offset})
		dst (Just$ Rect x y 0 0)
	blitSurface
		(surface animation) (Just$ srect {rectH = offset})
		dst (Just$ Rect x (y + (rectH srect - offset)) 0 0)
	return ()

