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

