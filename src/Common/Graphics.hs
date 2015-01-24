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

module Common.Graphics (
	Sprite,
	renderSprite, renderSpritePart, renderSpriteLoopV,
	makeSprite, makeAnimationH, makeAnimationV
) where

import Data.Array
import Graphics.UI.SDL

data Sprite = Sprite {
	surface :: Surface,
	frames :: Array Int Rect
} deriving (Show)

-- Render a frame from an animation
renderSprite :: Surface -> Int -> (Int, Int) -> Sprite -> IO ()
renderSprite dst frame (x, y) sprite = do
	blitSurface
		(surface sprite) (Just$ frames sprite ! frame)
		dst (Just$ Rect x y 0 0)
	return ()

-- Render part of a frame
renderSpritePart :: Surface -> Sprite -> Int -> (Int, Int) -> Rect -> IO ()
renderSpritePart dst sprite frame (x, y) (Rect xOff yOff w h) = do
	let (Rect x0 y0 _ _) = frames sprite ! frame

	blitSurface (surface sprite)
		(Just$ Rect (x0 + xOff) (y0 + yOff) w h)
		dst (Just$ Rect x y 0 0)
	
	return ()

-- Render a frame from a sprite as a vertical loop
renderSpriteLoopV :: Surface -> Int ->
	(Int, Int) -> Int -> Sprite -> IO ()
renderSpriteLoopV dst frame (x, y) offset sprite = do
	let srect = frames sprite ! frame
	blitSurface
		(surface sprite) (Just$ srect {
			rectY = rectY srect + offset,
			rectH = rectH srect - offset})
		dst (Just$ Rect x y 0 0)
	blitSurface
		(surface sprite) (Just$ srect {rectH = offset})
		dst (Just$ Rect x (y + (rectH srect - offset)) 0 0)
	return ()

-- Make sprites from preloaded surfaces
makeSprite :: Surface -> (Int, Int) -> (Int, Int) -> Sprite
makeSprite surface (w, h) (xTile, yTile) =
	Sprite {
		surface = surface,
		frames = listArray (0, 0) [Rect (xTile * w) (yTile * h) w h]
	}

makeAnimationH :: Surface -> (Int, Int) -> Int -> Sprite
makeAnimationH surface (w, h) frames =
	Sprite {
		surface = surface,
		frames = listArray (0, (frames - 1))
			[Rect (i * w) 0 w h | i <- [0..(frames - 1)]]
	}

makeAnimationV :: Surface -> (Int, Int) -> Int -> Sprite
makeAnimationV surface (w, h) frames =
	Sprite {
		surface = surface,
		frames = listArray (0, (frames - 1))
			[Rect 0 (i * h) w h | i <- [0..(frames - 1)]]
	}

