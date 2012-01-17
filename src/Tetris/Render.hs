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

module Tetris.Render (
	renderFrame
) where

import Common.Counters
import Common.Graphics
import Common.HighScores
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Array
import Graphics.UI.SDL
import qualified Common.Queue as Q
import qualified Data.Map as M
import Tetris.Assets
import Tetris.GameState

fieldX = 13 :: Int
fieldY = 13 :: Int

realX x = fieldX + (x * tileS)
realY y = fieldY + ((19 - y) * tileS)

-- Render a frame
renderFrame :: GameState -> ReaderT Assets IO ()
renderFrame state@(GameState {..}) = do
	Assets {..} <- ask
	display <- liftIO getVideoSurface

	-- render field
	forM_ (assocs field) $ \((x, y), tm) -> liftIO$
		when (y < 20) $
			case tm of
				Nothing -> do
					fillRect display
						(Just$Rect (realX x) (realY y) tileS tileS) (Pixel 0)
					return ()
				Just tile ->
					renderAnimation display (fieldTileFrame y)
						(realX x) (realY y) (gfx M.! tile)

	-- render brick
	let
		brickCoords =
			map (\(x, y) -> (x + currentPos, currentHeight - y)) $
				srsCoords currentBrick currentRotation
		brickAni = gfx M.! (tile currentBrick)
		brickHOffset = case currentSlide of
			SlideLeft -> slideFTA
			SlideRight -> - slideFTA
		brickVOffset = if gracePeriod then 0 else - downFTA
	forM_ brickCoords $ \(x, y) -> liftIO$
		renderAnimation display 0
			((realX x) + brickHOffset) ((realY y) + brickVOffset) brickAni

	-- render border
	liftIO$ do
		renderAnimation display 0 0 0 (gfx M.! FrameH)
		renderAnimation display 0 0 13 (gfx M.! FrameV)
		renderAnimation display 0 0 533 (gfx M.! FrameH)
		renderAnimation display 0 273 13 (gfx M.! FrameV)
		renderCounter (8 + 286) 177 (levelCounter scoreState)
		renderCounter (54 + 286) 177 (scoreCounter scoreState)
		renderAnimation display 0 286 0 (gfx M.! SidePanel)

	-- render preview
	when showPreview$ do
		let
			previewBrick = Q.head brickQueue
			previewAni = gfx M.! (tile previewBrick)
			previewCoords = srsCoords previewBrick RUp
		forM_ previewCoords $ \(x, y) -> liftIO$
			renderAnimation display 0
				(325 + x * tileS) (230 + y * tileS) previewAni
	
	-- UI elements
	when (mode == IntroMode) $ liftIO$ do
		let
			w1 = surfaceGetWidth introMessage
			w2 = surfaceGetWidth introMessage2
		blitSurface introMessage Nothing
			display (Just$ Rect ((260 - w1) `div` 2 + 13) 96 0 0)
		blitSurface introMessage2 Nothing
			display (Just$ Rect ((260 - w2) `div` 2 + 13) 126 0 0)
		blitSurface introMessage3 Nothing
			display (Just$ Rect 16 216 0 0)
		renderHighScores display 16 260 254 font
			(Color 0 64 255) highScores

	when (mode == HighScoreMode) $ liftIO$ do
		blitSurface highScoreMessage Nothing
			display (Just$ Rect 16 216 0 0)
		renderHighScores display 16 260 254 font
			(Color 0 64 255) highScores
		

	when (mode == PausedMode) $ liftIO$
		renderAnimation display 0 26 193 (gfx M.! Paused)

	when (mode == GameOverMode) $ liftIO$
		renderAnimation display 0 43 241 (gfx M.! GameOverTile)

	liftIO$ Graphics.UI.SDL.flip display
	return ()
	where
		fieldTileFrame y = if y `elem` fullLines then 19 - lineFTA else 0

