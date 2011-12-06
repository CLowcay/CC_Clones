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

import Common.Graphics
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Array
import Graphics.UI.SDL
import qualified Data.Map as M
import Tetris.Assets
import Tetris.GameState

fieldX = 0 :: Int
fieldY = 0 :: Int

realX x = fieldX + (x * tileS)
realY y = fieldY + (y * tileS)

-- Render a frame
renderFrame :: GameState -> ReaderT Assets IO ()
renderFrame state@(GameState {..}) = do
	Assets {..} <- ask
	display <- liftIO getVideoSurface

	-- render field
	forM_ (assocs field) $ \((x, y), tm) -> liftIO$
		when (y < 22) $
			case tm of
				Nothing -> return ()
				Just tile -> renderAnimation display 0
					(realX x) (realY y) (gfx M.! tile)

	-- render brick
	let
		brickCoords =
			map (\(x, y) -> (x + currentPos, y + currentHeight - 2)) $
				srsCoords currentBrick currentRotation
		brickAni = gfx M.! (tile currentBrick)
	forM_ brickCoords $ \(x, y) -> liftIO$
		renderAnimation display 0
			(realX x) (realY y) brickAni

	when (mode == PausedMode) $ liftIO$
		renderAnimation display 0 123 160 (gfx M.! Paused)

	when (mode == GameOverMode) $ liftIO$
		renderAnimation display 0 140 208 (gfx M.! GameOverTile)

	liftIO$ Graphics.UI.SDL.flip display
	return ()

