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

module Snake.Render where

import Common.Counters
import Common.Graphics
import Common.HighScores
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Array
import Graphics.UI.SDL
import qualified Data.Map as M
import Snake.Assets
import Snake.GameState

-- Render a frame
renderFrame :: GameState -> ReaderT Assets IO ()
renderFrame state@(GameState {..}) = do
	Assets {..} <- ask

	let frame = if not$null eatingApples
		then (15 - framesToAlignment + 5) `mod` 16 else 4
	display <- liftIO getVideoSurface

	-- render the doors
	liftIO$ do
		blitSurface wallStamp Nothing display (Just$ Rect 0 0 0 0)
		let (x, y, open) = inDoor in unless open $
			renderSprite display 0 (x * 16, y * 16)
				(gfx M.! inDoorTile)
		let (x, y, open) = outDoor in unless open $
			renderSprite display 0 (x * 16, y * 16)
				(gfx M.! outDoorTile)

	-- render the side panel
	liftIO$ do
		renderCounter (23 + 480, 172) levelCounter
		renderCounter (84 + 480, 172) scoreCounter
		renderSprite display 0 (480, 0) (gfx M.! SidePanel)

	-- render food
	liftIO$ do
		forM_ (M.keys foodCells) $ \(x, y) ->
			renderSprite display 0 (x * 16, y * 16)
				(gfx M.! (foodCells M.! (x, y)))
		forM_ eatingApples $ \((x, y), tile) ->
			renderSprite display 0 (x * 16, y * 16) (gfx M.! tile)

	-- The snake
	when (mode `elem` [InGameMode, GameOverMode, PausedMode, HighScoreMode]) $
		renderSnake display frame state

	-- UI elements
	when (mode == IntroMode) $ liftIO$ do
		let
			w1 = surfaceGetWidth (messages M.! MessageIntro1)
		blitSurface (messages M.! MessageIntro1) Nothing
			display (Just$ Rect ((480 - w1) `div` 2) 128 0 0)
		blitSurface (messages M.! MessageIntro2) Nothing
			display (Just$ Rect 32 212 0 0)
		renderHighScores display (32, 260) 416 font
			(Color 0 64 255) highScores
	
	when (mode == HighScoreMode) $ liftIO$ do
		blitSurface (messages M.! MessageHighScores) Nothing
			display (Just$ Rect 32 196 0 0)
		renderHighScores display (32, 260) 416 font
			(Color 0 64 255) highScores
		return ()

	when (mode == PausedMode) $ liftIO$
		renderSprite display 0 (123, 160) (gfx M.! Paused)

	when (mode == GameOverMode) $ liftIO$
		renderSprite display 0 (140, 208) (gfx M.! GameOverTile)

	liftIO$ Graphics.UI.SDL.flip display
	return ()

-- Render the snake
renderSnake :: Surface -> Int -> GameState -> ReaderT Assets IO ()
renderSnake dst frame (state@(GameState {snakeCells})) = do
	Assets {..} <- ask

	let
		snakeTiles = zip
			(map (\((x, y), show) -> ((x * 16, y * 16), show)) snakeCells)
			(inferSnakeTiles (map fst snakeCells))
		getTile = snd
		-- offsets for rendering parts of the head
		offset = framesToAlignment state
		offsetTail = if holdCount state > 0 then 0 else 15 - offset
		offset2 = offset + 3
		offset3 = offset2 - 16
		nHeadTiles = if offset3 < 0 ||
			elem (getTile$head$tail snakeTiles) cornerTiles then 2 else 3
		headAni = gfx M.! getTile (head snakeTiles)
		bodyTiles = drop 2 (reverse (drop nHeadTiles snakeTiles))
		tailTiles = take 2 (reverse snakeTiles)

	-- render head
	let (render1, renderT1, render2) = case head snakeTiles of
		(_, HeadLeft) -> (renderLeft1, renderLeft1, renderLeft2)
		(_, HeadRight) -> (renderRight1, renderRightT1, renderRight2)
		(_, HeadUp) -> (renderUp1, renderUp1, renderUp2)
		(_, HeadDown) -> (renderDown1, renderDownT1, renderDown2)
	if nHeadTiles == 2
		then liftIO$ do
			renderHead1 (head snakeTiles) offset render1 gfx
			renderHead3 (head$ tail snakeTiles) headAni offset2 renderT1 render2 gfx
		else liftIO$ do
			renderHead1 (snakeTiles !! 0) offset render1 gfx
			renderHead2 (snakeTiles !! 1) headAni offset2 render2 gfx
			renderHead3 (snakeTiles !! 2) headAni offset3 renderT1 render2 gfx

	-- render body
	liftIO$ forM_ bodyTiles $ \(((x, y), show), tile) ->
		when show $ renderSprite dst 0 (x, y) (gfx M.! tile)

	-- render tail
	liftIO$ case tailTiles of
		[tile1@(_, SnakeTHL), tile2] ->
			renderTail tile1 tile2 offsetTail renderLeft1 renderLeftT2 gfx
		[tile1@(_, SnakeTHR), tile2] ->
			renderTail tile1 tile2 offsetTail renderRightT1 renderRight2 gfx
		[tile1@(_, SnakeTVU), tile2] ->
			renderTail tile1 tile2 offsetTail renderUp1 renderUpT2 gfx
		[tile1@(_, SnakeTVD), tile2] ->
			renderTail tile1 tile2 offsetTail renderDownT1 renderDown2 gfx

	return ()
	where
		cornerTiles = [SnakeUL, SnakeDL, SnakeUR, SnakeDR]

		renderHead1 (((x, y), show), tile) offset render gfx =
			when show $ render (gfx M.! tile) frame offset (x, y) >> return ()

		renderHead2 (((x, y), show), tile) headAni offset render gfx =
			when show $ if tile `notElem` cornerTiles then do
					render headAni frame offset 16 (x, y)
					return ()
				else do
					renderSprite dst 0 (x, y) (gfx M.! tile)
					return ()

		renderHead3 (((x, y), show), tile) headAni offset render1 render2 gfx =
			when show $ if tile `notElem` cornerTiles
				then do
					render2 headAni frame offset offset (x, y)
					render1 (gfx M.! tile) 0 offset (x, y)
					return ()
				else do
					renderSprite dst 0 (x, y) (gfx M.! tile)
					return ()

		renderTail snakeTile1 snakeTile2 offset render1 render2 gfx = do
			let 
				(((x1, y1), show1), tile1) = snakeTile1
				(((x2, y2), show2), tile2) = snakeTile2
			when show1 $ do
				render1 (gfx M.! tile1) 0 offset (x1, y1)
				return ()
			when show2 $ if tile2 `notElem` cornerTiles
				then do
					render2 (gfx M.! tile1) 0 offset offset (x2, y2)
					render1 (gfx M.! tile2) 0 offset (x2, y2)
					return ()
				else do
					renderSprite dst 0 (x2, y2) (gfx M.! tile2)
					return ()

		renderLeft1 sprite frame offset (x, y) =
			renderSpritePart dst sprite frame (x + offset, y)$ Rect 0 0 (16 - offset) 16

		renderLeft2 sprite frame offset w pos =
			renderSpritePart dst sprite frame pos$ Rect (19 - offset) 0 w 16

		renderLeftT2 sprite frame offset w pos =
			renderSpritePart dst sprite frame pos$ Rect (16 - offset) 0 w 16

		renderRight1 sprite frame offset pos =
			renderSpritePart dst sprite frame pos$ Rect (offset + 3) 0 (16 - offset) 16

		renderRightT1 sprite frame offset pos =
			renderSpritePart dst sprite frame pos$ Rect offset 0 (16 - offset) 16

		renderRight2 sprite frame offset w (x, y) =
			renderSpritePart dst sprite frame (x + 16 - w, y)$ Rect (offset - w) 0 w 16

		renderUp1 sprite frame offset (x, y) =
			renderSpritePart dst sprite frame (x, y + offset)$ Rect 0 0 16 (16 - offset)

		renderUp2 sprite frame offset h pos =
			renderSpritePart dst sprite frame pos$ Rect 0 (19 - offset) 16 h

		renderUpT2 sprite frame offset h pos =
			renderSpritePart dst sprite frame pos$ Rect 0 (16 - offset) 16 h

		renderDown1 sprite frame offset pos =
			renderSpritePart dst sprite frame pos$ Rect 0 (offset + 3) 16 (16 - offset)

		renderDownT1 sprite frame offset pos =
			renderSpritePart dst sprite frame pos$ Rect 0 offset 16 (16 - offset)

		renderDown2 sprite frame offset h (x, y) =
			renderSpritePart dst sprite frame (x, y + 16 - h)$ Rect 0 (offset - h) 16 h

