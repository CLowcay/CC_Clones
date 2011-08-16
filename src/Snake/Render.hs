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
along with this program.  If not, see <http://www.gnu.org/licenses/>.module Main where
-}

module Snake.Render where

import Common.Counters
import Common.Graphics
import Common.HighScores
import Control.Monad
import Data.Array
import Graphics.UI.SDL
import qualified Data.Map as M
import Snake.GameState

-- Render a frame
renderFrame :: GameState -> IO ()
renderFrame state = do
	let
		mode = gs_mode state
		gfx = gs_gfx state
		eatingApples = gs_eatingApples state
		frame = if not$null eatingApples
			then ((15 - (gs_framesToAlignment state) + 5) `mod` 16) else 4
	display <- getVideoSurface

	blitSurface (gs_wallStamp state) Nothing display (Just$ Rect 0 0 0 0)
	let (x, y, open) = gs_inDoor state in when (not open) $
		renderAnimation display 0 (x * 16) (y * 16)
			(gfx M.! (gs_inDoorTile state))
	let (x, y, open) = gs_outDoor state in when (not open) $
		renderAnimation display 0 (x * 16) (y * 16)
			(gfx M.! (gs_outDoorTile state))

	-- render the side panel
	renderCounter (23 + 480) 172 (gs_levelCounter state)
	renderCounter (84 + 480) 172 (gs_scoreCounter state)
	renderAnimation display 0 480 0 (gfx M.! SidePanel)

	-- render food
	let foodCells = gs_foodCells state
	forM_ (M.keys foodCells) (\(x, y) ->
			renderAnimation display 0 (x * 16) (y * 16)
				(gfx M.! (foodCells M.! (x, y)))
		)
	forM_ eatingApples (\((x, y), tile) ->
			renderAnimation display 0 (x * 16) (y * 16) (gfx M.! tile)
		) 

	-- The snake
	when (mode == InGameMode || mode == GameOverMode ||
		mode == PausedMode || mode == HighScoreMode) $ do
			renderSnake display frame state

	-- UI elements
	when (mode == IntroMode) $ do
		let
			introMessage = gs_introMessage state
			w1 = surfaceGetWidth introMessage
			introMessage2 = gs_introMessage2 state
		blitSurface introMessage Nothing
			display (Just$ Rect ((480 - w1) `div` 2) 128 0 0)
		blitSurface introMessage2 Nothing
			display (Just$ Rect 32 212 0 0)
		renderHighScores display 32 260 416 (gs_font state)
			(Color 0 64 255) (gs_highScores state)
	
	when (mode == HighScoreMode) $ do
		blitSurface (gs_highScoreMessage state) Nothing
			display (Just$ Rect 32 196 0 0)
		renderHighScores display 32 260 416 (gs_font state)
			(Color 0 64 255) (gs_highScores state)
		return ()

	when (mode == PausedMode) $ do
		renderAnimation display 0 123 160 (gfx M.! Paused)

	when (mode == GameOverMode) $ do
		renderAnimation display 0 140 208 (gfx M.! GameOverTile)

	Graphics.UI.SDL.flip display
	return ()

-- Render the snake
renderSnake :: Surface -> Int -> GameState -> IO ()
renderSnake dst frame state = do
	let
		snakeCells = gs_snakeCells state
		snakeTiles = zip
			(map (\((x, y), show) -> ((x * 16, y * 16), show)) snakeCells)
			(inferSnakeTiles (map fst snakeCells))
		getTile = snd
		-- offsets for rendering parts of the head
		offset = gs_framesToAlignment state
		offsetTail = if (gs_holdCount state > 0) then 0 else 15 - offset
		offset2 = offset + 3
		offset3 = offset2 - 16
		nHeadTiles = if offset3 < 0 ||
			elem (getTile$head$tail snakeTiles) cornerTiles then 2 else 3
		headAni = gfx M.! (getTile$head snakeTiles)
		bodyTiles = drop 2 (reverse (drop nHeadTiles snakeTiles))
		tailTiles = take 2 (reverse snakeTiles)

	-- render head
	let (render1, renderT1, render2) = case head snakeTiles of
		(_, HeadLeft) -> (renderLeft1, renderLeft1, renderLeft2)
		(_, HeadRight) -> (renderRight1, renderRightT1, renderRight2)
		(_, HeadUp) -> (renderUp1, renderUp1, renderUp2)
		(_, HeadDown) -> (renderDown1, renderDownT1, renderDown2)
	if nHeadTiles == 2
		then do
			renderHead1 (head snakeTiles) offset render1
			renderHead3 (head$ tail snakeTiles) headAni offset2 renderT1 render2
		else do
			renderHead1 (head snakeTiles) offset render1
			renderHead2 (head$ tail snakeTiles) headAni offset2 render2
			renderHead3 (head$ drop 2 snakeTiles) headAni offset3 renderT1 render2

	-- render body
	forM_ bodyTiles (\(((x, y), show), tile) ->
			when show $ renderAnimation dst 0 x y (gfx M.! tile)
		)

	-- render tail
	case tailTiles of
		[tile1@(_, SnakeTHL), tile2] ->
			renderTail tile1 tile2 offsetTail (renderLeft1) (renderLeftT2)
		[tile1@(_, SnakeTHR), tile2] ->
			renderTail tile1 tile2 offsetTail (renderRightT1) (renderRight2)
		[tile1@(_, SnakeTVU), tile2] ->
			renderTail tile1 tile2 offsetTail (renderUp1) (renderUpT2)
		[tile1@(_, SnakeTVD), tile2] ->
			renderTail tile1 tile2 offsetTail (renderDownT1) (renderDown2)

	return ()
	where
		gfx = gs_gfx state
		cornerTiles = [SnakeUL, SnakeDL, SnakeUR, SnakeDR]

		renderHead1 (((x, y), show), tile) offset render =
			when show $ (render (gfx M.! tile) frame offset x y) >> return ()

		renderHead2 (((x, y), show), tile) headAni offset render =
			when show $ if not$ elem tile cornerTiles then do
					render headAni frame offset 16 x y
					return ()
				else do
					renderAnimation dst 0 x y (gfx M.! tile)
					return ()

		renderHead3 (((x, y), show), tile) headAni offset render1 render2 =
			when show $ if not$ elem tile cornerTiles
				then do
					render2 headAni frame offset offset x y
					render1 (gfx M.! tile) 0 offset x y
					return ()
				else do
					renderAnimation dst 0 x y (gfx M.! tile)
					return ()

		renderTail snakeTile1 snakeTile2 offset render1 render2 = do
			let 
				(((x1, y1), show1), tile1) = snakeTile1
				(((x2, y2), show2), tile2) = snakeTile2
			when show1 $ do
				render1 (gfx M.! tile1) 0 offset x1 y1
				return ()
			when show2 $ if not$ elem tile2 cornerTiles
				then do
					render2 (gfx M.! tile1) 0 offset offset x2 y2
					render1 (gfx M.! tile2) 0 offset x2 y2
					return ()
				else do
					renderAnimation dst 0 x2 y2 (gfx M.! tile2)
					return ()

		renderLeft1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 0 (16 - offset) 16)
				dst (Just$ Rect (x + offset) y 0 0)
		renderLeft2 src frame offset w x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect (19 - offset) 0 w 16)
				dst (Just$ Rect x y 0 0)
		renderLeftT2 src frame offset w x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect (16 - offset) 0 w 16)
				dst (Just$ Rect x y 0 0)
		renderRight1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect (offset + 3) 0 (16 - offset) 16)
				dst (Just$ Rect x y 0 0)
		renderRightT1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect offset 0 (16 - offset) 16)
				dst (Just$ Rect x y 0 0)
		renderRight2 src frame offset w x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect (offset - w) 0 w 16)
				dst (Just$ Rect (x + 16 - w) y 0 0)
		renderUp1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 0 16 (16 - offset))
				dst (Just$ Rect x (y + offset) 0 0)
		renderUp2 src frame offset h x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 (19 - offset) 16 h)
				dst (Just$ Rect x y 0 0)
		renderUpT2 src frame offset h x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 (16 - offset) 16 h)
				dst (Just$ Rect x y 0 0)
		renderDown1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 (offset + 3) 16 (16 - offset))
				dst (Just$ Rect x y 0 0)
		renderDownT1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 offset 16 (16 - offset))
				dst (Just$ Rect x y 0 0)
		renderDown2 src frame offset h x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 (offset - h) 16 h)
				dst (Just$ Rect x (y + 16 - h) 0 0)
		adjRect src frame (Rect x y w h) =
			let (Rect x0 y0 _ _) = ((frames src) ! frame) in
				Rect (x0 + x) (y0 + y) w h

