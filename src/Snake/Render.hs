module Snake.Render where

import Common.Counters
import Common.Graphics
import Control.Monad
import Data.Array
import Graphics.UI.SDL
import qualified Data.Map as Map
import Snake.GameState

-- Render a frame
renderFrame :: GameState -> IO ()
renderFrame state = do
	let gfx = gs_gfx state
	let eatingApples = gs_eatingApples state
	let frame = if not$null eatingApples
		then ((15 - (gs_framesToAlignment state) + 5) `mod` 16) else 4
	display <- getVideoSurface

	blitSurface (gs_wallStamp state) Nothing display (Just$ Rect 0 0 0 0)
	let (x, y, open) = gs_inDoor state in when (not open) $
		renderAnimation display 0 (x * 16) (y * 16)
			(gfx Map.! (gs_inDoorTile state))
	let (x, y, open) = gs_outDoor state in when (not open) $
		renderAnimation display 0 (x * 16) (y * 16)
			(gfx Map.! (gs_outDoorTile state))

	-- render the side panel
	renderCounter (23 + 480) 172 (gs_levelCounter state)
	renderCounter (84 + 480) 172 (gs_scoreCounter state)
	renderAnimation display 0 480 0 (gfx Map.! SidePanel)

	-- render food
	let foodCells = gs_foodCells state
	mapM_ (\(x, y) ->
			renderAnimation display 0 (x * 16) (y * 16)
				(gfx Map.! (foodCells Map.! (x, y)))
		) (Map.keys foodCells)
	mapM_ (\((x, y), tile) ->
			renderAnimation display 0 (x * 16) (y * 16) (gfx Map.! tile)
		) eatingApples

	renderSnake display frame state

	-- UI elements
	when (gs_paused state) $ do
		renderAnimation display 0 123 160 (gfx Map.! Paused)
	when (gs_gameOver state) $ do
		renderAnimation display 0 140 208 (gfx Map.! GameOverTile)
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
		headAni = gfx Map.! (getTile$head snakeTiles)
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
	mapM_ (\(((x, y), show), tile) -> when show $
		renderAnimation dst 0 x y (gfx Map.! tile)) bodyTiles

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
			when show $ (render (gfx Map.! tile) frame offset x y) >> return ()

		renderHead2 (((x, y), show), tile) headAni offset render =
			when show $ if not$ elem tile cornerTiles then do
					render headAni frame offset 16 x y
					return ()
				else do
					renderAnimation dst 0 x y (gfx Map.! tile)
					return ()

		renderHead3 (((x, y), show), tile) headAni offset render1 render2 =
			when show $ if not$ elem tile cornerTiles
				then do
					render2 headAni frame offset offset x y
					render1 (gfx Map.! tile) 0 offset x y
					return ()
				else do
					renderAnimation dst 0 x y (gfx Map.! tile)
					return ()

		renderTail snakeTile1 snakeTile2 offset render1 render2 = do
			let 
				(((x1, y1), show1), tile1) = snakeTile1
				(((x2, y2), show2), tile2) = snakeTile2
			when show1 $ do
				render1 (gfx Map.! tile1) 0 offset x1 y1
				return ()
			when show2 $ if not$ elem tile2 cornerTiles
				then do
					render2 (gfx Map.! tile1) 0 offset offset x2 y2
					render1 (gfx Map.! tile2) 0 offset x2 y2
					return ()
				else do
					renderAnimation dst 0 x2 y2 (gfx Map.! tile2)
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

