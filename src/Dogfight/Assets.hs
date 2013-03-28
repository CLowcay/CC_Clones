module Dogfight.Assets (
	Assets(..), Message(..),
	loadAssets
) where

import Common.AniTimer
import Common.Assets
import Common.Counters
import Common.Graphics
import Common.Util
import Dogfight.Gamestate
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.TTF

data Assets = Assets {
	gfx :: M.Map Tile Animation,
	sfx :: M.Map Sfx Chunk,
	font :: Font,
}

-- Load all assets
loadAssets :: IO Assets
loadAssets = do
	gfx <- loadSprites
	sfx <- loadSounds
	font <- loadFont

	return Assets {
		gfx = gfx,
		sfx = sfx,
		font = font,
		messages = M.fromList messageData
	}

loadSounds :: IO (M.Map Sfx Chunk)
loadSounds = do
	return$ M.fromList []

loadSprites :: IO (M.Map Tile Animation)
loadSprites = do
	sheet1 <- loadBMP$ getAssetPath "gfx/Sheet1.bmp"
	let sheet1Animation = makeAnimation sheet1 16 16
	paused <- loadBMP$ getAssetPath "gfx/Paused.bmp"
	gameOver <- loadBMP$ getAssetPath "gfx/gameOver.bmp"
	sidePanel <- loadBMP$ getAssetPath "gfx/SidePanel.bmp"
	digits <- loadBMP$ getAssetPath "gfx/Digits.bmp"
	headDown <- loadBMP$ getAssetPath "gfx/HeadDown.bmp"
	headLeft <- loadBMP$ getAssetPath "gfx/HeadLeft.bmp"
	headRight <- loadBMP$ getAssetPath "gfx/HeadRight.bmp"
	headUp <- loadBMP$ getAssetPath "gfx/HeadUp.bmp"

	mapM_ (\surface ->
			setColorKey surface [SrcColorKey] (Pixel 0x00FF00FF))
		[sheet1, paused, gameOver, sidePanel, digits,
			headDown, headLeft, headRight, headUp]

	let
		tileAnimation Digits = makeAnimation digits 20 180 0 0 
		tileAnimation Paused = makeAnimation paused 234 160 0 0 
		tileAnimation GameOverTile = makeAnimation gameOver 200 64 0 0 
		tileAnimation SidePanel = makeAnimation sidePanel 200 480 0 0 
		tileAnimation HeadDown = Animation {
		 	surface = headDown,
		 	frames = listArray (0, 15) (map (\n ->
				Rect (n * 16) 0 16 16) [0..15])}
		tileAnimation HeadLeft = Animation {
		 	surface = headLeft,
		 	frames = listArray (0, 15) (map (\n ->
				Rect 0 (n * 16) 16 16) [0..15])}
		tileAnimation HeadRight = Animation {
		 	surface = headRight,
		 	frames = listArray (0, 15) (map (\n ->
				Rect 0 (n * 16) 16 16) [0..15])}
		tileAnimation HeadUp = Animation {
		 	surface = headUp,
		 	frames = listArray (0, 15) (map (\n ->
				Rect (n * 16) 0 16 16)[0..15])}
		tileAnimation SnakeV = sheet1Animation 0 0
		tileAnimation SnakeH = sheet1Animation 1 0
		tileAnimation SnakeUL = sheet1Animation 2 0
		tileAnimation SnakeUR = sheet1Animation 3 0
		tileAnimation SnakeDR = sheet1Animation 3 1
		tileAnimation SnakeDL = sheet1Animation 2 1
		tileAnimation SnakeTHL = sheet1Animation 0 1
		tileAnimation SnakeTHR = sheet1Animation 1 1
		tileAnimation SnakeTVU = sheet1Animation 4 0
		tileAnimation SnakeTVD = sheet1Animation 4 1
		tileAnimation AppleA = sheet1Animation 5 0
		tileAnimation AppleB = sheet1Animation 5 1
		tileAnimation WallV = sheet1Animation 0 2
		tileAnimation WallH = sheet1Animation 1 2
		tileAnimation WallUL = sheet1Animation 2 2
		tileAnimation WallUR = sheet1Animation 3 2
		tileAnimation WallDR = sheet1Animation 3 3
		tileAnimation WallDL = sheet1Animation 2 3
		tileAnimation WallTVU = sheet1Animation 4 2
		tileAnimation WallTVD = sheet1Animation 4 3
		tileAnimation WallTHL = sheet1Animation 0 3
		tileAnimation WallTHR = sheet1Animation 1 3
		tileAnimation WallDot = sheet1Animation 5 2
		tileAnimation WallXR = sheet1Animation 0 4
		tileAnimation WallXU = sheet1Animation 1 4
		tileAnimation WallXD = sheet1Animation 2 4
		tileAnimation WallX = sheet1Animation 3 4
		tileAnimation WallXL = sheet1Animation 4 4
		tileAnimation DoorInV = sheet1Animation 5 3
		tileAnimation DoorOutV = sheet1Animation 5 3
		tileAnimation DoorInH = sheet1Animation 5 4
		tileAnimation DoorOutH = sheet1Animation 5 4
	return$ M.fromList$ map (\tile ->
		(tile, tileAnimation tile)) allTiles

loadFont :: IO Font
loadFont = openFont
	(getAssetPath "fonts/TitilliumText22L004.otf") 28

