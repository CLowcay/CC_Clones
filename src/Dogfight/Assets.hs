module Dogfight.Assets (
	Assets(..),
	loadAssets
) where

import Common.AniTimer
import Common.Assets
import Common.Counters
import Common.Graphics
import Common.Util
import Control.Applicative
import Dogfight.Gamestate
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.TTF

data Assets = Assets {
	gfx :: M.Map Tile Sprite,
	font :: Font
}

-- Load all assets
loadAssets :: IO Assets
loadAssets = do
	gfx <- loadSprites
	font <- loadFont

	return Assets {
		gfx = gfx,
		font = font
	}

loadSprites :: IO (M.Map Tile Animation)
loadSprites = do
	sheet1 <- loadBMP$ getAssetPath "gfx/hideandseek.bmp"
	let sheet1Sprite = makeSprite sheet1 (16, 16)
	boxTile <- loadBMP$ getAssetPath "gfx/blueblock.bmp"
	panel <- loadBMP$ getAssetPath "gfx/spaceshipsPanel.bmp"
	paused <- loadBMP$ getAssetPath "gfx/Paused.bmp"
	gameOver <- loadBMP$ getAssetPath "gfx/gameOver.bmp"
	digits <- loadBMP$ getAssetPath "gfx/Digits.bmp"

	forM_ [paused, gameOver, panel, digits]$ \surface ->
		setColorKey surface [SrcColorKey] (Pixel 0x00FF00FF))

	let
		bg = makeBackground$ spriteFor BoxTile

		spriteFor Background = makeSprite bg (520, 546) (0, 0)
		spriteFor Digits = makeSprite digits (20, 180) (0, 0) 
		spriteFor Paused = makeSprite paused (234, 160) (0, 0) 
		spriteFor GameOverTile = makeSprite gameOver (200, 64) (0, 0) 
		spriteFor SidePanel = makeSprite panel (208, 546) (0, 0) 
		spriteFor BoxTile = makeSprite boxTile (26, 26) (0, 0)
		spriteFor PlayerR = sheet1Sprite (0, 0)
		spriteFor PlayerD = sheet1Sprite (26, 0)
		spriteFor PlayerL = sheet1Sprite (52, 0)
		spriteFor PlayerU = sheet1Sprite (78, 0)
		spriteFor AiR = sheet1Sprite (0, 26)
		spriteFor AiD = sheet1Sprite (26, 26)
		spriteFor AiL = sheet1Sprite (52, 26)
		spriteFor AiU = sheet1Sprite (78, 26)
		spriteFor EngineR = sheet1Sprite (0, 78)
		spriteFor EngineD = sheet1Sprite (26, 78)
		spriteFor EngineL = sheet1Sprite (52, 78)
		spriteFor EngineU = sheet1Sprite (78, 78)
		spriteFor LaserR = sheet1Sprite (0, 52)
		spriteFor LaserD = sheet1Sprite (26, 52)
		spriteFor LaserL = sheet1Sprite (52, 52)
		spriteFor LaserU = sheet1Sprite (78, 52)

	return$ M.fromList$ map (\tile ->
		(tile, spriteFor tile)) allTiles

makeBackground :: Sprite -> IO Surface
makeBackground boxSprite = do
	surface <- createRGBSurface [HWSurface] 520 546 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000 >>= displayFormat

	forM_ (vBorderPos 0 <$> [1..21])$ \pos -> do
		renderSprite surface 0 pos boxSprite
	forM_ (hBorderPos 0 <$> [1..19])$ \pos -> do
		renderSprite surface 0 pos boxSprite
	forM_ (hBorderPos 520 <$> [1..19])$ \pos -> do
		renderSprite surface 0 pos boxSprite
	forM_ (boxPos <$> [1..8] <*> [1..8]))$ \pos -> do
		renderSprite surface 0 pos boxSprite
	
	return surface

	where
		vBorderPos x n = (x, (n - 1) * 26)
		hBorderPos y n = (n * 26, y)
		boxPos (n, m) = (n * 52, m * 52)

loadFont :: IO Font
loadFont = openFont
	(getAssetPath "fonts/titillium/TitilliumText22L004.otf") 28

