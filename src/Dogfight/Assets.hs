module Dogfight.Assets (
	Assets(..), Message(..),
	loadAssets,
	bgColor, messageColor
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

bgColor = Pixel 0x323232
messageColor = Color 0 64 255

data Assets = Assets {
	gfx :: M.Map Tile Sprite,
	font :: Font
	getMessage :: Message -> Surface
}

data Message = MessageIntro1 | MessageIntro2 | MessageHighScores
	deriving Show

-- Load all assets
loadAssets :: IO Assets
loadAssets = do
	gfx <- loadSprites
	font <- loadFont

	messageData <- 
		mapM (\(m, s) ->
			surface <- renderUTF8Solid font s messageColor
			return surface
		) [
			(MessageIntro1, "Press F2 to start, Esc to quit"),
			(MessageIntro2, "High scores:"),
			(MessageHighScores, "New high score! Enter your name")
		]
	
	let
		messageMap MessageIntro1 = messageData !! 0
		messageMap MesageIntro2 = messageData !! 1
		messageMap MessageHighScores = messageData !! 2

	return Assets {
		gfx = gfx,
		font = font,
		messages = messageMap
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
		bg = makeBackground (spriteFor BoxTile)
			(spriteFor FrameH) (spriteFor FrameV)
		frameV = makeFrameV$ spriteFor BoxTile
		frameH = makeFrameH$ spriteFor BoxTile

		spriteFor Background = makeSprite bg (520, 546) (0, 0)
		spriteFor FrameV = makeSprite frameV (26, 546) (0, 0)
		spriteFor FrameH = makeSprite frameH (494, 26) (0, 0)
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

makeVFrame :: Sprite -> IO Surface
makeVFrame boxSprite = do
	surface <- createRGBSurface [HWSurface] 26 546 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000 >>= displayFormat

	forM_ (vBorderPos <$> [1..21])$ \pos -> do
		renderSprite surface 0 pos boxSprite
	
	return surface

	where vBorderPos n = (0, (n - 1) * 26)

makeHFrame :: Sprite -> IO Surface
makeHFrame boxSprite = do
	surface <- createRGBSurface [HWSurface] 494 26 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000 >>= displayFormat

	forM_ (hBorderPos <$> [1..19])$ \pos -> do
		renderSprite surface 0 pos boxSprite
	
	return surface

	where hBorderPos 0 n = ((n - 1) * 26, 0)

makeBackground :: Sprite -> Sprite -> Sprite -> IO Surface
makeBackground boxSprite hBorder vBorder = do
	surface <- createRGBSurface [HWSurface] 520 546 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000 >>= displayFormat

	fillRect surface Nothing bgColor
	renderSprite surface 0 (0, 0) vBorder
	renderSprite surface 0 (26, 0) hBorder
	renderSprite surface 0 (26, 520) hBorder

	forM_ (boxPos <$> [1..8] <*> [1..8]))$ \pos -> do
		renderSprite surface 0 pos boxSprite
	
	return surface

	where boxPos (n, m) = (n * 52, m * 52)

loadFont :: IO Font
loadFont = openFont
	(getAssetPath "fonts/titillium/TitilliumText22L004.otf") 28

