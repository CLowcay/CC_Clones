{-# LANGUAGE RecursiveDo #-}

module Spaceships.Assets (
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
import Control.Monad
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.TTF
import qualified Data.Map as M
import Spaceships.GameState

bgColor = Pixel 0x323232
messageColor = Color 0 64 255

data Assets = Assets {
	gfx :: M.Map Tile Sprite,
	font :: Font,
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
		mapM (\(m, s) -> do
			surface <- renderUTF8Solid font s messageColor
			return surface
		) [
			(MessageIntro1, "Press F2 to start, Esc to quit"),
			(MessageIntro2, "High scores:"),
			(MessageHighScores, "New high score! Enter your name")
		]
	
	let
		messageMap MessageIntro1 = messageData !! 0
		messageMap MessageIntro2 = messageData !! 1
		messageMap MessageHighScores = messageData !! 2

	return Assets {
		gfx = gfx,
		font = font,
		getMessage = messageMap
	}

loadSprites :: IO (M.Map Tile Sprite)
loadSprites = do
	sheet1 <- loadBMP$ getAssetPath "gfx/hideandseek.bmp"
	let sheet1Sprite = makeSprite sheet1 (26, 26)
	boxTile <- loadBMP$ getAssetPath "gfx/blueblock.bmp"
	panel <- loadBMP$ getAssetPath "gfx/spaceshipsPanel.bmp"
	paused <- loadBMP$ getAssetPath "gfx/Paused.bmp"
	gameOver <- loadBMP$ getAssetPath "gfx/gameOver.bmp"
	digits <- loadBMP$ getAssetPath "gfx/Digits.bmp"

	forM_ [sheet1, paused, gameOver, panel, digits]$ \surface ->
		setColorKey surface [SrcColorKey] (Pixel 0x00FF00FF)

	let sbox = makeSprite boxTile (26, 26) (0, 0)
	frameV <- makeVFrame sbox
	frameH <- makeHFrame sbox
	let sFrameV = makeSprite frameV (26, 546) (0, 0)
	let sFrameH = makeSprite frameH (494, 26) (0, 0)

	bg <- makeBackground sbox sFrameH sFrameV

	let
		spriteFor Background = makeSprite bg (572, 546) (0, 0)
		spriteFor FrameV = sFrameV
		spriteFor FrameH = sFrameH
		spriteFor Digits = makeSprite digits (20, 180) (0, 0) 
		spriteFor Paused = makeSprite paused (234, 160) (0, 0) 
		spriteFor GameOverTile = makeSprite gameOver (200, 64) (0, 0) 
		spriteFor SidePanel = makeSprite panel (208, 546) (0, 0) 
		spriteFor BoxTile = makeSprite boxTile (26, 26) (0, 0)
		spriteFor PlayerR = sheet1Sprite (0, 0)
		spriteFor PlayerD = sheet1Sprite (1, 0)
		spriteFor PlayerL = sheet1Sprite (2, 0)
		spriteFor PlayerU = sheet1Sprite (3, 0)
		spriteFor AiR = sheet1Sprite (0, 1)
		spriteFor AiD = sheet1Sprite (1, 1)
		spriteFor AiL = sheet1Sprite (2, 1)
		spriteFor AiU = sheet1Sprite (3, 1)
		spriteFor EngineR = sheet1Sprite (0, 3)
		spriteFor EngineD = sheet1Sprite (1, 3)
		spriteFor EngineL = sheet1Sprite (2, 3)
		spriteFor EngineU = sheet1Sprite (3, 3)
		spriteFor LaserR = sheet1Sprite (0, 2)
		spriteFor LaserD = sheet1Sprite (1, 2)
		spriteFor LaserL = sheet1Sprite (2, 2)
		spriteFor LaserU = sheet1Sprite (3, 2)

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

	where hBorderPos n = ((n - 1) * 26, 0)

makeBackground :: Sprite -> Sprite -> Sprite -> IO Surface
makeBackground boxSprite hBorder vBorder = do
	surface <- createRGBSurface [HWSurface] 572 598 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000 >>= displayFormat

	fillRect surface Nothing bgColor
	renderSprite surface 0 (0, 0) vBorder
	renderSprite surface 0 (26, 0) hBorder
	renderSprite surface 0 (26, 520) hBorder

	forM_ (boxPos <$> [1..9] <*> [1..9])$ \pos -> do
		renderSprite surface 0 pos boxSprite
	
	return surface

	where boxPos n m = (n * 52, m * 52)

loadFont :: IO Font
loadFont = openFont
	(getAssetPath "fonts/titillium/TitilliumText22L004.otf") 28

