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

module Tetris.Assets (
	Assets(..), Message(..), loadAssets
) where

import Common.Assets
import Common.Graphics
import Control.Monad
import Data.Array
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.Rotozoomer
import Graphics.UI.SDL.TTF
import qualified Data.Map as M
import Tetris.GameState

data Assets = Assets {
	gfx :: M.Map Tile Animation,
	sfx :: M.Map Sfx Chunk,
	font :: Font,
	messages :: M.Map Message Surface
}

data Message = MessageIntro1 | MessageIntro2 | MessageIntro3 |
	MessageHighScores1 | MessageHighScores2
	deriving (Enum, Bounded, Eq, Ord, Show)

-- Load all assets
loadAssets :: IO Assets
loadAssets = do
	gfx <- loadSprites
	sfx <- loadSounds
	font <- loadFont
	messageData <-
		mapM (\(m, s) -> do
			surface <- renderUTF8Solid font s (Color 0xCC 0xCC 0xCC)
			return (m, surface)
		) [
			(MessageIntro1, "Press F2 to start,"),
			(MessageIntro2, "ESC to quit"),
			(MessageIntro3, "High scores:"),
			(MessageHighScores1, "New high score!"),
			(MessageHighScores2, "Enter name:")
		]
	return Assets {
		gfx = gfx,
		sfx = sfx,
		font = font,
		messages = M.fromList messageData 
	}

loadSounds :: IO (M.Map Sfx Chunk)
loadSounds = do
	turn <- loadWAV$ getAssetPath "sfx/SBlip.wav"
	line <- loadWAV$ getAssetPath "sfx/LBlip.wav"
	return$ M.fromList [(SfxTurn, turn), (SfxLine, line)]

loadSprites :: IO (M.Map Tile Animation)
loadSprites = do
	paused <- loadBMP$ getAssetPath "gfx/Paused.bmp"
	gameOver <- loadBMP$ getAssetPath "gfx/gameOver.bmp"
	digits <- loadBMP$ getAssetPath "gfx/Digits.bmp"

	frameH <- loadBMP$ getAssetPath "gfx/FrameH.bmp"
	frameV <- loadBMP$ getAssetPath "gfx/FrameV.bmp"
	sidePanel <- loadBMP$ getAssetPath "gfx/TetrisPanel.bmp"
	
	redTile <- loadBMP$ getAssetPath "gfx/RedTile.bmp"
	pinkTile <- loadBMP$ getAssetPath "gfx/PinkTile.bmp"
	yellowTile <- loadBMP$ getAssetPath "gfx/YellowTile.bmp"
	orangeTile <- loadBMP$ getAssetPath "gfx/OrangeTile.bmp"
	blueTile <- loadBMP$ getAssetPath "gfx/BlueTile.bmp"
	greyTile <- loadBMP$ getAssetPath "gfx/GreyTile.bmp"
	greenTile <- loadBMP$ getAssetPath "gfx/GreenTile.bmp"

	redTileAni <-  makeShrinkingAnimation redTile tileS tileS 20
	pinkTileAni <- makeShrinkingAnimation pinkTile tileS tileS 20
	yellowTileAni <- makeShrinkingAnimation yellowTile tileS tileS 20
	orangeTileAni <- makeShrinkingAnimation orangeTile tileS tileS 20
	blueTileAni <- makeShrinkingAnimation blueTile tileS tileS 20
	greyTileAni <- makeShrinkingAnimation greyTile tileS tileS 20
	greenTileAni <- makeShrinkingAnimation greenTile tileS tileS 20

	mapM_ (\surface ->
			setColorKey surface [SrcColorKey] (Pixel 0x00FF00FF))
		[paused, gameOver, digits, sidePanel]

	let
		tileAnimation Digits = makeAnimation digits 20 180 0 0
		tileAnimation Paused = makeAnimation paused 234 160 0 0
		tileAnimation GameOverTile = makeAnimation gameOver 200 64 0 0
		tileAnimation FrameH = makeAnimation frameH 286 13 0 0
		tileAnimation FrameV = makeAnimation frameV 13 520 0 0
		tileAnimation SidePanel = makeAnimation sidePanel 195 546 0 0
		tileAnimation RedTile = redTileAni
		tileAnimation PinkTile = pinkTileAni
		tileAnimation YellowTile = yellowTileAni
		tileAnimation OrangeTile = orangeTileAni
		tileAnimation BlueTile = blueTileAni
		tileAnimation GreyTile = greyTileAni
		tileAnimation GreenTile = greenTileAni

	return$ M.fromList$ map (\tile ->
		(tile, tileAnimation tile)) allTiles

loadFont :: IO Font
loadFont = openFont
	(getAssetPath "fonts/TitilliumText22L004.otf") 28

makeAnimation :: Surface -> Int -> Int -> Int -> Int -> Animation
makeAnimation surface w h x y =
	Animation {
		surface = surface,
		frames = listArray (0, 0) [Rect (x * w) (y * h) w h]
	}

makeShrinkingAnimation :: Surface -> Int -> Int -> Int -> (IO Animation)
makeShrinkingAnimation surface w h frames = do
	let factors = map
		(\x -> 1.0 - ((jimmyTypes x)/(jimmyTypes (frames - 1))))
		[1..(frames - 1)]
	let offsets = 0:map
		(\factor -> round$ ((jimmyTypes w) *  (1.0 - factor)) / 2.0) factors
	genFrames <- mapM (\factor -> zoom surface factor factor True) factors

	allFrames <- createRGBSurface [HWSurface] (frames * w) h 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000 >>= displayFormat

	blitSurface surface (Just$Rect 0 0 w h) allFrames (Just$Rect 0 0 w h)
	forM_ (zip3 genFrames [1..] offsets) $ \(genFrame, i, c) ->
		blitSurface genFrame (Just$Rect 0 0 w h)
			allFrames (Just$Rect (w * i + c) c w h)
	
	return$ Animation {
		surface = allFrames,
		frames = listArray (0, (frames - 1))
			[Rect (i * w) 0  w h | i <- [0..(frames - 1)]]
	}
	where
		jimmyTypes = fromInteger.toInteger

