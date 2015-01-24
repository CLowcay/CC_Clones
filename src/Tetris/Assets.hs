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
import Graphics.UI.SDL.TTF
import qualified Data.Map as M
import Tetris.GameState

data Assets = Assets {
	gfx :: M.Map Tile Sprite,
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

loadSprites :: IO (M.Map Tile Sprite)
loadSprites = do
	paused <- loadBMP$ getAssetPath "gfx/Paused.bmp"
	gameOver <- loadBMP$ getAssetPath "gfx/gameOver.bmp"
	digits <- loadBMP$ getAssetPath "gfx/Digits.bmp"

	frameH <- loadBMP$ getAssetPath "gfx/FrameH.bmp"
	frameV <- loadBMP$ getAssetPath "gfx/FrameV.bmp"
	sidePanel <- loadBMP$ getAssetPath "gfx/TetrisPanel.bmp"
	
	redTile <- loadBMP$ getAssetPath "gfx/RedShrink.bmp"
	pinkTile <- loadBMP$ getAssetPath "gfx/PinkShrink.bmp"
	yellowTile <- loadBMP$ getAssetPath "gfx/YellowShrink.bmp"
	orangeTile <- loadBMP$ getAssetPath "gfx/OrangeShrink.bmp"
	blueTile <- loadBMP$ getAssetPath "gfx/BlueShrink.bmp"
	greyTile <- loadBMP$ getAssetPath "gfx/GreyShrink.bmp"
	greenTile <- loadBMP$ getAssetPath "gfx/GreenShrink.bmp"

	let
		redTileAni =  makeAnimationH redTile (tileS, tileS) 20
		pinkTileAni = makeAnimationH pinkTile (tileS, tileS) 20
		yellowTileAni = makeAnimationH yellowTile (tileS, tileS) 20
		orangeTileAni = makeAnimationH orangeTile (tileS, tileS) 20
		blueTileAni = makeAnimationH blueTile (tileS, tileS) 20
		greyTileAni = makeAnimationH greyTile (tileS, tileS) 20
		greenTileAni = makeAnimationH greenTile (tileS, tileS) 20

	mapM_ (\surface ->
			setColorKey surface [SrcColorKey] (Pixel 0x00FF00FF))
		[paused, gameOver, digits, sidePanel]

	let
		tileAnimation Digits = makeSprite digits (20, 180) (0, 0)
		tileAnimation Paused = makeSprite paused (234, 160) (0, 0)
		tileAnimation GameOverTile = makeSprite gameOver (200, 64) (0, 0)
		tileAnimation FrameH = makeSprite frameH (286, 13) (0, 0)
		tileAnimation FrameV = makeSprite frameV (13, 520) (0, 0)
		tileAnimation SidePanel = makeSprite sidePanel (195, 546) (0, 0)
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
	(getAssetPath "fonts/titillium/TitilliumText22L004.otf") 28

