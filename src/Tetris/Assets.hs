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
	Assets(..), loadAssets
) where

import Common.Assets
import Common.Graphics
import Data.Array
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.TTF
import qualified Data.Map as M
import Tetris.GameState

data Assets = Assets {
	gfx :: M.Map Tile Animation,
	sfx :: M.Map Sfx Chunk,
	font :: Font
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
		font = font
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
	
	redTile <- loadBMP$ getAssetPath "gfx/RedTile.bmp"
	pinkTile <- loadBMP$ getAssetPath "gfx/PinkTile.bmp"
	yellowTile <- loadBMP$ getAssetPath "gfx/YellowTile.bmp"
	orangeTile <- loadBMP$ getAssetPath "gfx/OrangeTile.bmp"
	blueTile <- loadBMP$ getAssetPath "gfx/BlueTile.bmp"
	greyTile <- loadBMP$ getAssetPath "gfx/GreyTile.bmp"
	greenTile <- loadBMP$ getAssetPath "gfx/GreenTile.bmp"

	mapM_ (\surface ->
			setColorKey surface [SrcColorKey] (Pixel 0x00FF00FF))
		[paused, gameOver, digits]

	let
		tileAnimation Digits = makeAnimation digits 20 180 0 0
		tileAnimation Paused = makeAnimation paused 234 160 0 0
		tileAnimation GameOverTile = makeAnimation gameOver 200 64 0 0
		tileAnimation RedTile = makeAnimation redTile tileS tileS 0 0
		tileAnimation PinkTile = makeAnimation pinkTile tileS tileS 0 0
		tileAnimation YellowTile = makeAnimation yellowTile tileS tileS 0 0
		tileAnimation OrangeTile = makeAnimation orangeTile tileS tileS 0 0
		tileAnimation BlueTile = makeAnimation blueTile tileS tileS 0 0
		tileAnimation GreyTile = makeAnimation greyTile tileS tileS 0 0
		tileAnimation GreenTile = makeAnimation greenTile tileS tileS 0 0

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

