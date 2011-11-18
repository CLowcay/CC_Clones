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

module Snake.Assets (
	Assets(..),
	loadAssets,
	Snake.Assets.loadLevel
) where

import Common.AniTimer
import Common.Assets
import Common.Counters
import Common.Graphics
import Common.Util
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Array
import Data.List
import Data.Maybe
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.TTF
import qualified Common.Queue as Q
import qualified Data.Map as M
import qualified Data.Set as S
import Snake.GameState

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
	chomp <- loadWAV$ getAssetPath "sfx/chomp.wav"
	bump <- loadWAV$ getAssetPath "sfx/bump.wav"
	return$ M.fromList [(Chomp, chomp), (Bump, bump)]

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

loadLevel :: Int -> GameState -> ReaderT Assets IO GameState
loadLevel level (state@(GameState {wallStamp})) = do
	-- load the level file
	fileData <- fmap lines$
		liftIO.readFile$ getAssetPath$ "levels/snake" ++ show level
	let startDirection = case trim (head fileData) of
		"D" -> DDown
		"U" -> DUp
		"L" -> DLeft
		"R" -> DRight
		_ -> error$ "Invalid level file for level " ++ show level
	let levelMap = concatMap (\(y, line) ->
		concatMap (\(x, c) -> case c of
			'1' -> [((x, y), WallDL)]
			'2' -> [((x, y), WallXU)]
			'3' -> [((x, y), WallDR)]
			'4' -> [((x, y), WallXR)]
			'5' -> [((x, y), WallX)]
			'6' -> [((x, y), WallXL)]
			'7' -> [((x, y), WallUL)]
			'8' -> [((x, y), WallXD)]
			'9' -> [((x, y), WallUR)]
			'-' -> [((x, y), WallH)]
			'/' -> [((x, y), WallV)]
			'>' -> [((x, y), WallTHR)]
			'<' -> [((x, y), WallTHL)]
			'_' -> [((x, y), WallTVU)]
			'~' -> [((x, y), WallTVD)]
			'.' -> [((x, y), WallDot)]
			'a' -> [((x, y), AppleA)]
			'b' -> [((x, y), AppleB)]
			'i' -> [((x, y), DoorInH)]
			'I' -> [((x, y), DoorInV)]
			'e' -> [((x, y), DoorOutH)]
			'E' -> [((x, y), DoorOutV)]
			' ' -> []
			_ -> error$ "Invalid level file for level " ++ show level
		)$ zip [0..] (oddElems line))$ zip [0..] (tail fileData)
	
	-- prepare the wallStamp
	Assets {gfx} <- ask
	let
		noRenderSprites = [DoorInH, DoorInV, DoorOutH, DoorOutV, AppleA, AppleB]
		toRender = filter (\(_, tile) -> not$tile `elem` noRenderSprites) levelMap
	liftIO$fillRect wallStamp (Just$ Rect 0 0 480 480) (Pixel 0x00000000)

	liftIO$ forM_ toRender $ \((x, y), tile) ->
		renderAnimation wallStamp 0 (x * 16) (y * 16) (gfx M.! tile)
	
	-- Prepare the doors
	let isInDoorTile = (\(_, tile) -> tile `elem` [DoorInH, DoorInV])
	let isOutDoorTile = (\(_, tile) -> tile `elem` [DoorOutH, DoorOutV])
	let inDoor = fst$ fromJust$ find isInDoorTile levelMap
	let outDoor = fst$ fromJust$ find isOutDoorTile levelMap
	let snakeCells = map (\((dx, dy), visible) ->
		((fst inDoor + dx, snd inDoor + dy), visible)) $
			case startDirection of
				DUp -> [((0, i), i == 0) | i <- [0..(4 + (2 * level))]]
				DDown -> [((0, -i), i == 0) | i <- [0..(4 + (2 * level))]]
				DLeft -> [((i, 0), i == 0) | i <- [0..(4 + (2 * level))]]
				DRight -> [((-i, 0), i == 0) | i <- [0..(4 + (2 * level))]]
	
	-- Initialise the state
	return$ state {
		fastMode = False,
		nextDirections = Q.empty, currentDirection = startDirection,
		aniTimer = resetTimer,
		framesToAlignment = 15,
		holdCount = 0,
		snakeCells = snakeCells,
		foodCells = M.fromList$ concatMap
			(\((x, y), tile) -> [((x, y), tile) | tile `elem` [AppleA, AppleB]])
			levelMap,
		wallCells = S.fromList$ concatMap
			(\(cell, tile) -> [cell | tile `elem`
				[WallV, WallH, WallUL, WallUR, WallDR, WallDL,
				WallTVU, WallTVD, WallTHL, WallTHR, WallDot,
				WallXR, WallXL, WallXU, WallXD, WallX]]
			) levelMap,
		inDoor = (fst inDoor, snd inDoor, True),
		inDoorTile = snd$ fromJust$ find isInDoorTile levelMap,
		outDoor = (fst outDoor, snd outDoor, False),
		outDoorTile = snd$ fromJust$ find isOutDoorTile levelMap,
		Snake.GameState.loadLevel = False,
		level = level,
		levelCounter = setCounter level (levelCounter state),
		eatingApples = []
	}

makeAnimation :: Surface -> Int -> Int -> Int -> Int -> Animation
makeAnimation surface w h x y =
	Animation {
		surface = surface,
		frames = listArray (0, 0) [Rect (x * w) (y * h) w h]
	}

