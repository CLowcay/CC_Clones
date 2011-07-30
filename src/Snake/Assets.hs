module Snake.Assets where

import Common.Assets
import Common.Counters
import Common.Graphics
import Common.Util
import Data.Array
import Data.List
import Data.Maybe
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import qualified Data.Map as Map
import qualified Data.Set as Set
import Snake.GameState

loadSounds :: IO (Map.Map Sfx Chunk)
loadSounds = do
	chomp <- loadWAV$ getAssetPath "sfx/chomp.wav"
	bump <- loadWAV$ getAssetPath "sfx/bump.wav"
	return$ Map.fromList$ [(Chomp, chomp), (Bump, bump)]

loadSprites :: IO (Map.Map Tile Animation)
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
		tileAnimation Digits = Animation {
		 	surface = digits,
		 	frames = listArray (0, 0) [Rect 0 0 20 180]}
		tileAnimation Paused = Animation {
		 	surface = paused,
		 	frames = listArray (0, 0) [Rect 0 0 234 160]}
		tileAnimation GameOverTile = Animation {
			surface = gameOver,
			frames = listArray (0, 0) [Rect 0 0 200 64]}
		tileAnimation SidePanel = Animation {
		 	surface = sidePanel,
		 	frames = listArray (0, 0) [Rect 0 0 200 480]}
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
	return$ Map.fromList$ map (\tile ->
		(tile, tileAnimation tile)) allTiles

loadLevel :: Int -> GameState -> IO (GameState)
loadLevel level state = do
	-- load the level file
	fileData <- fmap lines$
		readFile$ getAssetPath$ "levels/snake" ++ (show level)
	let startDirection = case trim (head fileData) of
		"D" -> DDown
		"U" -> DUp
		"L" -> DLeft
		"R" -> DRight
		_ -> error$ "Invalid level file for level " ++ (show level)
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
			_ -> error$ "Invalid level file for level " ++ (show level)
		)$ zip [0..] (oddElems line))$ zip [0..] (tail fileData)
	
	-- prepare the wallStamp
	let
		wallStamp = gs_wallStamp state
		gfx = gs_gfx state
		noRenderSprites = [DoorInH, DoorInV, DoorOutH, DoorOutV, AppleA, AppleB]
	fillRect wallStamp (Just$ Rect 0 0 480 480) (Pixel 0x00000000)
	mapM_ (\((x, y), tile) ->
		renderAnimation wallStamp 0 (x * 16) (y * 16) (gfx Map.! tile))
		(filter (\(_, tile) -> not$elem tile noRenderSprites) levelMap)
	
	-- Prepare the doors
	let isInDoorTile = (\(_, tile) -> tile == DoorInH || tile == DoorInV)
	let isOutDoorTile = (\(_, tile) -> tile == DoorOutH || tile == DoorOutV)
	let inDoor = fst$ fromJust$ find isInDoorTile levelMap
	let outDoor = fst$ fromJust$ find isOutDoorTile levelMap
	let snakeCells = map (\((dx, dy), visible) ->
		(((fst inDoor) + dx, (snd inDoor) + dy), visible)) $
			case startDirection of
				DUp -> map (\i -> ((0, i), i == 0)) [0..(4 + (2 * level))]
				DDown -> map (\i -> ((0, -i), i == 0)) [0..(4 + (2 * level))]
				DLeft -> map (\i -> ((-i, 0), i == 0)) [0..(4 + (2 * level))]
				DRight -> map (\i -> ((i, 0), i == 0)) [0..(4 + (2 * level))]
	
	-- Initialise the state
	return$ state {
		gs_nextDirection = startDirection, gs_currentDirection = startDirection,
		gs_ttFrameSwap = 0,
		gs_fastMode = False,
		gs_framesToAlignment = 15,
		gs_holdCount = 0,
		gs_snakeCells = snakeCells,
		gs_foodCells = Map.fromList$ concatMap (\((x, y), tile) ->
				if tile == AppleA || tile == AppleB
					then [((x, y), tile)] else []
			) levelMap,
		gs_wallCells = Set.fromList$ concatMap (\(cell, tile) ->
			if (elem tile [WallV, WallH, WallUL, WallUR, WallDR, WallDL,
				WallTVU, WallTVD, WallTHL, WallTHR, WallDot,
				WallXR, WallXL, WallXU, WallXD, WallX])
				then [cell] else []) levelMap,
		gs_inDoor = (fst inDoor, snd inDoor, True),
		gs_inDoorTile = snd$ fromJust$ find isInDoorTile levelMap,
		gs_outDoor = (fst outDoor, snd outDoor, False),
		gs_outDoorTile = snd$ fromJust$ find isOutDoorTile levelMap,
		gs_loadLevel = False,
		gs_level = level,
		gs_levelCounter = setCounter level (gs_levelCounter state),
		gs_eatingApples = []
	}

makeAnimation :: Surface -> Int -> Int -> Int -> Int -> Animation
makeAnimation surface w h x y =
	Animation {
		surface = surface,
		frames = listArray (0, 0) [Rect (x * w) (y * h) w h]
	}

