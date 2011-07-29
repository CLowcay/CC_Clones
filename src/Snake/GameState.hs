module Snake.GameState (
	Direction(..),
	GameState(..),
	Tile(..), allTiles,
	appleValues, appleValuesR,
	updateGame, inferSnakeTiles
) where

import Common.Counters
import Common.Graphics
import Common.Util
import Data.List
import Data.Maybe
import Debug.Trace
import Graphics.UI.SDL
import qualified Data.Map as Map
import qualified Data.Set as Set

data Direction = DLeft | DRight | DUp | DDown deriving (Enum, Eq, Ord, Show)

-- The complete state of the game at any point in time
data GameState = GameState {
	gs_gfx :: Map.Map Tile Animation,
	gs_wallStamp :: Surface,
	gs_nextDirection :: Direction,
	gs_ttFrameSwap :: Integer,
	gs_framesToAlignment :: Int,
	gs_holdCount :: Int,
	gs_snakeCells :: [((Int, Int), Bool)],
	gs_foodCells :: Map.Map (Int, Int) Int,
	gs_wallCells :: Set.Set (Int, Int),
	gs_inDoor :: (Int, Int, Bool),
	gs_inDoorTile :: Tile,
	gs_outDoor :: (Int, Int, Bool),
	gs_outDoorTile :: Tile,
	gs_score :: Int, gs_scoreCounter :: CounterState,
	gs_loadLevel :: Bool, -- set to true if the level must be reloaded
	gs_level :: Int, gs_levelCounter :: CounterState,
	gs_gameOver :: Bool, gs_paused :: Bool
} deriving (Show)

data Tile = Digits | Paused | SidePanel |
	HeadDown | HeadLeft | HeadRight | HeadUp |
	SnakeV | SnakeH | SnakeUL | SnakeUR | SnakeDR | SnakeDL |
	SnakeTHL | SnakeTHR | SnakeTVU | SnakeTVD | AppleA | AppleB |
	WallV | WallH | WallUL | WallUR | WallDR | WallDL |
	WallTVU | WallTVD | WallTHL | WallTHR | WallDot |
	WallXR | WallXL | WallXU | WallXD | WallX |
	DoorInH | DoorOutH | DoorInV | DoorOutV
	deriving (Enum, Ord, Eq, Show)
allTiles = enumFrom Digits   -- A list of all the tiles

-- Values of the food tiles
appleValues :: Map.Map Tile Int
appleValues = Map.fromList [(AppleA, 1), (AppleB, 5)]
appleValuesR :: Map.Map Int Tile
appleValuesR = Map.fromList [(1, AppleA), (5, AppleB)]

-- The delay for snake frames, in picoseconds
frameDelay :: Integer
frameDelay = ((1::Integer) * 10^12) `div` 64

-- Update the game state based on a time delta
updateGame :: Integer -> GameState -> GameState
updateGame _ (state@(GameState {gs_gameOver = True})) = state
updateGame _ (state@(GameState {gs_paused = True})) = state
updateGame delay state =
	let
		anidiff = (gs_ttFrameSwap state) - delay
		advanceFrames = fromInteger$ if anidiff < 0
			then ((abs anidiff) `div` frameDelay) + 1 else 0
		offset' = (gs_framesToAlignment state) - advanceFrames
		framesToAlignment = if offset' < 0
			then offset' `mod` 16 else offset'
		advanceCells = if offset' < 0
			then ((abs offset') `div` 16) + 1 else 0
		eatenApple =
			if advanceCells > 0 && (Map.member (fst$head snakeCells) foodCells)
				then Just$ foodCells Map.! (fst$head snakeCells) else Nothing
		scoreCounter = 
			addCounter (fromMaybe 0 eatenApple) (gs_scoreCounter state)
		snakeCells' =
			hideExitedCells outDoor $
				advanceAllCells snakeCells (gs_holdCount state) advanceCells
		level = if (isOpen outDoor') && (all (not.snd) snakeCells)
			then (gs_level state) + 1 else (gs_level state)
		levelCounter = setCounter level (gs_levelCounter state)
		foodCells' = if isJust eatenApple
			then Map.delete (fst$head snakeCells) foodCells
			else foodCells
		inDoor' = if ((not$snd$last snakeCells) && (snd$last snakeCells'))
			then closeDoor inDoor else inDoor
		outDoor' = if Map.size foodCells' == 0
			then openDoor outDoor else outDoor
	in
		state {
			gs_framesToAlignment = framesToAlignment,
			gs_ttFrameSwap = if anidiff < 0
				then frameDelay + (anidiff `mod` frameDelay)
				else anidiff,
			gs_holdCount =
				(max 0 (gs_holdCount state - advanceCells)) + (fromMaybe 0 eatenApple),
			gs_snakeCells = snakeCells',
			gs_inDoor = inDoor', gs_outDoor = outDoor',
			gs_foodCells = foodCells',
			gs_score = (gs_score state) + (fromMaybe 0 eatenApple),
			gs_scoreCounter = updateCounter delay scoreCounter,
			gs_levelCounter = updateCounter delay levelCounter,
			gs_level = level,
			gs_loadLevel = level /= (gs_level state),
			gs_gameOver =
				if (snd$head snakeCells') &&
					(not$allClear inDoor' outDoor' advanceCells)
				then True else gs_gameOver state
		}
	where
		snakeCells = gs_snakeCells state
		foodCells = gs_foodCells state
		inDoor = gs_inDoor state
		outDoor = gs_outDoor state
		-- Are all the cells we pass over clear of obstructions
		allClear inDoor' outDoor' advance = all (\a ->
				isClear (head snakeCells) inDoor' outDoor' a
			) [1 .. advance]
		-- Is a cell clear of obstruction
		isClear ((x, y), _) inDoor' outDoor' advance =
			let cell = case gs_nextDirection state of
				DUp -> (x, y - advance)
				DDown -> (x, y + advance)
				DLeft -> (x - advance, y)
				DRight -> (x + advance, y)
			in (not$Set.member cell (gs_wallCells state)) &&
				(not$inDoor' == (fst cell, snd cell, False)) &&
				(not$outDoor' == (fst cell, snd cell, False)) &&
				(not$any (\(scell, _) -> cell == scell) snakeCells)
		-- Hide snake cells that have passed through the exit
		hideExitedCells (x, y, _) snake =
			reverse (hideExitedCells0 (reverse snake) False True)
			where
				hideExitedCells0 [] passed isTail = []
				hideExitedCells0 (((cx, cy), visible):cells) passed isTail =
					let passed' = ((cx == x && cy == y) || passed) in
						((cx, cy), visible && (not (if isTail then passed' else passed))):
							(hideExitedCells0 cells passed' False)
		-- Advance the snake multiple cells
		advanceAllCells snake holdCount 0 = snake
		advanceAllCells snake holdCount n =
			advanceAllCells (advanceCell snake holdCount) (holdCount - 1) (n - 1)
		-- Advance the snake a single cell
		advanceCell (h@((x, y), _):body) holdCount =
			case gs_nextDirection state of
				DUp -> ((x, y - 1), True):h:(nextBody holdCount body)
				DDown -> ((x, y + 1), True):h:(nextBody holdCount body)
				DLeft -> ((x - 1, y), True):h:(nextBody holdCount body)
				DRight -> ((x + 1, y), True):h:(nextBody holdCount body)
		-- Get the next body of the snake, accounting for growth
		nextBody holdCount body =
			if holdCount > 0 then body else Data.List.init body
		-- Open and close doors
		openDoor door = let (x, y, _) = door in (x, y, True)
		closeDoor door = let (x, y, _) = door in (x, y, False)
		isOpen (_, _, open) = open

-- Infer the tile to use for each cell in the snake, based on the
-- surrounding cells
inferSnakeTiles :: [(Int, Int)] -> [Tile]
inferSnakeTiles cells =
	(case (diffs$ take 2 cells) of
		[(0, 0), (1, 0)] -> HeadLeft
		[(0, 0), (-1, 0)] -> HeadRight
		[(0, 0), (0, 1)] -> HeadUp
		[(0, 0), (0, -1)] -> HeadDown
		x -> traceShow x (error "Invalid diffs for head")
	) : (map (\xs -> case (diffs xs) of
		[(0, 0), (1, 0), (1, 0)] -> SnakeH
		[(0, 0), (-1, 0), (-1, 0)] -> SnakeH
		[(0, 0), (0, 1), (0, 1)] -> SnakeV
		[(0, 0), (0, -1), (0, -1)] -> SnakeV
		[(0, 0), (1, 0), (0, 1)] -> SnakeUR
		[(0, 0), (0, -1), (-1, 0)] -> SnakeUR
		[(0, 0), (0, 1), (1, 0)] -> SnakeDL
		[(0, 0), (-1, 0), (0, -1)] -> SnakeDL
		[(0, 0), (-1, 0), (0, 1)] -> SnakeUL
		[(0, 0), (0, -1), (1, 0)] -> SnakeUL
		[(0, 0), (0, 1), (-1, 0)] -> SnakeDR
		[(0, 0), (1, 0), (0, -1)] -> SnakeDR
		x -> traceShow x (error "Invalid diffs for body")
	) (slidingWindow 3 cells)) ++
	(case (diffs$ drop ((length cells) - 2) cells) of
		[(0, 0), (1, 0)] -> [SnakeTHR]
		[(0, 0), (-1, 0)] -> [SnakeTHL]
		[(0, 0), (0, 1)] -> [SnakeTVD]
		[(0, 0), (0, -1)] -> [SnakeTVU]
		x -> traceShow x (error "Invalid diffs for tail")
	)
	where
		diffs xs = reverse$snd$ foldl
			(\((x0, y0), rs) -> \(x, y) -> ((x, y), (x - x0, y - y0):rs))
			(head xs, []) xs

