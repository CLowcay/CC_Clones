module Snake.GameState (
	Direction(..),
	GameState(..),
	Tile(..), allTiles,
	Sfx(..), Channels(..),
	updateGame, inferSnakeTiles
) where

import Common.Counters
import Common.Graphics
import Common.Util
import Data.List
import Data.Maybe
import Debug.Trace
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import qualified Data.Map as Map
import qualified Data.Set as Set

data Direction = DLeft | DRight | DUp | DDown deriving (Enum, Eq, Ord, Show)

-- The complete state of the game at any point in time
data GameState = GameState {
	gs_gfx :: Map.Map Tile Animation,
	gs_sfx :: Map.Map Sfx Chunk,
	gs_wallStamp :: Surface,
	gs_nextDirection :: Direction,
	gs_ttFrameSwap :: Integer,
	gs_fastMode :: Bool,
	gs_framesToAlignment :: Int,
	gs_holdCount :: Int,
	gs_snakeCells :: [((Int, Int), Bool)],
	gs_foodCells :: Map.Map (Int, Int) Tile,
	gs_wallCells :: Set.Set (Int, Int),
	gs_inDoor :: (Int, Int, Bool),
	gs_inDoorTile :: Tile,
	gs_outDoor :: (Int, Int, Bool),
	gs_outDoorTile :: Tile,
	gs_score :: Int, gs_scoreCounter :: CounterState,
	gs_loadLevel :: Bool,    -- set to true if the level must be reloaded
	gs_sfxEvents :: [(Sfx, Channels)],  -- sounds to be played after rendering
	gs_level :: Int, gs_levelCounter :: CounterState,
	gs_gameOver :: Bool, gs_paused :: Bool,
	gs_eatingApples :: [((Int, Int), Tile)]
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

data Sfx = Chomp | Bump
	deriving (Enum, Ord, Eq, Show)

data Channels = SfxChannel1 | SfxChannel2 | ChannelCount
	deriving (Enum, Ord, Eq, Show)

-- Values of the food tiles
appleValue :: Tile -> Int
appleValue AppleA = 1
appleValue AppleB = 5

-- The delay for snake frames, in picoseconds
getFrameDelay :: Int -> Bool -> Integer
getFrameDelay level fastMode = ((1::Integer) * 10^12) `div` divisor
	where divisor =
		(((fromIntegral level) * 16) + 16) * (if fastMode then 4 else 1)

-- Update the game state based on a time delta
updateGame :: Integer -> GameState -> GameState
updateGame _ (state@(GameState {gs_gameOver = True})) =
	state {gs_sfxEvents = [], gs_eatingApples = []}
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
		snakeCells' =
			hideExitedCells outDoor $
				advanceAllCells snakeCells (gs_holdCount state) advanceCells
		eatenApples = concatMap (\(cell, _) ->
				if Map.member cell foodCells then [cell] else []) $
			take advanceCells snakeCells'
		eatenApplesValue =
			sum$ map (\cell -> appleValue (foodCells Map.! cell)) eatenApples
		scoreCounter = addCounter eatenApplesValue (gs_scoreCounter state)
		level = if (isOpen outDoor') && (all (not.snd) snakeCells)
			then (gs_level state) + 1 else (gs_level state)
		levelCounter = setCounter level (gs_levelCounter state)
		foodCells' = foldr (Map.delete) foodCells eatenApples
		inDoor' = if ((not$snd$last snakeCells) && (snd$last snakeCells'))
			then closeDoor inDoor else inDoor
		outDoor' = if Map.size (Map.filter (== AppleA) foodCells') == 0
			then openDoor outDoor else outDoor
		gameOver' = 
			if (snd$head snakeCells') &&
				(not$allClear inDoor' outDoor' advanceCells)
			then True else gameOver
	in
		state {
			gs_framesToAlignment = framesToAlignment,
			gs_ttFrameSwap = if anidiff < 0
				then frameDelay + (anidiff `mod` frameDelay)
				else anidiff,
			gs_holdCount =
				(max 0 (gs_holdCount state - advanceCells)) + eatenApplesValue,
			gs_snakeCells = snakeCells',
			gs_inDoor = inDoor', gs_outDoor = outDoor',
			gs_foodCells = foodCells',
			gs_score = (gs_score state) + eatenApplesValue,
			gs_scoreCounter = updateCounter delay scoreCounter,
			gs_levelCounter = updateCounter delay levelCounter,
			gs_level = level,
			gs_loadLevel = level /= (gs_level state),
			gs_gameOver = gameOver',
			gs_eatingApples = if advanceCells > 0
				then map (\cell -> (cell, foodCells Map.! cell)) eatenApples
				else gs_eatingApples state,
			gs_sfxEvents = concat [
				(if gameOver' && (not gameOver) then [(Bump, SfxChannel2)] else []),
				(take (length eatenApples) (repeat (Chomp, SfxChannel1)))
			]
		}
	where
		sfx = gs_sfx state
		frameDelay = getFrameDelay (gs_level state) (gs_fastMode state)
		snakeCells = gs_snakeCells state
		foodCells = gs_foodCells state
		inDoor = gs_inDoor state
		outDoor = gs_outDoor state
		gameOver = gs_gameOver state
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

