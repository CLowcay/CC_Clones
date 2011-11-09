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

{-# LANGUAGE RecordWildCards #-}

module Snake.GameState (
	Direction(..),
	GameMode(..), GameState(..),
	Tile(..), allTiles,
	Sfx(..), Channels(..),
	updateGame, inferSnakeTiles
) where

import Common.Counters
import Common.Graphics
import Common.HighScores
import Common.Util
import Data.List
import Data.Maybe
import Debug.Trace
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.TTF
import qualified Common.Queue as Q
import qualified Data.Map as M
import qualified Data.Set as S

data Direction = DLeft | DRight | DUp | DDown
	deriving (Enum, Eq, Ord, Show)

oppositeDirection DLeft = DRight
oppositeDirection DRight = DLeft
oppositeDirection DUp = DDown
oppositeDirection DDown = DUp

data GameMode =
	IntroMode | InGameMode | PausedMode | GameOverMode | HighScoreMode
	deriving (Enum, Eq, Show)

-- The complete state of the game at any point in time
data GameState = GameState {
	mode :: GameMode,
	fastMode :: Bool,
	highScores :: HighScoreState,
	wallStamp :: Surface,
	introMessage :: Surface, introMessage2 :: Surface,
	highScoreMessage :: Surface,
	-- enqueue on the back, dequeue from the front
	nextDirections :: Q.Queue Direction,
	currentDirection :: Direction,
	ttFrameSwap :: Int,
	framesToAlignment :: Int,
	holdCount :: Int,
	snakeCells :: [((Int, Int), Bool)],
	foodCells :: M.Map (Int, Int) Tile,
	wallCells :: S.Set (Int, Int),
	inDoor :: (Int, Int, Bool),
	inDoorTile :: Tile,
	outDoor :: (Int, Int, Bool),
	outDoorTile :: Tile,
	score :: Int, scoreCounter :: CounterState,
	loadLevel :: Bool,    -- set to true if the level must be reloaded
	sfxEvents :: [(Sfx, Channels)],  -- sounds to be played after rendering
	level :: Int, levelCounter :: CounterState,
	eatingApples :: [((Int, Int), Tile)]
} deriving (Show)

data Tile = Digits | Paused | SidePanel | GameOverTile |
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

-- How long to display the game over message, in milliseconds
gameOverDelay :: Int
gameOverDelay = (1 * 10^3) * 4

-- The delay for snake frames, in milliseconds
getFrameDelay :: Int -> Bool -> Int
getFrameDelay level fastMode = (1 * 10^3) `div` divisor
	where divisor =
		((level * 8) + 32) * (if fastMode then 4 else 1)

-- Get the next direction from the queue
getNextDirection ::
	Direction -> Q.Queue Direction -> (Direction, Q.Queue Direction)
getNextDirection currentDirection directions =
	if Q.null directions then (currentDirection, Q.empty) else
		case Q.dequeue directions of
			(direction, directions') ->
				if direction == currentDirection ||
					oppositeDirection direction == currentDirection
				then getNextDirection currentDirection directions'
				else (direction, directions')

-- Update the game state based on a time delta
updateGame :: Int -> GameState -> GameState
updateGame delay (state@(GameState {mode = GameOverMode})) =
	let
		ttFrameSwap' = ttFrameSwap state - delay
		done = ttFrameSwap' <= 0
	in state {
		mode = if done then IntroMode else GameOverMode,
		level = if done then 0 else level state,
		loadLevel = done,
		ttFrameSwap = max 0 ttFrameSwap',
		sfxEvents = [],
		eatingApples = []
	}
updateGame _ (state@(GameState {mode = PausedMode})) = state
updateGame _ (state@(GameState {mode = IntroMode})) = state
updateGame _ (state@(GameState {mode = HighScoreMode})) =
	state {
		sfxEvents = [],
		eatingApples = []
	}
updateGame delay (state@(GameState {mode = InGameMode, ..})) = let
		anidiff = ttFrameSwap - delay
		ttFrameSwap' = if anidiff < 0
			then frameDelay + (anidiff `mod` frameDelay)
			else anidiff
		advanceFrames = if anidiff < 0
			then (abs anidiff `div` frameDelay) + 1 else 0
		offset' = framesToAlignment - advanceFrames
		framesToAlignment' = if offset' < 0
			then offset' `mod` 16 else offset'
		advanceCells = if offset' < 0
			then (abs offset' `div` 16) + 1 else 0
		snakeCells' =
			hideExitedCells outDoor $
				advanceAllCells snakeCells holdCount advanceCells
		eatenApples = concatMap
			(\(cell, _) -> [cell | cell `M.member` foodCells]) $
			take advanceCells snakeCells'
		eatenApplesValue =
			sum$ map (\cell -> appleValue (foodCells M.! cell)) eatenApples
		scoreCounter' = if gameOver
			then resetCounter 0 scoreCounter
			else addCounter eatenApplesValue scoreCounter
		level' = if isOpen outDoor' && all (not.snd) snakeCells
			then level + 1 else level
		levelCounter' = if gameOver
			then resetCounter 0 levelCounter
			else setCounter level' levelCounter
		foodCells' = foldr M.delete foodCells eatenApples
		inDoor' = if not (snd (last snakeCells)) && snd (last snakeCells')
			then closeDoor inDoor else inDoor
		outDoor' = if M.size (M.filter (== AppleA) foodCells') == 0
			then openDoor outDoor else outDoor
		gameOver = snd (head snakeCells') &&
			not (allClear inDoor' outDoor' advanceCells)
		newHighScore = isNewHighScore score highScores
	in
		state {
			mode = if gameOver
				then if newHighScore
					then HighScoreMode else GameOverMode
				else InGameMode,
			highScores = if gameOver && newHighScore
				then insertHighScore score highScores else highScores,
			framesToAlignment = framesToAlignment',
			nextDirections =
				if advanceCells > 0 then nextDirections' else nextDirections,
			currentDirection =
				if advanceCells > 0 then currentDirection' else currentDirection,
			ttFrameSwap = if gameOver then gameOverDelay else ttFrameSwap',
			holdCount =
				max 0 (holdCount - advanceCells) + eatenApplesValue,
			snakeCells = snakeCells',
			inDoor = inDoor', outDoor = outDoor',
			foodCells = foodCells',
			score = score + eatenApplesValue,
			scoreCounter = updateCounter delay scoreCounter',
			levelCounter = updateCounter delay levelCounter',
			level = level',
			loadLevel = level' /= level,
			eatingApples = if advanceCells > 0
				then map (\cell -> (cell, foodCells M.! cell)) eatenApples
				else eatingApples,
			sfxEvents = 
				[(Bump, SfxChannel2) | gameOver] ++
				replicate (length eatenApples) (Chomp, SfxChannel1)
		}
	where
		(currentDirection', nextDirections') =
			getNextDirection currentDirection nextDirections
		frameDelay = getFrameDelay level fastMode
		-- Are all the cells we pass over clear of obstructions
		allClear inDoor' outDoor' advance = all
			(isClear (head snakeCells) inDoor' outDoor') [1 .. advance]
		-- Is a cell clear of obstruction
		isClear ((x, y), _) inDoor' outDoor' advance =
			let cell = case currentDirection' of
				DUp -> (x, y - advance)
				DDown -> (x, y + advance)
				DLeft -> (x - advance, y)
				DRight -> (x + advance, y)
			in not (cell `S.member` wallCells) &&
				inDoor' /= (fst cell, snd cell, False) &&
				outDoor' /= (fst cell, snd cell, False) &&
				all (\(scell, _) -> cell /= scell) snakeCells
		-- Hide snake cells that have passed through the exit
		hideExitedCells (x, y, _) snake =
			reverse (hideExitedCells0 (reverse snake) False True)
			where
				hideExitedCells0 [] passed isTail = []
				hideExitedCells0 (((cx, cy), visible):cells) passed isTail =
					let passed' = ((cx == x && cy == y) || passed) in
						((cx, cy), visible && not (if isTail then passed' else passed)) :
							hideExitedCells0 cells passed' False
		-- Advance the snake multiple cells
		advanceAllCells snake holdCount 0 = snake
		advanceAllCells snake holdCount n =
			advanceAllCells (advanceCell snake holdCount) (holdCount - 1) (n - 1)
		-- Advance the snake a single cell
		advanceCell (h@((x, y), _):body) holdCount =
			case currentDirection' of
				DUp -> ((x, y - 1), True) : h : nextBody holdCount body
				DDown -> ((x, y + 1), True) : h : nextBody holdCount body
				DLeft -> ((x - 1, y), True) : h : nextBody holdCount body
				DRight -> ((x + 1, y), True) : h : nextBody holdCount body
		-- Get the next body of the snake, accounting for growth
		nextBody holdCount body =
			if holdCount > 0 then body else Prelude.init body
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
	) : map (\xs -> case (diffs xs) of
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
	) (slidingWindow 3 cells) ++
	(case (diffs$ drop (length cells - 2) cells) of
		[(0, 0), (1, 0)] -> [SnakeTHR]
		[(0, 0), (-1, 0)] -> [SnakeTHL]
		[(0, 0), (0, 1)] -> [SnakeTVD]
		[(0, 0), (0, -1)] -> [SnakeTVU]
		x -> traceShow x (error "Invalid diffs for tail")
	)
	where
		diffs xs = reverse$snd$ foldl
			(\((x0, y0), rs) (x, y) -> ((x, y), (x - x0, y - y0):rs))
			(head xs, []) xs

