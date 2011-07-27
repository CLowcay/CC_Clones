module Snake.GameState where

import Common.Util
import Data.List
import Data.Maybe
import Debug.Trace
import Graphics.UI.SDL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Snake.Graphics

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
	gs_in_door :: (Int, Int, Bool),
	gs_out_door :: (Int, Int, Bool),
	gs_score :: Int,
	gs_level :: Int
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
frameDelay = ((1::Integer) * 10^12) `div` 32

-- Update the game state based on a time delta
updateGame :: Integer -> GameState -> GameState
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
	in
		state {
			gs_framesToAlignment = framesToAlignment,
			gs_ttFrameSwap = if anidiff < 0
				then frameDelay + (anidiff `mod` frameDelay)
				else anidiff,
			gs_holdCount =
				(max 0 (gs_holdCount state - advanceCells)) + (fromMaybe 0 eatenApple),
			gs_snakeCells =
				advanceAllCells snakeCells (gs_holdCount state) advanceCells,
			gs_foodCells = if isJust eatenApple
				then Map.delete (fst$head snakeCells) foodCells
				else foodCells,
			gs_score = (gs_score state) + (fromMaybe 0 eatenApple)
		}
	where
		snakeCells = gs_snakeCells state
		foodCells = gs_foodCells state
		advanceAllCells snake holdCount 0 = snake
		advanceAllCells snake holdCount n =
			advanceAllCells (advanceCell snake holdCount) (holdCount - 1) (n - 1)
		advanceCell (h@((x, y), _):body) holdCount =
			case gs_nextDirection state of
				DUp -> ((x, y - 1), True):h:(nextBody holdCount body)
				DDown -> ((x, y + 1), True):h:(nextBody holdCount body)
				DLeft -> ((x - 1, y), True):h:(nextBody holdCount body)
				DRight -> ((x + 1, y), True):h:(nextBody holdCount body)
		nextBody holdCount body =
			if holdCount > 0 then body else Data.List.init body

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

