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

module Tetris.GameState (
	GameMode(..), GameState(..),
	Sfx(..), Channels(..),
	Tile(..), Brick(..), Rotation(..),
	allTiles, clearField, srsCoords, tile, tileS,
	updateGame
) where

import Common.Counters
import Data.Array
import Data.Maybe
import qualified Common.Queue as Q

data GameMode =
	IntroMode | InGameMode | PausedMode | GameOverMode | HighScoreMode
	deriving (Enum, Eq, Show)

data Tile = Digits | Paused | GameOverTile |
	RedTile | PinkTile | YellowTile |
	OrangeTile | BlueTile | GreyTile | GreenTile
	deriving (Enum, Ord, Eq, Show)
allTiles = enumFrom Digits   -- A list of all the tiles

data Brick = IBrick | JBrick | LBrick | OBrick | SBrick | TBrick | ZBrick
	deriving (Enum, Ord, Eq, Show)
data Rotation = RUp | RDown | RLeft | RRight
	deriving (Enum, Ord, Eq, Show)

-- The complete state of the game at any point in time
data GameState = GameState {
	mode :: GameMode,
	brickQueue :: Q.Queue Brick,
	currentBrick :: Brick,
	currentRotation :: Rotation,
	currentHeight :: Int,
	currentPos :: Int,
	field :: [Array Int (Maybe Tile)],
	ttFrameSwap :: Int,
	framesToAlignment :: Int,
	score :: Int, scoreCounter :: CounterState,
	sfxEvents :: [(Sfx, Channels)],  -- sounds to be played after rendering
	level :: Int, levelCounter :: CounterState,
	dropKey :: Bool
} deriving (Show)

clearField :: [Array Int (Maybe Tile)]
clearField = replicate 24 (listArray (0, 9) (replicate 10 Nothing))

data Sfx = SfxTurn | SfxLine
	deriving (Enum, Ord, Eq, Show)

data Channels = SfxChannel | ChannelCount
	deriving (Enum, Ord, Eq, Show)

-- Coordinates of tiles according to the SRS rotation scheme
-- coords are relative to a 4x4 grid.  See http://tetris.wikia.com/wiki/SRS
srsCoords :: Brick -> Rotation -> [(Int, Int)]
srsCoords IBrick RUp = [(0, 1), (1, 1), (2, 1), (3, 1)]
srsCoords IBrick RRight = [(2, 0), (2, 1), (2, 2), (2, 3)]
srsCoords IBrick RDown = [(0, 2), (1, 2), (2, 2), (3, 2)]
srsCoords IBrick RLeft = [(1, 0), (1, 1), (1, 2), (1, 3)]

srsCoords JBrick RUp = [(0, 0), (0, 1), (1, 1), (2, 1)]
srsCoords JBrick RRight = [(1, 0), (2, 0), (1, 1), (1, 2)]
srsCoords JBrick RDown = [(0, 1), (1, 1), (2, 1), (2, 2)]
srsCoords JBrick RLeft = [(1, 0), (1, 1), (1, 2), (0, 2)]

srsCoords LBrick RUp = [(0, 1), (1, 1), (2, 1), (2, 0)]
srsCoords LBrick RRight = [(1, 0), (1, 1), (1, 2), (2, 2)]
srsCoords LBrick RDown = [(0, 2), (0, 1), (1, 1), (2, 1)]
srsCoords LBrick RLeft = [(0, 0), (1, 0), (1, 1), (1, 2)]

srsCoords OBrick _ = [(1, 0), (2, 0), (2, 1), (1, 1)]

srsCoords SBrick RUp = [(0, 1), (1, 1), (1, 0), (2, 0)]
srsCoords SBrick RRight = [(1, 0), (1, 1), (2, 1), (2, 2)]
srsCoords SBrick RDown = [(0, 2), (1, 2), (1, 1), (2, 1)]
srsCoords SBrick RLeft = [(0, 0), (0, 1), (1, 1), (1, 2)]

srsCoords TBrick RUp = [(1, 0), (0, 1), (1, 1), (2, 1)]
srsCoords TBrick RRight = [(2, 1), (1, 0), (1, 1), (1, 2)]
srsCoords TBrick RDown = [(1, 2), (0, 1), (1, 1), (2, 1)]
srsCoords TBrick RLeft = [(0, 1), (1, 0), (1, 1), (1, 2)]

srsCoords ZBrick RUp = [(0, 0), (1, 0), (1, 1), (2, 1)]
srsCoords ZBrick RRight = [(2, 0), (2, 1), (1, 1), (1, 2)]
srsCoords ZBrick RDown = [(0, 1), (1, 1), (1, 2), (2, 2)]
srsCoords ZBrick RLeft = [(1, 0), (1, 1), (0, 1), (0, 2)]

-- How big is a cell
tileS = 26 :: Int

-- Define the colours of the bricks
tile :: Brick -> Tile
tile IBrick = RedTile 
tile JBrick = PinkTile 
tile LBrick = YellowTile 
tile OBrick = OrangeTile 
tile SBrick = BlueTile 
tile TBrick = GreyTile 
tile ZBrick = GreenTile

-- The delay between frames
getFrameDelay :: Int -> Bool -> Int
getFrameDelay level dropkey = (1 * 10^3) `div` divisor
	where divisor =
		((level * 8) + 32) * (if dropkey then 4 else 1)

-- Determine the next GameState from the current GameState
updateGame :: Int -> GameState -> GameState
updateGame delay (state@(GameState {mode = GameOverMode})) =
	let
		ttFrameSwap' = ttFrameSwap state - delay
		done = ttFrameSwap' <= 0
	in state {
		mode = if done then IntroMode else GameOverMode,
		level = if done then 0 else level state,
		ttFrameSwap = max 0 ttFrameSwap',
		sfxEvents = []
	}
updateGame _ (state@(GameState {mode = PausedMode})) = state
updateGame _ (state@(GameState {mode = IntroMode})) = state
updateGame delay (state@(GameState {mode = InGameMode, ..})) = let
		anidiff = ttFrameSwap - delay
		ttFrameSwap' = if anidiff < 0
			then frameDelay + (anidiff `mod` frameDelay)
			else anidiff
		advanceFrames = if anidiff < 0
			then (abs anidiff `div` frameDelay) + 1 else 0
		offset' = framesToAlignment - advanceFrames
		framesToAlignment' = if offset' < 0
			then offset' `mod` tileS else offset'
		advanceCells = if offset' < 0
			then (abs offset' `div` tileS) + 1 else 0
	in
		state {
			mode = InGameMode,
			framesToAlignment = framesToAlignment',
			ttFrameSwap = ttFrameSwap'
		}
	where
		frameDelay = getFrameDelay level dropKey

