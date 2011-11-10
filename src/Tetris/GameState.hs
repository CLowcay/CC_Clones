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
	allTiles, clearField
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
	level :: Int, levelCounter :: CounterState
} deriving (Show)

clearField :: [Array Int (Maybe Tile)]
clearField = replicate 24 (listArray (0, 9) (replicate 10 Nothing))

data Sfx = SfxTurn | SfxLine
	deriving (Enum, Ord, Eq, Show)

data Channels = SfxChannel | ChannelCount
	deriving (Enum, Ord, Eq, Show)


