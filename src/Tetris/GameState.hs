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
	Sfx(..), Channels(..)
) where

import Common.Counters
import Data.Array

data GameMode =
	IntroMode | InGameMode | PausedMode | GameOverMode | HighScoreMode
	deriving (Enum, Eq, Show)

data Tile = RedTile | PinkTile | YellowTile |
	OrangeTile | BlueTile | GreyTile | GreenTile
data Brick = IBrick | JBrick | LBrick | OBrick | SBrick | TBrick | ZBrick
data Rotation = RUp | RDown | RLeft | RRight

-- The complete state of the game at any point in time
data GameState = GameState {
	mode :: GameMode,
	-- brickQueue :: Queue Brick,
	currentBrick :: Brick,
	currentRotation :: Rotation,
	currentHeight :: Int,
	currentPos :: Int,
	field :: [Array Int Tile],
	ttFrameSwap :: Int,
	framesToAlignment :: Int,
	score :: Int, scoreCounter :: CounterState,
	sfxEvents :: [(Sfx, Channels)],  -- sounds to be played after rendering
	level :: Int, levelCounter :: CounterState,
} deriving (Show)

data Sfx = Turn | Line
	deriving (Enum, Ord, Eq, Show)

data Channels = SfxChannel | ChannelCount
	deriving (Enum, Ord, Eq, Show)

