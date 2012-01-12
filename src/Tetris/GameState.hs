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
	Tile(..), Brick(..), Rotation(..), SlideAction(..),
	allTiles, clearField, srsCoords, tile, tileS,
	updateGame, randomBag, nextBrick
) where

import Common.AniTimer
import Common.Counters
import Common.Util
import Control.Monad.State
import Data.Array
import Data.Maybe
import Debug.Trace
import qualified Common.Queue as Q
import System.Random

data GameMode =
	IntroMode | InGameMode | PausedMode | GameOverMode | HighScoreMode
	deriving (Enum, Eq, Show)

data Tile = Digits | Paused | GameOverTile |
	FrameH | FrameV |
	RedTile | PinkTile | YellowTile |
	OrangeTile | BlueTile | GreyTile | GreenTile
	deriving (Enum, Ord, Eq, Show)
allTiles = enumFrom Digits   -- A list of all the tiles

data Brick = IBrick | JBrick | LBrick | OBrick | SBrick | TBrick | ZBrick
	deriving (Enum, Ord, Eq, Show)
firstBrick = IBrick
lastBrick = ZBrick
data Rotation = RUp | RDown | RLeft | RRight
	deriving (Enum, Ord, Eq, Show)

data SlideAction = SlideLeft | SlideRight
	deriving (Enum, Ord, Eq, Show)

type Field = Array (Int, Int) (Maybe Tile)

-- The complete state of the game at any point in time
data GameState = GameState {
	mode :: GameMode,
	randomState :: StdGen,
	brickQueue :: Q.Queue Brick,
	gracePeriod :: Bool,
	currentBrick :: Brick,
	currentRotation :: Rotation,
	currentHeight :: Int, -- 0 indexed, axis bottom to top
	currentPos :: Int, -- 0 indexed, axis goes left to right
	currentSlide :: Maybe SlideAction,
	field :: Field,
	downTimer :: AniTimer,
	slideTimer :: AniTimer,
	-- FTA = Frames To Alignment
	downFTA :: Int,
	slideFTA :: Int,
	score :: Int, scoreCounter :: CounterState,
	sfxEvents :: [(Sfx, Channels)],  -- sounds to be played after rendering
	level :: Int, levelCounter :: CounterState,
	dropKey :: Bool
} deriving (Show)

clearField :: Array (Int, Int) (Maybe Tile)
clearField = array ((0, 0), (8, 22))
	[((x, y), if x == y then Just RedTile else Nothing)|x <- [0..8], y <- [0..22]]

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

-- Convert block coordinates to field coordinates
toFieldCoords :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
toFieldCoords height pos = map (\(x, y) -> (x + pos, height - y))

-- Determine if a list of field coordinates are a valid block position
isValidPosition :: [(Int, Int)] -> Field -> Bool
isValidPosition coords field =
	all (\(x, y) -> y >= 0 && (isNothing$ field ! (x, y))) coords

-- Generate a random bag of blocks
randomBag :: RandomGen r => State r (Q.Queue Brick)
randomBag = do
	p <- permutation [firstBrick..lastBrick]
	return (Q.enqueueMany Q.empty p)

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
getDropDelay :: Int -> Bool -> Double
getDropDelay level dropkey = ((1 :: Double) * 10^3) / divisor
	where divisor =
		(((fromIntegral level) * 8) + 32) * (if dropkey then 10 else 1)

-- How long to display the game over message, in milliseconds
gameOverDelay :: Double
gameOverDelay = ((1 :: Double) * 10^3) * 4

slideDelay :: Double
slideDelay = ((1 :: Double) * 10 ^ 3) / ((fromIntegral tileS) * 2)

-- Determine the next GameState from the current GameState
updateGame :: Int -> GameState -> GameState
updateGame delay (state@(GameState {mode = GameOverMode})) =
	let
		(frames, downTimer') =
			runState (advanceFrames delay gameOverDelay) (downTimer state)
		done = frames > 0
	in state {
		mode = if done then IntroMode else GameOverMode,
		level = if done then 0 else level state,
		downTimer = if done then resetTimer else downTimer',
		sfxEvents = []
	}
updateGame _ (state@(GameState {mode = PausedMode})) = state
updateGame _ (state@(GameState {mode = IntroMode})) = state
updateGame delay (state@(GameState {mode = InGameMode, ..})) = let
		(downFrames, downTimer') =
			runState (advanceFrames delay dropDelay) downTimer
		(slideFrames, slideTimer') =
			runState (advanceFrames delay slideDelay) slideTimer
		downOffset' = downFTA - downFrames
		downFTA' = if downOffset' < 0
			then downOffset' `mod` tileS else downOffset'
		downCells = if downOffset' < 0
			then (abs downOffset' `div` tileS) + 1 else 0
		((currentHeight', gracePeriod'), field') =
			runState (updateBrick downCells) field
		slideOffset' = slideFTA - slideFrames
		slideFTA' = if slideOffset' < 0
			then slideOffset' `mod` tileS else slideOffset'
		state' = state {
			mode = InGameMode,
			downFTA = downFTA',
			slideFTA = if isSliding then slideFTA' else slideFTA,
			downTimer = downTimer',
			slideTimer = if isSliding then slideTimer' else slideTimer,
			field = field',
			gracePeriod = gracePeriod',
			currentHeight = if currentHeight' < 0 then 22 else currentHeight'
		}
	in
		if currentHeight' < 0 then nextBrick state' else state'
	where
		dropDelay = getDropDelay level dropKey
		isSliding = isJust currentSlide
		brickFieldCoords height = toFieldCoords height currentPos
			(srsCoords currentBrick currentRotation)
		blockDown 0 height = (height, False)
		blockDown downCells height =
			if isValidPosition (brickFieldCoords (height - 1)) field
				then blockDown (downCells - 1) (height - 1)
				else (height, True)
		updateBrick :: Int -> State Field (Int, Bool)
		updateBrick downCells = do
			if gracePeriod then
				if downCells > 0 then do
					currentField <- get
					put$ mergeField currentField
						(brickFieldCoords currentHeight) (tile currentBrick)
					return (-1, False)
				else return (currentHeight, True)
			else return (blockDown downCells currentHeight)
nextBrick (state@(GameState {..})) = let
		(brick, bricks') = Q.dequeue brickQueue
		emptyBag = Q.null bricks'
		(newBag, randomState') = runState randomBag randomState
	in
		state {
			randomState = if emptyBag then randomState' else randomState,
			gracePeriod = False,
			brickQueue = if emptyBag then newBag else bricks',
			currentBrick = brick,
			currentHeight = 22,
			currentPos = 0,
			currentRotation = RUp,
			currentSlide = Nothing
		}

mergeField :: Field -> [(Int, Int)] -> Tile -> Field
mergeField field coords tile = field // (coords `zip` (repeat$ Just tile))

