module Dogfight.GameState (
	Direction(..),
	GameMode(..), GameState(..),
	Tile(..), allTiles,
	Sfx(..), Channels(..),
	updateGame
) where

import Control.Monad
import Data.List
import Data.Maybe

-- Exposed types

data Direction = DLeft | DRight | DUp | DDown
	deriving (Enum, Eq, Show)

data GameMode =
	IntroMode | InGameMode | PausedMode | GameOverMode | HighScoreMode
	deriving (Enum, Eq, Show)

data Motion = {
	position :: (Int, Int),
	direction :: Direction,
	offset :: Int
} deriving (Show)

data Ship = {
	ship_motion :: Motion,
	ship_exploding :: Bool
} deriving (Show)

data Laser = {
	laser_motion :: Motion,
	laser_fta :: Int
} deriving (Show)

data GameState {
	mode :: GameMode,
	laserTimer :: AniTimer,
	shipsTimer :: AniTimer,
	playerTimer :: AniTimer,
	ships_fta :: Int,
	player_fta :: Int,
	score :: Int, scoreCounter :: CounterState,
	sfxEvents :: [(Sfx, Channels)],  -- sounds to be played after rendering
	level :: Int, levelCounter :: CounterState,

	player :: Ship,
	turn :: Direction,
	baddies :: [Ship],
	lasers :: [Laser]
} deriving (Show)

data Tile = Digits | Paused | GameOverTile |
	BlueTile | PinkTile | OrangeTile | YellowTile |
	RedTile | GreyTile | GreenTile |
	PlayerR | PlayerD | PlayerL | PlayerU |
	AiR | AiD | AiL | AiU | EngineR | EngineD | EngineL | EngineU |
	LaserR | LaserD | LaserL | LaserU
	deriving (Enum, Ord, Eq, Show)
allTiles = enumFrom Digits   -- A list of all the tiles

tileSize = 26
gridSize = 19

data Sfx = Fire | Hit
	deriving (Enum, Ord, Eq, Show)

data Channels = SfxChannel1 | SfxChannel2 | ChannelCount
	deriving (Enum, Ord, Eq, Show)

-- Update the game state from a delta
updateGame :: Int -> GameState -> GameState
updateGame delay (state@(GameState {mode = InGameMode, ..})) = 
	doSpaceshiops delay $ doLasers delay state

-- frames -> fta -> (fta', advanceTiles)
updateFTA :: Int -> Int -> (Int, Int)
updateFTA frames fta =
	let offset = fta - frames
	in if offset < 0
		then (offset `mod` tileSize, (abs offset `div` tileSize) + 1)
		else (offset, 0)

updatePosition :: (Int, Int) -> Direction -> (Int, Int)
updatePosition (x, y) direction = case direction of
	DUp -> (x, y - 1)
	DDown -> (x, y + 1)
	DLeft -> (x - 1, y)
	DRight -> (x + 1, y)
	
-- Move lasers and detect laser collisions
doLasers :: Int -> GameState -> GameState
doLasers delay (state@(GameState {...})) = let
		(frames, laserTimer') =
			runState (advanceFrames delay laserDelay) laserTimer
		(lasers', (baddies', playerDead)) =
			runState updateLasers lasers $ (baddies, False)
	in
		state {
			laserTimer = laserTimer',
			player = if (playerDead) then player {ship_exploding = True} else player,
			baddies = baddies',
			lasers = lasers'
		}
	where
		-- Laser collision detection, also computes effects on baddies and player
		wallCollision (x, y) = x < 0 || x >= gridSize || y < 0 || y >= gridSize
		collision :: (Int, Int) -> State ([Ship], Bool) Bool
		collision pos = do
			(baddies, playerDead) <- get
			let (isCollision, baddies') =
				mapAccumR (c -> ship -> if position.ship_motion ship == pos
					then (True, ship {ship_exploding = True}) else (c, ship)
				) False baddies

			let gotPlayer = position.ship_motion player == pos
			put (baddies', playerDead || gotPlayer)
			return$ isCollision || gotPlayer

		-- Move the laser, do collision detection,
		-- and compute effects of collisions 
		updateLaser :: Laser -> State ([Ship], Bool) (Maybe Laser)
		updateLaser laser = do
			(fta', advance) <- updateFTA frames (laser_fta laser)

			(laser', inFlight') <-
				foldM (\_ (l@(Laser {laser_motion}), inFlight) -> do
					pos <- updatePosition
						(position laser_motion) (direction laser_motion)
					c <- collision pos
					return$ if c then (laser, False)
						else if wallCollision pos then (laser, False)
							else (laser {
								laser_motion {position = pos},
								laser_fta = fta'}, inFlight)
				) (laser, True) [1..advance]

			return$ if inFlight' then Just laser else None

		-- update all the lasers
		updateLasers :: [Laser] -> State ([Ship], Bool) [Laser]
		updateLasers lasers = do
			ml <- mapM updateLaser lasers
			return$ catMaybes ml

-- Move spaceships.  For every alignment boundary crossed, run the ai and update
-- the player direction.
doSpaceships :: Int -> GameState -> GameState
doSpaceships delay (state@(GameState {...})) = let
		(playerFrames, playerTimer') =
			runState (advanceFrames delay playerDelay) playerTimer
		(shipsFrames, shipsTimer') =
			runState (advanceFrames delay shipsDelay) shipsTimer
	in

