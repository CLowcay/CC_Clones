module Dogfight.GameState (
	Direction(..),
	GameMode(..), GameState(..),
	Tile(..), allTiles,
	Sfx(..), Channels(..),
	updateGame
) where

-- Exposed types

data Direction = DLeft | DRight | DUp | DDown
	deriving (Enum, Eq, Show)

data GameMode =
	IntroMode | InGameMode | PausedMode | GameOverMode | HighScoreMode
	deriving (Enum, Eq, Show)

data AI = {
	x :: Int,
	y :: Int,
	direction :: Direction,
	offset :: Int,
	exploding :: Bool
} deriving (Show)

data Laser = {
	x :: Int,
	y :: Int,
	direction :: Direction,
	offset :: Int,
	framesToAlignment :: Int
}

data GameState {
	mode :: GameMode,
	aniTimer :: AniTimer,
	framesToAlignment :: Int,
	score :: Int, scoreCounter :: CounterState,
	sfxEvents :: [(Sfx, Channels)],  -- sounds to be played after rendering
	level :: Int, levelCounter :: CounterState,

	direction :: Direction,
	offset :: Int,
	turn :: Direction,
	exploding :: Bool,
	baddies :: [AI],
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

data Sfx = Fire | Hit
	deriving (Enum, Ord, Eq, Show)

data Channels = SfxChannel1 | SfxChannel2 | ChannelCount
	deriving (Enum, Ord, Eq, Show)

-- Update the game state from a delta
updateGame :: Int -> GameState -> GameState
updateGame delay (state@(GameState {mode = InGameMode, ..})) = let

-- Move lasers and detect laser collisions
doLasers :: Int -> GameState -> GameState

-- Move spaceships.  For every alignment boundary crossed, run the ai and update
-- the player direction.
doSpaceships :: Int -> GameState -> GameState

