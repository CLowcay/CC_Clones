{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Spaceships.GameState (
	Direction(..),
	GlobalState(..),
	GameOutput(..), HighScoreEditing(..),
	GameRound(..),
	LaneControl(..), GameObject(..), Kind(..),
	Tile(..), allTiles,
	lanePosition,
	introMode
) where

import Common.Counters
import Common.Events
import Common.HighScores
import Control.Applicative
import Control.Monad.State (execState)
import Data.Maybe
import FRP.Events
import FRP.Yampa
import FRP.Yampa.Vector2
import Graphics.UI.SDL hiding (Event, NoEvent)
import qualified Graphics.UI.SDL as SDL

-- Exposed types
-- ----------------------------------------------------------------------------

data Direction = DLeft | DUp | DDown | DRight deriving (Eq, Ord, Show)
data Lane = HLane Int | VLane Int deriving (Eq, Ord, Show)

data GameOutput =
		Intro GlobalState |
		GameOver GlobalState |
		HighScore GlobalState HighScoreState (Event HighScoreEditing) |
		Playing GameRound
	deriving Show

data RoundOutcome = RoundDied | RoundCompleted deriving Show

data HighScoreEditing = EditingStart | EditingStop deriving Show

data GlobalState = GlobalState {
	gs_score :: Int,
	gs_level :: Int,
	gs_scoreC :: CounterState,
	gs_levelC :: CounterState,
	gs_highScores :: HighScoreState
} deriving Show

data GameObject = GameObject Kind LaneControl deriving Show
data LaneControl = LaneControl Lane Float Direction deriving Show
data Kind = Player | Enemy | Laser deriving Show

initSpaceship = GameObject Player (LaneControl (HLane 9) laneMax DLeft)
initEnemy n = GameObject Enemy (LaneControl (VLane n) 0 DDown)

data GameRound = GameRound {
	gr_objects :: [GameObject],
	gr_scoreC :: CounterState,
	gr_levelC :: CounterState
} deriving Show

data Tile = Digits | Paused | GameOverTile |
	SidePanel | BoxTile | Background | FrameV | FrameH |
	PlayerR | PlayerD | PlayerL | PlayerU |
	AiR | AiD | AiL | AiU | EngineR | EngineD | EngineL | EngineU |
	LaserR | LaserD | LaserL | LaserU
	deriving (Enum, Ord, Eq, Show)
allTiles = enumFrom Digits   -- A list of all the tiles

-- Configuration
-- ----------------------------------------------------------------------------

tileSize = 26 :: Int
levelBonus = 0 :: Int
nLanes = 10 :: Int
baseSpeed = 10 :: Float

laneMax :: Float
laneMax = fromIntegral$ ((nLanes + nLanes - 1) * tileSize) - tileSize - 1

lanePosition :: LaneControl -> (Int, Int)
lanePosition (LaneControl lane t _) = case lane of
	HLane n -> (tileSize + (round t), (n * 2 * tileSize) + tileSize)
	VLane n -> ((n * 2 * tileSize) + tileSize, tileSize + (round t))

laneNumbertoPos :: Int -> Float
laneNumbertoPos n = fromIntegral$ n * tileSize * 2

-- Top level SFs
-- ----------------------------------------------------------------------------

introMode :: GlobalState -> SF (Event SDLEvents) GameOutput
introMode gs = switch (doIntro gs) (const$ gameRound gs)

gameOverMode :: GlobalState -> SF (Event SDLEvents) GameOutput
gameOverMode gs = switch (doGameOver gs) (const$ introMode gs)

highScoreMode :: GlobalState -> SF (Event SDLEvents) GameOutput
highScoreMode gs = dSwitch (doHighScore gs) (\gs' -> introMode gs')

gameRound :: GlobalState -> SF (Event SDLEvents) GameOutput
gameRound gs = switch (doGameRound gs)$ \case 
	(RoundCompleted, gs') -> gameRound$ gs' {
		gs_level = (gs_level gs') + 1,
		gs_score = (gs_score gs') + levelBonus,
		gs_scoreC = addCounter levelBonus (gs_scoreC gs),
		gs_levelC = addCounter 1 (gs_levelC gs)
	}
	(RoundDied, gs') -> if isNewHighScore (gs_score gs') (gs_highScores gs')
		then highScoreMode gs' else gameOverMode gs'

-- Mode SFs
-- ----------------------------------------------------------------------------

doIntro :: GlobalState -> SF (Event SDLEvents) (GameOutput, Event ())
doIntro gs = proc e -> do
	eF2 <- sdlKeyPresses (mkKey SDLK_F2) True -< e
	returnA -< (Intro gs, eF2 `tag` ())

doGameOver :: GlobalState -> SF (Event SDLEvents) (GameOutput, Event ())
doGameOver gs = proc e -> do
	r <- after 3 () -< ()
	returnA -< (GameOver gs, r)

doHighScore :: GlobalState ->
	SF (Event SDLEvents) (GameOutput, Event GlobalState)
doHighScore gs = loopPre (gs_highScores gs)$ proc (e, hs0) -> do
	let hs1 = execState (handleEvents highScoreEventHandler (event [] id e)) hs0

	eStart <- now EditingStart -< ()
	let eStop = if not$ isEditing hs1 then Event gs else NoEvent
	let eEditing = eStart `lMerge` (eStop `tag` EditingStop)
	
	returnA -< ((HighScore gs hs1 eEditing, eStop), hs1)

-- Game SFs
-- ----------------------------------------------------------------------------

doGameRound :: GlobalState ->
	SF (Event SDLEvents) (GameOutput, Event (RoundOutcome, GlobalState))
doGameRound gs = proc e -> do
	ship <- spaceship baseSpeed initSpaceship <<< directionInput -< e
	returnA -< (Playing$ GameRound {
			gr_objects = [ship],
			gr_scoreC = gs_scoreC gs,
			gr_levelC = gs_levelC gs
		}, NoEvent)

spaceship :: Float -> GameObject -> SF (Event Direction) GameObject
spaceship speed0 (GameObject s l0) = switch
	(arr (first (GameObject s)) <<< laneMotion speed0 l0)
	(\l -> spaceship speed0 (GameObject s l))

laneMotion :: Float -> LaneControl ->
	SF (Event Direction) (LaneControl, Event LaneControl)
laneMotion speed0 (LaneControl lane0 p0 d0) = proc e -> do
	nextDirection <- hold d0 -< e
	direction <- laneDirection lane0 d0 -< e
	let speed =
		if direction == DLeft || direction == DUp then (0 - speed0) else speed0

	rec
		p <- iPre p0 <<< imIntegral p0 -< rspeed
		let rspeed =
			if p <= 0 && speed < 0 || p >= laneMax && speed > 0 then 0 else speed

	crossing <- boundaryCrossing p0 -< p
	let laneChange = case crossing of
		Event n -> case lane0 of
			(HLane n0) -> if nextDirection == DUp || nextDirection == DDown
				then Event$ LaneControl (VLane n) (laneNumbertoPos n0) nextDirection
				else NoEvent
			(VLane n0) -> if nextDirection == DLeft || nextDirection == DRight
				then Event$ LaneControl (HLane n) (laneNumbertoPos n0) nextDirection
				else NoEvent
		NoEvent -> NoEvent
	
	returnA -< (LaneControl lane0 p direction, laneChange)

laneDirection :: Lane -> Direction -> SF (Event Direction) Direction
laneDirection lane d0 = hold d0 <<< (arr$ dirEvents lane)
	where
		dirEvents (HLane _) = filterE$ liftA2 (||) (== DLeft) (== DRight)
		dirEvents (VLane _) = filterE$ liftA2 (||) (== DUp) (== DDown)

-- generate an event when we enter a new grid cell
boundaryCrossing :: Float -> SF Float (Event Int)
boundaryCrossing p0 = loopPre p0$ proc (p', p) -> do
	let (g0, g1)  = if p' > p
		then (floor$ p / 52, floor$ p' / 52)
		else if p' < p
			then (ceiling$ p / 52, ceiling$ p' / 52)
			-- hack for the case where the spaceship is stalled
			else (-1, floor$ (p + 1) / 52)

	returnA -< (if g0 /= g1 then Event g1 else NoEvent, p')

directionInput :: SF (Event SDLEvents) (Event Direction)
directionInput = proc e -> do
	eKeys <- sdlKeys (mkKey <$> [SDLK_UP, SDLK_DOWN, SDLK_LEFT, SDLK_RIGHT]) True -< e
	returnA -< keyToDirection.last <$> eKeys

keyToDirection :: Keysym -> Direction
keyToDirection k = case k of
	Keysym SDLK_UP _ _ -> DUp
	Keysym SDLK_DOWN _ _ -> DDown
	Keysym SDLK_LEFT _ _ -> DLeft
	Keysym SDLK_RIGHT _ _ -> DRight
	_ -> error "Direction keys incorrectly filtered"

