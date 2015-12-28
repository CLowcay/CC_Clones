{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Spaceships.GameState (
	Direction(..),
	GlobalState(..),
	GameOutput(..), HighScoreEditing(..),
	GameRound(..), Spaceship(..), Laser(..),
	Tile(..), allTiles,
	laneToPos,
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

data Direction = DLeft | DUp | DDown | DRight deriving (Eq, Ord, Show)

nLanes = 10
laneMax = ((nLanes + nLanes - 1) * 26) - 26 - 1
data Lane = HLane Int | VLane Int deriving (Eq, Ord, Show)

laneToPos :: Lane -> Float -> (Float, Float)
laneToPos (HLane n) t = (26 + t, fromIntegral$ (n * 2 * 26) + 26)
laneToPos (VLane n) t = (fromIntegral$ (n * 2 * 26) + 26, 26 + t)

laneDirection :: Lane -> Float -> Direction
laneDirection (HLane _) t
	| t < 0 = DLeft
	| otherwise = DRight
laneDirection (VLane _) t
	| t < 0 = DUp
	| otherwise = DDown

laneNumbertoPos :: Int -> Float
laneNumbertoPos n = (fromIntegral n) * 26 * 2

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

data Spaceship = Spaceship Float Lane Direction deriving Show
initSpaceship = Spaceship laneMax (HLane 9) DLeft
initEnemy n = Spaceship 0 (VLane n) DDown

data Laser = Laser (Vector2 Float) Direction deriving Show

data GameRound = GameRound {
	gr_player :: Spaceship,
	gr_enemies :: [Spaceship],
	gr_lasers :: [Laser],
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

tileSize = 26
gridSize = 19

levelBonus = 0

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

doGameRound :: GlobalState ->
	SF (Event SDLEvents) (GameOutput, Event (RoundOutcome, GlobalState))
doGameRound gs = proc e -> do
	ship <- spaceship 4.0 initSpaceship <<< directionInput -< e
	returnA -< (Playing$ GameRound {
			gr_player = ship,
			gr_enemies = [],
			gr_lasers = [],
			gr_scoreC = gs_scoreC gs,
			gr_levelC = gs_levelC gs
		}, NoEvent)

spaceship :: Float -> Spaceship -> SF (Event Direction) Spaceship
spaceship speed0 s0 = switch (spaceshipLane speed0 s0) (spaceship speed0)

spaceshipLane :: Float -> Spaceship -> SF (Event Direction) (Spaceship, Event Spaceship)
spaceshipLane speed0 (Spaceship p0 lane0 d0) = proc e -> do
	-- TODO: set the initial direction properly
	nextDirection <- hold d0 -< e
	speed <- laneSpeed lane0 speed0 d0 -< e
	rec
		p <- iPre p0 <<< imIntegral p0 -< rspeed
		let rspeed =
			if p <= 0 && speed < 0 || p >= laneMax && speed > 0 then 0 else speed

	crossing <- boundaryCrossing p0 -< p
	let laneChange = case crossing of
		Event n -> case lane0 of
			(HLane n0) -> case nextDirection of
				DUp -> Event$ Spaceship (laneNumbertoPos n0) (VLane n) DUp
				DDown -> Event$ Spaceship (laneNumbertoPos n0) (VLane n) DDown
				_ -> NoEvent
			(VLane n0) -> case nextDirection of
				DLeft -> Event$ Spaceship (laneNumbertoPos n0) (HLane n) DLeft
				DRight -> Event$ Spaceship (laneNumbertoPos n0) (HLane n) DRight
				_ -> NoEvent
		NoEvent -> NoEvent
	
	returnA -< (Spaceship p lane0 (laneDirection lane0 speed), laneChange)

laneSpeed :: Lane -> Float -> Direction -> SF (Event Direction) Float
laneSpeed lane speed0 d0 = hold r0 <<< (arr speedEvents)
	where
		r0 = if d0 == DLeft || d0 == DUp then (0 - speed0) else speed0
		speedEvents =
			case lane of
				HLane _ -> mapFilterE (\d -> case d of
					DLeft -> Just (0 - speed0)
					DRight -> Just speed0
					_ -> Nothing)
				VLane _ -> mapFilterE (\d -> case d of
					DUp -> Just (0 - speed0)
					DDown -> Just speed0
					_ -> Nothing)

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

