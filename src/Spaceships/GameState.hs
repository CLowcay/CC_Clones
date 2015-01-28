{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Spaceships.GameState (
	Direction(..),
	GlobalState(..),
	GameOutput(..), HighScoreEditing(..),
	GameRound(..), Spaceship(..), Laser(..),
	Tile(..), allTiles,
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

data Direction = DLeft | DRight | DUp | DDown
	deriving (Enum, Eq, Show)

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

data Spaceship = Spaceship (Vector2 Float) Direction deriving Show
initSpaceship = Spaceship
	(vector2 (10 * (fromIntegral tileSize)) (10 * (fromIntegral tileSize))) DLeft
initEnemy n = Spaceship
	(vector2 ((fromIntegral n) * 2 * (fromIntegral tileSize)) 0) DDown

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
	ship <- spaceship initSpaceship 1.0 <<< directionInput -< e
	returnA -< (Playing$ GameRound {
			gr_player = ship,
			gr_enemies = [],
			gr_lasers = [],
			gr_scoreC = gs_scoreC gs,
			gr_levelC = gs_levelC gs
		}, NoEvent)

spaceship :: Spaceship -> Float -> SF (Event Direction) Spaceship
spaceship (Spaceship p0 d0) speed = loopPre d0$ proc (e, d) -> do
	nextDirection <- hold d0 -< e
	rec
		v <- spaceshipV (directionToV d0) -< (cell, nextDirection)
		p <- integral -< speed *^ v 
		cell <- boundaryCrossing p0 -< p
	let d' = vToDirection d v
	returnA -< (Spaceship p d', d')

spaceshipV :: Vector2 Float -> SF (Event (Int, Int), Direction) (Vector2 Float)
spaceshipV v0 =
	sscan (\v (cell, direction) -> event v (updateDirection v direction) cell) v0
	where
		inRange x = x >= 0 && x < gridSize
		updateDirection v direction (x, y) =
			if inRange x && inRange y
				then maybe v id$ directionToVM (odd x) (odd y) direction 
				else zeroVector

-- generate an event when we enter a new grid cell
boundaryCrossing :: Vector2 Float -> SF (Vector2 Float) (Event (Int, Int))
boundaryCrossing p0 = loopPre p0$ proc (p', p) -> do
	let g0 = toGridCell p
	let g1 = toGridCell p'
	returnA -< (if g0 /= g1 then Event g1 else NoEvent, p')
	where toGridCell v = let (x, y) = vector2XY v in (floor x, floor y)

directionToV :: Direction -> Vector2 Float
directionToV DUp = vector2 0 (-1)
directionToV DDown = vector2 0 1
directionToV DLeft = vector2 (-1) 0
directionToV DRight = vector2 0 1

type XOdd = Bool
type YOdd = Bool

directionToVM :: XOdd -> YOdd -> Direction -> Maybe (Vector2 Float)
directionToVM False _ DUp = Just$ vector2 0 (-1)
directionToVM False _ DDown = Just$ vector2 0 1
directionToVM _ False DLeft = Just$ vector2 (-1) 0
directionToVM _ False DRight = Just$ vector2 0 1
directionToVM _ _ _ = Nothing

vToDirection :: Direction -> Vector2 Float -> Direction
vToDirection d0 v = let (x, y) = vector2XY v in
	if x < 0 then DLeft
	else if x > 0 then DRight
	else if y < 0 then DUp
	else if y > 0 then DDown
	else d0

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

