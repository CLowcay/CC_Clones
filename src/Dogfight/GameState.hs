{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Dogfight.GameState (
	Direction(..),
	Tile(..), allTiles
) where

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
		Intro |
		GameOver |
		HighScore HighScoreState (Event ()) |
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

introMode :: GlobalState -> SF SDLEvents GameOutput
introMode gs = switch doIntro (const$ gameRound gs)

gameOverMode :: GlobalState -> SF SDLEvents GameOutput
gameOverMode = switch doGameOver (const$ introMode gs)

highScoreMode :: GlobalState -> SF SDLEvents GameOutput
highScoreMode gs = dswitch doHighScore (\gs' -> introMode gs')

gameRound :: GlobalState -> SF SDLEvents GameOutput
gameRound gs = switch doGameRound$ \case 
	(RoundCompleted, gs') -> gameRound$ gs' {
		gs_level += 1,
		gs_score += levelBonus,
		gs_scoreC = addCounter levelBonus (gs_scoreC gs)
		gs_levelC = addCounter 1 (gs_levelC gs)
	}
	(RoundDied, gs') -> if isNewHighScore (gs_score gs') (gs_highScores gs')
		then highScoreMode gs' else gameOverMode gs'

doIntro :: SF SDLEvents (GameOutput, Event ())
doIntro = proc e -> do
	eF2 <- sdlKeyPresses (mkKey SDLK_F2) True -< e
	return -< (Intro, eF2 `tag` ())

doGameOver :: SF SDLEvents (GameOutput, Event ())
doGameOver = proc e -> do
	r <- after 3 () -< ()
	return -< (GameOver, r)

doHighScore :: GlobalState -> SF SDLEvents (GameOutput, Event GlobalState)
doHighScore gs = loopPre (gs_highScore gs)$ proc (e, hs0) -> do
	let hs1 = execState (handleEvents highScoreEventHandler (event [] id e)) hs0

	eStart <- now EditingStart -< ()
	let eStop = if isEditing hs1 then Event () else NoEvent
	let eEditing = eStart `lMerge` (eStop `tag` EditingStop)
	
	return -< ((HighScore hs1 eEditing, eStop), hs1)

doGameRound :: GlobalState -> SF SDLEvents (GameOutput, Event (RoundOutcome, GlobalState))
doGameRound gs = proc e -> do
	ship <- spaceship initSpaceship 1.0 <<< directionInput -< e
	returnA -< (GameOutput {
			gr_player = ship,
			gr_enemies = [],
			gr_lasers = [],
			gr_scoreC = gs_scoreC,
			gr_levelC = gs_levelC
		}, NoEvent)

spaceship :: Spaceship -> Float -> SF (Event Direction) Spaceship
spaceship (Spaceship p0 d0) speed = loopPre d0$ proc (e, d) -> do
	nextDirection <- hold d0 -< e
	rec {
		v <- spaceshipV p0 -< (cell, nextDirection)
		p <- integral -< speed *^ v 
		cell <- boundaryCrossing p0 -< p
	}
	let d' = vToDirection d v
	returnA -< (Spaceship p d', d')

spaceshipV :: Vector2 Float -> SF (Event (Int, Int), Direction) (Event (Vector2 Float))
spaceshipV v0 = loopPre v0$  proc ((cell, direction), v) -> do
	let v' = fmap (\(x, y) ->
		if inRange x && inRange y then
			maybe v id$ directionToV direction (odd x) (odd y)
		else zeroVector) cell
	returnA -< (v', event v id v')
	where
		inRange x = x >= 0 && x < gridSize

-- generate an event when we enter a new grid cell
boundaryCrossing :: Vector2 Float -> SF (Vector2 Float) Event (Int, Int)
boundaryCrossing p0 = loopPre p0$ proc (p', p) -> do
	let g0 = toGridCell p
	let g1 = toGridCell p'
	return -< if g0 /= g1 then Event g1 else NoEvent
	where toGridCell v = let (x, y) = vector2XY in (floor x, floor y)

type XOdd = Bool
type YOdd = Bool
directionToV :: Direction -> XOdd -> YOdd -> Maybe (Vector2 Float)
directionToV False _ DUp = Just$ vector2 0 (-1)
directionToV False _ DDown = Just$ vector2 0 1
directionToV _ False DLeft = Just$ vector2 (-1) 0
directionToV _ False DRight = Just$ vector2 0 1
directionToV _ _ _ = Nothing

vToDirection :: Direction -> Vector2 Float -> Direction
vToDirection d0 v = let (x, y) <- vector2XY in
	if x < 0 then DLeft
	else if x > 0 then DRight
	else if y < 0 then DUp
	else if y > 0 then DDown
	else d0

directionInput :: SF SDLEvents Direction
directionInput = proc e -> do
	let keys = mkKey <$> [SDLK_UP, SDLK_DOWN, SDLK_LEFT, SDLK_RIGHT]
	eKeys <- sdlKeys keys True -< e
	return <- keyToDirection.last <$> eKeys

keyToDirection :: Keysym -> Direction
keyToDirection k
	| k `kEqIgn` SDLK_UP = DUp
	| k `kEqIgn` SDLK_DOWN = DDown
	| k `kEqIgn` SDLK_LEFT = DLeft
	| k `kEqIgn` SDLK_RIGHT = DRight
	| otherwise = error "Direction keys incorrectly filtered"

