{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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
import Data.List
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

data GameObject = GameObject Kind LaneControl deriving (Show, Eq)
data LaneControl = LaneControl Lane Float Direction deriving (Show, Eq)
data Kind = Player | Enemy | Laser deriving (Show, Eq)

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
laserWidth = 10 :: Int -- TODO: check this

laneMax :: Float
laneMax = fromIntegral$ ((nLanes + nLanes - 1) * tileSize) - tileSize - 1

lanePosition :: LaneControl -> (Int, Int)
lanePosition (LaneControl lane t _) = case lane of
	HLane n -> (tileSize + (round t), (n * 2 * tileSize) + tileSize)
	VLane n -> ((n * 2 * tileSize) + tileSize, tileSize + (round t))

laneNumbertoPos :: Int -> Float
laneNumbertoPos n = fromIntegral$ n * tileSize * 2

boundingRect :: GameObject -> (Int, Int, Int, Int)
boundingRect (GameObject Laser (lane@(LaneControl _ _ dir))) =
	let (x, y) = lanePosition lane in case dir of
		DLeft ->  (x, y, laserWidth, tileSize)
		DUp ->    (x, y, tileSize, laserWidth)
		DDown ->  (x, y + tileSize - laserWidth, tileSize, laserWidth)
		DRight -> (x + tileSize - laserWidth, y, laserWidth, tileSize)
boundingRect (GameObject _ lane) =
	let (x, y) = lanePosition lane in (x, y, tileSize, tileSize)

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

data FireTheLazer = FireTheLazer deriving (Show, Eq)
data SpaceshipCommand = DirectionCommand Direction | FireCommand deriving (Show, Eq)
type GameObjectController = SF (Event [SpaceshipCommand], [GameObject]) (GameObject, Event FireTheLazer)
type GameObjectControllers = SF (Event [SpaceshipCommand], [GameObject]) [(GameObject, Event FireTheLazer)]

doGameRound :: GlobalState ->
	SF (Event SDLEvents) (GameOutput, Event (RoundOutcome, GlobalState))
doGameRound gs = proc e -> do
	objs <- objects initRound <<< spaceshipInput -< e

	returnA -< (Playing$ GameRound {
			gr_objects = objs,
			gr_scoreC = gs_scoreC gs,
			gr_levelC = gs_levelC gs
		}, NoEvent)

objects :: [GameObject] -> SF (Event [SpaceshipCommand]) [GameObject]
objects objs0 = proc e -> do
	rec
		objs <- (arr$ fmap fst) <<< objectControl objs0 <<< iPre (NoEvent, objs0) -< (e, objs)
	returnA -< objs

objectControl :: [GameObject] -> GameObjectControllers
objectControl objs0 = pSwitchB
	(mkController <$> objs0)
	gameLogic$ \controllers objs ->
		(NoEvent, objs) >-- objectControl objs
	where
		mkController (obj@(GameObject Player _)) = player baseSpeed obj
		mkController (obj@(GameObject Enemy _)) = enemy baseSpeed obj
		mkController (obj@(GameObject Laser _)) = laser (baseSpeed * 1.2) obj

gameLogic :: SF ((Event [SpaceshipCommand], [GameObject]), [(GameObject, Event FireTheLazer)]) (Event [GameObject])
gameLogic = proc (_, objCmds) -> do
	let objs = fst <$> objCmds
	let candidates = case tails objs of
		[] -> []
		(xs:tls) -> (objs `zip`) =<< tls
	let objectCollisions = nub$ foldr (\(x, y) collisions ->
		if isCollision x y then x:y:collisions else collisions) [] candidates
	let allCollisions = objectCollisions ++ (filter isLaserOutOfBounds objs)
	let anyCollisions = not$ null allCollisions
	let survivors = filter (not.(`elem` allCollisions).fst) objCmds
	let (withLasers, anyLasers) = foldr
		(\(obj, e) (rl@(r, _)) -> case e of
			Event _ -> case fireLaser obj of
				Just laserBolt -> (r ++ [laserBolt], True)
				Nothing -> rl
			NoEvent -> rl
		) (fst <$> survivors, False) survivors
	returnA -< if anyLasers || anyCollisions then Event withLasers else NoEvent
	
isCollision :: GameObject -> GameObject -> Bool
-- lasers cannot collide with lasers
isCollision (GameObject Laser _) (GameObject Laser _) = False
isCollision a b =
	let
		(x1, y1, w1, h1) = boundingRect a
		(x2, y2, w2, h2) = boundingRect b
	in x1 + w1 >= x2 && x2 + w2 >= x1 && y1 + h1 >= y2 && y2 + h2 >= y1

isLaserOutOfBounds :: GameObject -> Bool
isLaserOutOfBounds (GameObject Laser (LaneControl _ p _)) = p <= 0 || p >= laneMax
isLaserOutOfBounds _ = False

initRound :: [GameObject]
initRound = initSpaceship : (initEnemy <$> [0..(nLanes - 1)])

fireLaser :: GameObject -> Maybe GameObject
fireLaser (GameObject _ (LaneControl lane0 p0 d0)) =
	let p = p0 + (fromIntegral$
		if d0 == DLeft || d0 == DUp then 0 - tileSize else tileSize)
	in if p <= 0 || p >= laneMax then Nothing
		else Just$ GameObject Laser (LaneControl lane0 p d0)

laser :: Float -> GameObject -> GameObjectController
laser speed0 (GameObject k l0) =
	(arr$ \(l, _) -> (GameObject k l, NoEvent))
		<<< laneMotion speed0 l0 <<< never

enemy :: Float -> GameObject -> GameObjectController
enemy speed0 obj0 = proc (_, objs) -> do
	obj <- spaceshipMotion speed0 obj0 -< NoEvent
	returnA -< (obj, NoEvent)

player :: Float -> GameObject -> GameObjectController
player speed0 obj0 = proc (e, _) -> do
	obj <- spaceshipMotion speed0 obj0 -<
		mapFilterE (safeLast.catMaybes.fmap directionCommand) e
	returnA -< if event False (FireCommand `elem`) e
		then (obj, Event FireTheLazer)
		else (obj, NoEvent)

	where
		directionCommand (DirectionCommand dir) = Just dir
		directionCommand _ = Nothing
		safeLast [] = Nothing
		safeLast xs = Just$ last xs

spaceshipMotion :: Float -> GameObject -> SF (Event Direction) GameObject
spaceshipMotion speed0 (GameObject s l0) = switch
	(arr (first (GameObject s)) <<< laneMotion speed0 l0)
	(\l -> spaceshipMotion speed0 (GameObject s l))

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

spaceshipInput :: SF (Event SDLEvents) (Event [SpaceshipCommand])
spaceshipInput = (arr$ fmap (catMaybes.fmap keyToSpaceshipCommand)) <<< sdlAllKeys

keyToSpaceshipCommand :: Keysym -> Maybe SpaceshipCommand
keyToSpaceshipCommand k = case k of
	Keysym SDLK_UP _ _ -> Just$ DirectionCommand DUp
	Keysym SDLK_DOWN _ _ -> Just$ DirectionCommand DDown
	Keysym SDLK_LEFT _ _ -> Just$ DirectionCommand DLeft
	Keysym SDLK_RIGHT _ _ -> Just$ DirectionCommand DRight
	Keysym SDLK_SPACE _ _ -> Just FireCommand
	_ -> Nothing

