{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Spaceships.GameState (
	Direction(..),
	GlobalState(..),
	GameOutput(..), FullGameOutput(..), HighScoreEditing(..),
	GameRound(..),
	LaneControl(..), GameObject(..), Kind(..),
	Tile(..), allTiles,
	lanePosition,
	introMode,
	addCounters
) where

import Common.Counters
import Common.Events
import Common.HighScores
import Control.Applicative
import Control.Monad.State (execState)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import FRP.Counters
import FRP.Events
import FRP.Yampa
import FRP.Yampa.Vector2
import Graphics.UI.SDL hiding (Event, NoEvent)
import qualified Graphics.UI.SDL as SDL
import System.IO.Unsafe
import System.Random

import Debug.Trace

-- Exposed types
-- ----------------------------------------------------------------------------

data Direction = DLeft | DUp | DDown | DRight deriving (Eq, Ord, Show)
data Lane = HLane !Int | VLane !Int deriving (Eq, Ord, Show)

data GameOutput =
		Intro GlobalState |
		GameOver GlobalState |
		HighScore GlobalState HighScoreState (Event HighScoreEditing) |
		Playing GameRound
	deriving Show

data FullGameOutput = FullGameOutput {
	go_out :: !GameOutput,
	go_levelC :: CounterState,
	go_scoreC :: CounterState} deriving Show

data RoundOutcome = RoundDied | RoundCompleted deriving Show

data HighScoreEditing = EditingStart | EditingStop deriving Show

data GlobalState = GlobalState {
	gs_score :: !Int,
	gs_level :: !Int,
	gs_highScores :: HighScoreState
} deriving Show

data GameObject = GameObject !Kind !LaneControl deriving (Show, Eq)
data LaneControl = LaneControl !Lane !Float !Direction deriving (Show, Eq)
data Kind = Player | Enemy Int | Laser deriving (Show, Eq)

initSpaceship = GameObject Player (LaneControl (HLane 9) laneMax DLeft)
initEnemy n = GameObject (Enemy n) (LaneControl (VLane n) 0 DDown)

data GameRound = GameRound {
	gr_objects :: ![GameObject]
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
laserStrikePoints = 2 :: Int
forcedCollisionPoints = 5 :: Int
levelBonus = 9 :: Int
nLanes = 10 :: Int
baseSpeed = 8 :: Time
speedRamp = 1.1 :: Time
laserWidth = 10 :: Int -- TODO: check this
maxObjects = 1000

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

resetGlobalState :: GlobalState -> GlobalState
resetGlobalState gs = gs {gs_level = 1, gs_score = 0}

-- Top level SFs
-- ----------------------------------------------------------------------------

data SLCounterCommand =
	ScoreUpdate !CounterCommand |
	LevelUpdate !CounterCommand |
	ScoreLevelUpdate !CounterCommand !CounterCommand deriving Show

instance Monoid SLCounterCommand where
	mempty = ScoreUpdate$ CounterAdd 0
	(ScoreUpdate x) `mappend` (ScoreUpdate y) = ScoreUpdate (x <> y)
	(LevelUpdate x) `mappend` (LevelUpdate y) = LevelUpdate (x <> y)
	(ScoreLevelUpdate x1 x2) `mappend` (ScoreLevelUpdate y1 y2) = ScoreLevelUpdate (x1 <> y1) (x2 <> y2)
	(ScoreUpdate x) `mappend`          (LevelUpdate y) = ScoreLevelUpdate x y
	(ScoreUpdate x) `mappend` (ScoreLevelUpdate y1 y2) = ScoreLevelUpdate (x <> y1) y2
	(LevelUpdate x) `mappend` (ScoreLevelUpdate y1 y2) = ScoreLevelUpdate y1 (x <> y2)
	(LevelUpdate y)          `mappend` (ScoreUpdate x) = ScoreLevelUpdate x y
	(ScoreLevelUpdate y1 y2) `mappend` (ScoreUpdate x) = ScoreLevelUpdate (x <> y1) y2
	(ScoreLevelUpdate y1 y2) `mappend` (LevelUpdate x) = ScoreLevelUpdate y1 (x <> y2)

trackScores :: GlobalState -> SF (Event SLCounterCommand) GlobalState
trackScores = accumHoldBy$ \gs slCmd -> case slCmd of
	ScoreUpdate cmd -> gs {gs_score = applyCounterCommand cmd (gs_score gs)}
	LevelUpdate cmd -> gs {gs_level = applyCounterCommand cmd (gs_level gs)}
	ScoreLevelUpdate cmds cmdl -> gs {
		gs_score = applyCounterCommand cmds (gs_score gs),
		gs_level = applyCounterCommand cmdl (gs_level gs)}

addCounters :: CounterState -> CounterState ->
	SF (Event SDLEvents) (GameOutput, Event SLCounterCommand) ->
	SF (Event SDLEvents) FullGameOutput
addCounters scoreC levelC sf = sf >>> proc (go, cmd) -> do
	let (scmd, lcmd) = case cmd of
		Event (ScoreUpdate s) -> (Event s, NoEvent)
		Event (LevelUpdate l) -> (NoEvent, Event l)
		Event (ScoreLevelUpdate s l) -> (Event s, Event l)
		NoEvent -> (NoEvent, NoEvent)
	score <- counterControl (resetCounter 0 scoreC) -< scmd
	level <- counterControl (resetCounter 1 levelC) -< lcmd
	returnA -< FullGameOutput {go_out = go, go_levelC = level, go_scoreC = score}

introMode :: GlobalState -> SF (Event SDLEvents) (GameOutput, Event SLCounterCommand)
introMode gs = switch (doIntro gs) (const$ gameRound (resetGlobalState gs))

gameOverMode :: GlobalState -> SF (Event SDLEvents) (GameOutput, Event SLCounterCommand)
gameOverMode gs = switch (doGameOver gs) (const$ introMode gs)

highScoreMode :: GlobalState -> SF (Event SDLEvents) (GameOutput, Event SLCounterCommand)
highScoreMode gs = dSwitch (doHighScore gs) (\gs' -> introMode gs')

gameRound :: GlobalState -> SF (Event SDLEvents) (GameOutput, Event SLCounterCommand)
gameRound gs = dSwitch (doGameRound gs speed)$ \case 
	(RoundCompleted, gs') -> gameRound gs'
	(RoundDied, gs') -> if isNewHighScore (gs_score gs') (gs_highScores gs')
		then highScoreMode gs' else gameOverMode gs'
	where speed = baseSpeed * (speedRamp ** (fromIntegral$ gs_level gs))

-- Mode SFs
-- ----------------------------------------------------------------------------

-- Like "now", but delays the event.
-- Used to generate an event that will be seen after a delayed switch
nowE :: b -> SF a (Event b)
nowE x = takeEvents 2 <<< constant (Event x)

doIntro :: GlobalState -> SF (Event SDLEvents) ((GameOutput, Event SLCounterCommand), Event ())
doIntro gs = proc e -> do
	eF2 <- sdlKeyPresses (mkKey SDLK_F2) True -< e
	resetCounters <-
		nowE (ScoreLevelUpdate (CounterReset 0) (CounterReset 1)) -< ()
	returnA -< ((Intro gs, resetCounters), eF2 `tag` ())

doGameOver :: GlobalState -> SF (Event SDLEvents) ((GameOutput, Event SLCounterCommand), Event ())
doGameOver gs = proc e -> do
	r <- after 5 () -< ()
	returnA -< ((GameOver gs, NoEvent), r)

doHighScore :: GlobalState ->
	SF (Event SDLEvents) ((GameOutput, Event SLCounterCommand), Event GlobalState)
doHighScore gs =
	let hs0 = insertHighScore (gs_score gs) (gs_highScores gs)
	in loopPre hs0$ proc (e, hs0) -> do
		let hs1 = execState (handleEvents highScoreEventHandler (event [] id e)) hs0

		eStart <- nowE EditingStart -< ()
		let eStop = if not$ isEditing hs1
			then Event (gs {gs_highScores = hs1})
			else NoEvent
		let eEditing = eStart `lMerge` (eStop `tag` EditingStop)

		returnA -< (((HighScore gs hs1 eEditing, NoEvent), eStop), hs1)

-- Game SFs
-- ----------------------------------------------------------------------------

data GameObjectContainer a = GameObjectContainer {
	getCounterCommand :: !(Event SLCounterCommand),
	getObjects :: [a]
} deriving Functor

setCounterCommand :: Event SLCounterCommand -> GameObjectContainer a -> GameObjectContainer a
setCounterCommand x c = c {getCounterCommand = x}

data FireTheLazer = FireTheLazer !GameObject deriving (Show, Eq)
data SpaceshipCommand = DirectionCommand !Direction | FireCommand deriving (Show, Eq)
type GameObjectController = SF
	(Event [SpaceshipCommand], [GameObject])
	(GameObject, Event FireTheLazer)
type GameObjectControllers = SF
	(Event [SpaceshipCommand], [GameObject])
	(GameObjectContainer (GameObject, Event FireTheLazer))

isEnemy :: GameObject -> Bool
isEnemy (GameObject (Enemy _) _) = True
isEnemy _ = False

doGameRound :: GlobalState -> Time ->
	SF (Event SDLEvents) ((GameOutput, Event SLCounterCommand), Event (RoundOutcome, GlobalState))
doGameRound gs0 speed = proc e -> do
	GameObjectContainer counterCmd0 objs <-
		objects speed initRound <<< spaceshipInput -< e

	let roundCompleted = not.any isEnemy$ objs

	let counterCmd1 = if roundCompleted
		then Event$ ScoreLevelUpdate (CounterAdd levelBonus) (CounterAdd 1)
		else NoEvent
	
	let counterCmd = mergeBy (<>) counterCmd0 counterCmd1

	gs <- trackScores gs0 -< counterCmd

	let e =
		if not.any (\(GameObject p _) -> p == Player)$ objs
			then Event (RoundDied, gs)
		else if roundCompleted then Event (RoundCompleted, gs)
		else NoEvent

	returnA -< ((Playing$ GameRound {gr_objects = objs}, counterCmd), e)

objects :: Time -> [GameObject] ->
	SF (Event [SpaceshipCommand]) (GameObjectContainer GameObject)
objects speed objs0 = proc e -> do
	rec
		r@(GameObjectContainer _ objs) <- (arr$ fmap fst)
			<<< objectControl speed (GameObjectContainer NoEvent (mkController speed <$> objs0))
			<<< iPre (NoEvent, objs0) -< (e, objs)
	returnA -< r

mkController :: Time -> GameObject -> GameObjectController
mkController speed (obj@(GameObject Player _)) = player speed obj
mkController speed (obj@(GameObject (Enemy _) _)) = enemy speed obj
mkController speed (obj@(GameObject Laser _)) = laser (speed * 1.2) obj

infixr 0 -:>
(-:>) :: b -> SF a b -> SF a b
b -:> sf = switch ((b, NoEvent) --> constant (b, Event ())) (\_ -> sf)

objectControl :: Time ->
	GameObjectContainer GameObjectController ->
	GameObjectControllers
objectControl speed0 objs0 = pSwitchB
	objs0
	gameLogic$ \controllers (containerOps, lasers, objs, counterCmd) ->
		GameObjectContainer counterCmd ((, NoEvent) <$> objs) -:>
			objectControl speed0$ updateControllers controllers containerOps lasers
		
	where
		updateControllers (GameObjectContainer e controllers) containerOps lasers =
			GameObjectContainer e
				(applyContainerOps controllers containerOps ++ (mkController speed0 <$> lasers))

data ObjectContainerOp = KeepIt | ChuckIt deriving (Show, Eq)
applyContainerOps :: [a] -> [ObjectContainerOp] -> [a]
applyContainerOps xs cmds = fst <$> filter ((/= ChuckIt).snd) (xs `zip` cmds)

gameLogic :: SF
	((Event [SpaceshipCommand], [GameObject]), GameObjectContainer (GameObject, Event FireTheLazer))
	(Event ([ObjectContainerOp], [GameObject], [GameObject], Event SLCounterCommand))
gameLogic = proc (_, GameObjectContainer _ objCmds) -> do
	let objs = fst <$> objCmds
	let candidates = case tails objs of
		[] -> []
		(xs:tls) -> (objs `zip`) =<< tls
	let (objectCollisions, score) = foldr (\(x, y) (collisions, s) ->
		if isCollision x y
			then (x:y:collisions, s + scoreCollision x y)
			else (collisions, s)) ([], 0) candidates
	let allCollisions = (nub objectCollisions) ++ (filter isLaserOutOfBounds objs)
	let anyCollisions = not$ null allCollisions

	let containerOps = fmap (\(x, _) ->
		if x `elem` allCollisions then ChuckIt else KeepIt) objCmds
	
	let newLaserEvents =
		catEvents . fmap snd . filter ((/= ChuckIt).fst)$
			containerOps `zip` (snd <$> objCmds)
	
	let newLasers = case newLaserEvents of
		NoEvent -> []
		Event xs -> fmap (\(FireTheLazer l) -> l) xs

	let counterCmd = if score > 0 then Event$ ScoreUpdate$ CounterAdd score else NoEvent
	returnA -< if anyCollisions || not (null newLasers)
		then Event (containerOps, newLasers, objs, counterCmd) else NoEvent
	
isCollision :: GameObject -> GameObject -> Bool
-- lasers cannot collide with lasers
isCollision (GameObject Laser _) (GameObject Laser _) = False
-- enemies cannot collide with themselves
isCollision a@(GameObject (Enemy x) _) b@(GameObject (Enemy y) _) =
	if x == y then False else isCollision0 a b
isCollision a b = isCollision0 a b
isCollision0 a b =
	let
		(x1, y1, w1, h1) = boundingRect a
		(x2, y2, w2, h2) = boundingRect b
	in x1 + w1 >= x2 && x2 + w2 >= x1 && y1 + h1 >= y2 && y2 + h2 >= y1

scoreCollision :: GameObject -> GameObject -> Int
scoreCollision (GameObject Laser _) (GameObject (Enemy _) _) = 2
scoreCollision (GameObject (Enemy _) _) (GameObject Laser _) = 2
scoreCollision _ _ = 0

isLaserOutOfBounds :: GameObject -> Bool
isLaserOutOfBounds (GameObject Laser (LaneControl _ p _)) = p <= 0 || p >= laneMax
isLaserOutOfBounds _ = False

initRound :: [GameObject]
initRound = initSpaceship : (initEnemy <$> [0..(nLanes - 1)])

fireLaser :: GameObject -> Event FireTheLazer
fireLaser (GameObject _ (LaneControl lane0 p0 d0)) =
	let p = p0 + (fromIntegral$
		if d0 == DLeft || d0 == DUp then 0 - tileSize else tileSize)
	in if p <= 0 || p >= laneMax then NoEvent
		else Event$ FireTheLazer$ GameObject Laser (LaneControl lane0 p d0)

laser :: Time -> GameObject -> GameObjectController
laser speed0 (GameObject k l0) =
	(arr$ \(l, _) -> (GameObject k l, NoEvent))
		<<< laneMotion speed0 l0 <<< never

randomTurnInterval = baseSpeed * 3
baseSeekInterval = baseSpeed * 4
baseLaserInterval = 60

enemy :: Time -> GameObject -> GameObjectController
enemy speed0 obj0 = proc (_, objs) -> do
	e <- after firstTurnTime DUp -< ()

	rec 
		rTurn <- occasionally stdGen randomTurnInterval () -< ()
		sTurn <- occasionally stdGen seekInterval () -< ()
		mayFireLaser <- occasionally stdGen laserInterval () -< ()

		obj <- spaceshipMotion speed0 obj0 <<< iPre NoEvent -<
			mergeEvents [avoidCollision, e, randomTurn, seekPlayer]
		
		let GameObject (Enemy en) (LaneControl lane p d) = obj
		let ignoreLasers = filter (\(GameObject x _) ->
			x /= Laser && x /= Player) objs
		let validTurns = filter (safeToTurn ignoreLasers obj)
			[flipDirection d, turnDirection d, flipDirection$ turnDirection d, d]

		let avoidCollision = if safeToGo ignoreLasers obj d then NoEvent
				else case validTurns of
					[] -> NoEvent
					x:_ -> Event x

		let randomTurn = if not$ isEvent rTurn then NoEvent
			else case validTurns of
				[] -> NoEvent
				[x] -> NoEvent
				x:y:_ -> Event y

		let playerDirection = directionToPlayer obj (findPlayer objs)
		let seekPlayer =
			if isEvent sTurn && safeToTurn ignoreLasers obj playerDirection
			then Event playerDirection else NoEvent

	let doFireLaser =
		if isEvent mayFireLaser && losToPlayer obj objs then
			let Event (FireTheLazer l) = fireLaser obj
			in if not$ any (== l) objs then Event$ FireTheLazer l else NoEvent
		else NoEvent

	returnA -< (obj, doFireLaser)
	where
		seekInterval =  (baseSeekInterval * baseSpeed) / speed0
		laserInterval = baseLaserInterval * (baseSpeed / (speed0 * 1.2))
		firstTurnTime = realToFrac$
			fromIntegral (laneNumber obj0 * tileSize * 2) / speed0
		laneNumber (GameObject _ (LaneControl (HLane x) _ _)) = x
		laneNumber (GameObject _ (LaneControl (VLane x) _ _)) = x
		flipDirection DUp = DDown
		flipDirection DDown = DUp
		flipDirection DLeft = DRight
		flipDirection DRight = DLeft
		turnDirection DUp = DLeft
		turnDirection DLeft = DDown
		turnDirection DDown = DRight
		turnDirection DRight = DUp

		nextPos (GameObject t (LaneControl (HLane x) p d)) DUp = GameObject t (LaneControl (HLane$ x - 1) p DUp)
		nextPos (GameObject t (LaneControl (VLane x) p d)) DUp = GameObject t (LaneControl (VLane x) (p - fTileSize) DUp)
		nextPos (GameObject t (LaneControl (HLane x) p d)) DDown = GameObject t (LaneControl (HLane$ x + 1) p DDown)
		nextPos (GameObject t (LaneControl (VLane x) p d)) DDown = GameObject t (LaneControl (VLane x) (p + fTileSize) DDown)
		nextPos (GameObject t (LaneControl (VLane x) p d)) DLeft = GameObject t (LaneControl (VLane$ x - 1) p DLeft)
		nextPos (GameObject t (LaneControl (HLane x) p d)) DLeft = GameObject t (LaneControl (HLane x) (p - fTileSize) DLeft)
		nextPos (GameObject t (LaneControl (VLane x) p d)) DRight = GameObject t (LaneControl (VLane$ x + 1) p DRight)
		nextPos (GameObject t (LaneControl (HLane x) p d)) DRight = GameObject t (LaneControl (HLane x) (p + fTileSize) DRight)

		safeToGo objs obj d = inRange obj && noCollision (nextPos obj d) objs
		safeToTurn objs obj d = let obj1 = nextPos obj d in inRange obj1 && noCollision obj1 objs
		noCollision obj objs = not$ any (isCollision obj) objs

		fTileSize = (fromIntegral tileSize)
		inRange g@(GameObject _ (LaneControl _ p d)) =
			let ln = laneNumber g in ln >= 0 && ln < nLanes &&
				if d == DUp || d == DLeft then p > 0 else p < laneMax
		stdGen = unsafePerformIO getStdGen

directionToPlayer :: GameObject -> GameObject -> Direction
directionToPlayer (GameObject _ e) (GameObject _ p) =
	let 
		(x1, y1) = lanePosition e
		(x2, y2) = lanePosition p
		dx = x2 - x1
		dy = y2 - y1
	in
		if dx > dy then if dx >= 0 then DRight else DLeft
		else if dy >= 0 then DDown else DUp

findPlayer :: [GameObject] -> GameObject
findPlayer ((p@(GameObject Player _)):_) = p
findPlayer (_:xs) = findPlayer xs 
findPlayer _ = error "This cannot happen, because there is always a player object"

losToPlayer :: GameObject -> [GameObject] -> Bool
losToPlayer (GameObject _ (LaneControl lane0 p0 d0)) objs = 
	case reverse$ sortBy (comparing distance)$ filter isInLane objs of
		(GameObject Player _):_ -> True
		_ -> False
	where
		isInLane (GameObject _ (LaneControl l p _)) = l == lane0 || switchLane l p == lane0
		switchLane (HLane _) p = VLane (floor$ p / 52)
		switchLane (VLane _) p = HLane (floor$ p / 52)
		laneP (HLane x) = fromIntegral$ x * 52
		laneP (VLane x) = fromIntegral$ x * 52
		distance (GameObject _ (LaneControl l p d)) =
			if d0 == DLeft || d0 == DUp then
				if l == lane0 then p0 - p else p0 - (laneP l)
			else
				if l == lane0 then p - p0 else (laneP l) - p0

player :: Time -> GameObject -> GameObjectController
player speed0 obj0 = proc (e, _) -> do
	obj <- spaceshipMotion speed0 obj0 -<
		mapFilterE (safeLast.catMaybes.fmap directionCommand) e
	returnA -< if event False (FireCommand `elem`) e
		then (obj, fireLaser obj)
		else (obj, NoEvent)

	where
		directionCommand (DirectionCommand dir) = Just dir
		directionCommand _ = Nothing
		safeLast [] = Nothing
		safeLast xs = Just$ last xs

spaceshipMotion :: Time -> GameObject -> SF (Event Direction) GameObject
spaceshipMotion speed0 (GameObject s l0) = switch
	(arr (first (GameObject s)) <<< laneMotion speed0 l0)
	(\l -> spaceshipMotion speed0 (GameObject s l))

laneMotion :: Time -> LaneControl ->
	SF (Event Direction) (LaneControl, Event LaneControl)
laneMotion speed0 (LaneControl lane0 p0 d0) = proc e -> do
	nextDirection <- hold d0 -< e
	direction <- laneDirection lane0 d0 -< e
	let speed = realToFrac$
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

