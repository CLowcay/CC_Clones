module Main where

import Char
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.List
import Debug.Trace
import Graphics.UI.SDL
import IO
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Time

main :: IO ()
main = do
	initSDL
	state0 <- initGameState
	state1 <- loadLevel 1 state0
	time <- getClockTime
	mainLoop time state1
	quit

initSDL :: IO ()
initSDL = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 680 480 32 [HWSurface, DoubleBuf]
	return ()

mainLoop :: ClockTime -> GameState -> IO ()
mainLoop time0 state = do
	renderFrame state

	time1 <- getClockTime
	let delay = clockTimeDiff time0 time1

	events <- pollAllEvents
	let (continue, state') = runState (handleAllEvents events) state
	let state'' = updateGame delay state'
	if continue then mainLoop time1 state'' else return ()

clockTimeDiff :: ClockTime -> ClockTime -> Integer
clockTimeDiff time0 time1 = let timeDiff = diffClockTimes time1 time0 in
	max ((tdPicosec timeDiff) + (toInteger$ tdSec timeDiff) * 10^12) 0

-- Get all available events, returns the backwards, so be sure reverse
-- the list if order of events is important
pollAllEvents :: IO ([Event])
pollAllEvents = fmap reverse pollAllEvents0
	where
		pollAllEvents0 = do
			event <- pollEvent
			case event of
				NoEvent -> return []
				_ -> (liftM (event:)) pollAllEvents

-- Handle a list of events
-- Returns True if the event loop should continue, otherwise False
handleAllEvents :: [Event] -> State GameState Bool
handleAllEvents =
	foldM (\continue -> \event -> do
		continue' <- handleEvent event
		return$ continue && continue'
	) True

handleEvent :: Event -> State GameState Bool
handleEvent Quit = return False
handleEvent (KeyDown sym) = do
	state <- get
	case (symKey sym) of
		SDLK_UP -> do
			put$state {gs_nextDirection = DUp}
			return True
		SDLK_DOWN -> do
			put$state {gs_nextDirection = DDown}
			return True
		SDLK_LEFT -> do
			put$state {gs_nextDirection = DLeft}
			return True
		SDLK_RIGHT -> do
			put$state {gs_nextDirection = DRight}
			return True
		SDLK_ESCAPE -> return False
		_ -> return True
handleEvent _ = return True

frameDelay :: Integer
frameDelay = ((1::Integer) * 10^12) `div` 16

updateGame :: Integer -> GameState -> GameState
updateGame delay state =
	let
		anidiff = (gs_ttFrameSwap state) - delay
		advanceFrames = fromInteger$ if anidiff < 0
			then ((abs anidiff) `div` frameDelay) + 1 else 0
		offset' = (gs_framesToAlignment state) - advanceFrames
		framesToAlignment = if offset' < 0
			then offset' `mod` 16 else offset'
		advanceTiles = if offset' < 0
			then ((abs offset') `div` 16) + 1 else 0
	in
		state {
			gs_framesToAlignment = framesToAlignment,
			gs_ttFrameSwap = if anidiff < 0
				then frameDelay + (anidiff `mod` frameDelay)
				else anidiff,
			gs_snakeTiles = times advanceTiles (advanceTile) (gs_snakeTiles state)
		}
	where
		times 0 f x = x
		times n f x = times (n - 1) f (f x)
		advanceTile (h@((x, y), _):body) = case gs_nextDirection state of
			DUp -> ((x, y - 1), True):h:(Data.List.init body)
			DDown -> ((x, y + 1), True):h:(Data.List.init body)
			DLeft -> ((x - 1, y), True):h:(Data.List.init body)
			DRight -> ((x + 1, y), True):h:(Data.List.init body)

data Animation = Animation {
	surface :: Surface,
	frames :: Array Int Rect
} deriving (Show)

renderAnimation :: Surface -> Int -> Int -> Int -> Animation -> IO ()
renderAnimation dst frame x y animation = do
	blitSurface
		(surface animation) (Just$ (frames animation) ! frame)
		dst (Just$ Rect x y 0 0)
	return ()

renderAnimationLoopV :: Surface -> Int ->
	Int -> Int -> Int -> Animation -> IO ()
renderAnimationLoopV dst frame x y offset animation = do
	let srect = (frames animation) ! frame
	blitSurface
		(surface animation) (Just$ srect {
			rectY = rectY srect + offset,
			rectH = rectH srect - offset})
		dst (Just$ Rect x y 0 0)
	blitSurface
		(surface animation) (Just$ srect {rectH = offset})
		dst (Just$ Rect x (rectH srect - offset) 0 0)
	return ()

renderFrame :: GameState -> IO ()
renderFrame state = do
	let gfx = gs_gfx state
	display <- getVideoSurface
	blitSurface (gs_wallStamp state) Nothing display (Just$ Rect 0 0 0 0)
	renderAnimation display 0 480 0 (gfx Map.! SidePanel)
	renderSnake display 8 state
	Graphics.UI.SDL.flip display
	return ()

renderSnake :: Surface -> Int -> GameState -> IO ()
renderSnake dst frame state = do
	let
		snakeTiles = gs_snakeTiles state
		snakeSprites = zip
			(map (\((x, y), show) -> ((x * 16, y * 16), show)) snakeTiles)
			(inferSnakeSprites (map fst snakeTiles))
		spriteShow = snd.fst
		spritePos = fst.fst
		getSprite = snd
		-- offsets for rendering parts of the head, see renderHeadx
		offset = gs_framesToAlignment state
		offset2 = offset + 3
		offset3 = 3 - ((16 - offset) `mod` 16)
		nHeadSprites = if offset3 < 0 then 2 else 3
		headAni = gfx Map.! (getSprite$head snakeSprites)
		bodySprites = drop nHeadSprites snakeSprites
		tailSprite = last snakeSprites

	-- render head
	let (render1, render2) = case head snakeSprites of
		(_, HeadLeft) -> (renderLeft1, renderLeft2)
		(_, HeadRight) -> (renderRight1, renderRight2)
		(_, HeadUp) -> (renderUp1, renderUp2)
		(_, HeadDown) -> (renderDown1, renderDown2)
	if nHeadSprites == 2
		then do
			renderHead1 (head snakeSprites) offset render1
			renderHead3 (head$ tail snakeSprites) headAni offset2 render1 render2
		else do
			renderHead1 (head snakeSprites) offset render1
			renderHead2 (head$ tail snakeSprites) headAni offset2 render2
			renderHead3 (head$ drop 2 snakeSprites) headAni offset3 render1 render2

	-- render body
	mapM_ (\(((x, y), show), sprite) ->
		renderAnimation dst 0 x y (gfx Map.! sprite)) bodySprites

	-- render tail
	case tailSprite of
		(((x, y), show), SnakeTHL) -> when show $ do
			renderLeft1 (gfx Map.! SnakeTHL) 0 offset x y
			return ()
		(((x, y), show), SnakeTHR) -> when show $ do
			renderRight1 (gfx Map.! SnakeTHL) 0 offset x y
			return ()
		(((x, y), show), SnakeTVU) -> when show $ do
			renderUp1 (gfx Map.! SnakeTHL) 0 offset x y
			return ()
		(((x, y), show), SnakeTVD) -> when show $ do
			renderDown1 (gfx Map.! SnakeTHL) 0 offset x y
			return ()

	return ()
	where
		gfx = gs_gfx state
		cornerSprites = [SnakeUL, SnakeDL, SnakeUR, SnakeDR]
		-- first offset is frames to alignment
		renderHead1 (((x, y), show), sprite) offset render =
			when show $ (render (gfx Map.! sprite) frame offset x y) >> return ()
		-- second offset is frames to alignment + 3
		renderHead2 (((x, y), show), _) headAni offset render =
			when show $ (render headAni frame offset 16 x y) >> return ()
		-- third offset is 3 - ((16 - frames to alignment) mod 16)
		renderHead3 (((x, y), show), sprite) headAni offset render1 render2 =
			when show $ if not$ elem sprite cornerSprites
				then do
					render2 headAni frame offset 16 x y
					render1 (gfx Map.! sprite) 0 (15 - offset) x y
					return ()
				else do
					renderAnimation dst 0 x y (gfx Map.! sprite)
					return ()
		renderLeft1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 0 (16 - offset) 16)
				dst (Just$ Rect (x + offset) y 0 0)
		renderLeft2 src frame offset w x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect (19 - offset) 0 w 16)
				dst (Just$ Rect x y 0 0)
		renderRight1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect (offset + 3) 0 (16 - offset) 16)
				dst (Just$ Rect x y 0 0)
		renderRight2 src frame offset w x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect (offset - w) 0 w 16)
				dst (Just$ Rect (x + 16 - w) y 0 0)
		renderUp1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 0 16 (16 - offset))
				dst (Just$ Rect x (y + offset) 0 0)
		renderUp2 src frame offset h x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 (19 - offset) 16 h)
				dst (Just$ Rect x y 0 0)
		renderDown1 src frame offset x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 (offset + 3) 16 (16 - offset))
				dst (Just$ Rect x y 0 0)
		renderDown2 src frame offset h x y =
			blitSurface (surface src)
				(Just$ adjRect src frame $ Rect 0 (offset - h) 16 h)
				dst (Just$ Rect x (y + 16 - h) 0 0)
		adjRect src frame (Rect x y w h) =
			let (Rect x0 y0 _ _) = ((frames src) ! frame) in
				Rect (x0 + x) (y0 + y) w h

inferSnakeSprites :: [(Int, Int)] -> [Sprite]
inferSnakeSprites tiles =
	(case (diffs$ take 2 tiles) of
		[(0, 0), (1, 0)] -> HeadRight
		[(0, 0), (-1, 0)] -> HeadLeft
		[(0, 0), (0, 1)] -> HeadUp
		[(0, 0), (0, -1)] -> HeadDown
		x -> traceShow x (error "Invalid diffs for head")
	) : (map (\xs -> case (diffs xs) of
		[(0, 0), (1, 0), (1, 0)] -> SnakeH
		[(0, 0), (-1, 0), (-1, 0)] -> SnakeH
		[(0, 0), (0, 1), (0, 1)] -> SnakeV
		[(0, 0), (0, -1), (0, -1)] -> SnakeV
		[(0, 0), (1, 0), (0, 1)] -> SnakeUR
		[(0, 0), (0, -1), (-1, 0)] -> SnakeUR
		[(0, 0), (0, 1), (1, 0)] -> SnakeDL
		[(0, 0), (-1, 0), (0, -1)] -> SnakeDL
		[(0, 0), (-1, 0), (0, 1)] -> SnakeUL
		[(0, 0), (0, -1), (1, 0)] -> SnakeUL
		[(0, 0), (0, 1), (-1, 0)] -> SnakeDR
		[(0, 0), (1, 0), (0, -1)] -> SnakeDR
		x -> traceShow x (error "Invalid diffs for body")
	) (slidingWindow 3 tiles)) ++
	(case (diffs$ drop ((length tiles) - 2) tiles) of
		[(0, 0), (1, 0)] -> [SnakeTHR]
		[(0, 0), (-1, 0)] -> [SnakeTHL]
		[(0, 0), (0, 1)] -> [SnakeTVD]
		[(0, 0), (0, -1)] -> [SnakeTVU]
		x -> traceShow x (error "Invalid diffs for tail")
	)
	where
		slidingWindow n xs
			| length xs < n = []
			| otherwise = (take n xs):(slidingWindow n (tail xs))
		diffs xs = reverse$snd$ foldl
			(\((x0, y0), rs) -> \(x, y) -> ((x, y), (x - x0, y - y0):rs))
			(head xs, []) xs

data Sprite = Digits | Paused | SidePanel |
	HeadDown | HeadLeft | HeadRight | HeadUp |
	SnakeV | SnakeH | SnakeUL | SnakeUR | SnakeDR | SnakeDL |
	SnakeTHL | SnakeTHR | SnakeTVU | SnakeTVD | AppleA | AppleB |
	WallV | WallH | WallUL | WallUR | WallDR | WallDL |
	WallTVU | WallTVD | WallTHL | WallTHR | WallDot |
	WallXR | WallXL | WallXU | WallXD | WallX |
	DoorInH | DoorOutH | DoorInV | DoorOutV
	deriving (Enum, Ord, Eq, Show)
allSprites = enumFrom Digits

loadSprites :: IO (Map.Map Sprite Animation)
loadSprites = do
	sheet1 <- loadBMP$ getAssetPath "gfx/Sheet1.bmp"
	let sheet1Tile = getTile sheet1 16 16
	paused <- loadBMP$ getAssetPath "gfx/Paused.bmp"
	sidePanel <- loadBMP$ getAssetPath "gfx/SidePanel.bmp"
	digits <- loadBMP$ getAssetPath "gfx/Digits.bmp"
	headDown <- loadBMP$ getAssetPath "gfx/HeadDown.bmp"
	headLeft <- loadBMP$ getAssetPath "gfx/HeadLeft.bmp"
	headRight <- loadBMP$ getAssetPath "gfx/HeadRight.bmp"
	headUp <- loadBMP$ getAssetPath "gfx/HeadUp.bmp"

	mapM_ (\surface ->
			setColorKey surface [SrcColorKey] (Pixel 0x00FF00FF))
		[sheet1, paused, sidePanel, digits, headDown, headLeft, headRight, headUp]

	let
		spriteAnimation Digits = Animation {
		 	surface = digits,
		 	frames = listArray (0, 0) [Rect 0 0 20 198]}
		spriteAnimation Paused = Animation {
		 	surface = paused,
		 	frames = listArray (0, 0) [Rect 0 0 234 160]}
		spriteAnimation SidePanel = Animation {
		 	surface = sidePanel,
		 	frames = listArray (0, 0) [Rect 0 0 200 480]}
		spriteAnimation HeadDown = Animation {
		 	surface = headDown,
		 	frames = listArray (0, 15) (map (\n ->
				Rect (n * 16) 0 16 16) [0..15])}
		spriteAnimation HeadLeft = Animation {
		 	surface = headLeft,
		 	frames = listArray (0, 15) (map (\n ->
				Rect 0 (n * 16) 16 16) [0..15])}
		spriteAnimation HeadRight = Animation {
		 	surface = headRight,
		 	frames = listArray (0, 15) (map (\n ->
				Rect 0 (n * 16) 16 16) [0..15])}
		spriteAnimation HeadUp = Animation {
		 	surface = headUp,
		 	frames = listArray (0, 15) (map (\n ->
				Rect (n * 16) 0 16 16)[0..15])}
		spriteAnimation SnakeV = sheet1Tile 0 0
		spriteAnimation SnakeH = sheet1Tile 1 0
		spriteAnimation SnakeUL = sheet1Tile 2 0
		spriteAnimation SnakeUR = sheet1Tile 3 0
		spriteAnimation SnakeDR = sheet1Tile 3 1
		spriteAnimation SnakeDL = sheet1Tile 2 1
		spriteAnimation SnakeTHL = sheet1Tile 0 1
		spriteAnimation SnakeTHR = sheet1Tile 1 1
		spriteAnimation SnakeTVU = sheet1Tile 4 0
		spriteAnimation SnakeTVD = sheet1Tile 4 1
		spriteAnimation AppleA = sheet1Tile 5 0
		spriteAnimation AppleB = sheet1Tile 5 1
		spriteAnimation WallV = sheet1Tile 0 2
		spriteAnimation WallH = sheet1Tile 1 2
		spriteAnimation WallUL = sheet1Tile 2 2
		spriteAnimation WallUR = sheet1Tile 3 2
		spriteAnimation WallDR = sheet1Tile 3 3
		spriteAnimation WallDL = sheet1Tile 2 3
		spriteAnimation WallTVU = sheet1Tile 4 2
		spriteAnimation WallTVD = sheet1Tile 4 3
		spriteAnimation WallTHL = sheet1Tile 0 3
		spriteAnimation WallTHR = sheet1Tile 1 3
		spriteAnimation WallDot = sheet1Tile 5 2
		spriteAnimation WallXR = sheet1Tile 0 4
		spriteAnimation WallXU = sheet1Tile 1 4
		spriteAnimation WallXD = sheet1Tile 2 4
		spriteAnimation WallX = sheet1Tile 3 4
		spriteAnimation WallXL = sheet1Tile 4 4
		spriteAnimation DoorInV = sheet1Tile 5 3
		spriteAnimation DoorOutV = sheet1Tile 5 3
		spriteAnimation DoorInH = sheet1Tile 5 4
		spriteAnimation DoorOutH = sheet1Tile 5 4
	return$ Map.fromList$ map (\sprite ->
		(sprite, spriteAnimation sprite)) allSprites

getTile :: Surface -> Int -> Int -> Int -> Int -> Animation
getTile surface w h x y =
	Animation {
		surface = surface,
		frames = listArray (0, 0) [Rect (x * w) (y * h) w h]
	}

getAssetPath :: String -> String
getAssetPath fileName = ASSET_PREFIX ++ fileName

data Direction = DLeft | DRight | DUp | DDown deriving (Enum, Eq, Ord)
data GameState = GameState {
	gs_gfx :: Map.Map Sprite Animation,
	gs_wallStamp :: Surface,
	gs_nextDirection :: Direction,
	gs_ttFrameSwap :: Integer,
	gs_framesToAlignment :: Int,
	gs_snakeTiles :: [((Int, Int), Bool)],
	gs_foodTiles :: Map.Map (Int, Int) Int,
	gs_wallTiles :: Set.Set (Int, Int),
	gs_in_door :: (Int, Int, Bool),
	gs_out_door :: (Int, Int, Bool),
	gs_score :: Int,
	gs_level :: Int
}

initGameState :: IO (GameState)
initGameState = do
	gfx <- loadSprites
	wallStamp <- (createRGBSurface [HWSurface] 480 480 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000) >>= displayFormat

	return$ GameState {
		gs_gfx = gfx,
		gs_wallStamp = wallStamp,
		gs_nextDirection = DUp,
		gs_ttFrameSwap = 0,
		gs_framesToAlignment = 15,
		gs_snakeTiles = [],
		gs_foodTiles = Map.empty,
		gs_wallTiles = Set.empty,
		gs_in_door = (0, 0, False),
		gs_out_door = (0, 0, False),
		gs_score = 0,
		gs_level = 0
	}

loadLevel :: Int -> GameState -> IO (GameState)
loadLevel level state = do
	-- load the level file
	fileData <- fmap lines$
		readFile$ getAssetPath$ "levels/snake" ++ (show level)
	let startDirection = case trim (head fileData) of
		"D" -> DDown
		"U" -> DUp
		"L" -> DLeft
		"R" -> DRight
		_ -> error$ "Invalid level file for level " ++ (show level)
	let levelMap = concatMap (\(y, line) ->
		concatMap (\(x, c) -> case c of
			'1' -> [((x, y), WallDL)]
			'2' -> [((x, y), WallXU)]
			'3' -> [((x, y), WallDR)]
			'4' -> [((x, y), WallXR)]
			'5' -> [((x, y), WallX)]
			'6' -> [((x, y), WallXL)]
			'7' -> [((x, y), WallUL)]
			'8' -> [((x, y), WallXD)]
			'9' -> [((x, y), WallUR)]
			'-' -> [((x, y), WallH)]
			'/' -> [((x, y), WallV)]
			'>' -> [((x, y), WallTHR)]
			'<' -> [((x, y), WallTHL)]
			'_' -> [((x, y), WallTVU)]
			'~' -> [((x, y), WallTVD)]
			'.' -> [((x, y), WallDot)]
			'a' -> [((x, y), AppleA)]
			'b' -> [((x, y), AppleB)]
			'i' -> [((x, y), DoorInH)]
			'I' -> [((x, y), DoorInV)]
			'e' -> [((x, y), DoorOutH)]
			'E' -> [((x, y), DoorOutV)]
			' ' -> []
			_ -> error$ "Invalid level file for level " ++ (show level)
		)$ zip [0..] (oddElems line))$ zip [0..] (tail fileData)
	
	-- prepare the wallStamp
	let
		wallStamp = gs_wallStamp state
		gfx = gs_gfx state
	fillRect wallStamp (Just$ Rect 0 0 480 480) (Pixel 0x00000000)
	mapM_ (\((x, y), sprite) ->
		renderAnimation wallStamp 0 (x * 16) (y * 16) (gfx Map.! sprite))
		(filter (\(pos, sprite) ->
			not$elem sprite [DoorInH, DoorInV, DoorOutH, DoorOutV]) levelMap)
	
	-- Prepare the doors
	let inDoor = fst$ fromJust$ find
		(\((x, y), sprite) -> sprite == DoorInH || sprite == DoorInV) levelMap
	let outDoor = fst$ fromJust$ find
		(\((x, y), sprite) -> sprite == DoorOutH || sprite == DoorOutV) levelMap
	let snakeTiles = map (\((dx, dy), visible) ->
		(((fst inDoor) + dx, (snd inDoor) + dy), visible)) $
			case startDirection of
				DUp -> [((0, 0), True), ((0, 1), False), ((0, 2), False)]
				DDown -> [((0, 0), True), ((0, -1), False), ((0, -2), False)]
				DLeft -> [((0, 0), True), ((-1, 0), False), ((-2, 0), False)]
				DRight -> [((0, 0), True), ((1, 0), False), ((2, 0), False)]
	
	-- Initialise the state
	return$ state {
		gs_nextDirection = startDirection,
		gs_ttFrameSwap = 0,
		gs_framesToAlignment = 15,
		gs_snakeTiles = snakeTiles,
		gs_foodTiles = Map.fromList$ concatMap (\((x, y), sprite) ->
			case sprite of
				AppleA -> [((x, y), 1)]
				AppleB -> [((x, y), 5)]
				_ -> []) levelMap,
		gs_wallTiles = Set.fromList$ concatMap (\((x, y), sprite) ->
			if (elem sprite [WallV, WallH, WallUL, WallUR, WallDR, WallDL,
				WallTVU, WallTVD, WallTHL, WallTHR, WallDot,
				WallXR, WallXL, WallXU, WallXD, WallX])
				then [(x, y)] else []) levelMap,
		gs_in_door = (fst inDoor, snd inDoor, True),
		gs_out_door = (fst outDoor, snd outDoor, False),
		gs_level = level
	}

-- Trim whitespace from a string
trim :: String -> String
trim = reverse.(dropWhile isSpace).reverse.(dropWhile isSpace)

-- Concatenate two arrays
arrayCat :: Array Int a -> Array Int a -> Array Int a
arrayCat x y =
	array (0, cx + cy) (xl ++ (map (\(i, e) -> (i + cx, e)) yl))
	where
		xl = assocs x
		yl = assocs y
		cx = length xl
		cy = length yl

oddElems :: [a] -> [a]
oddElems [] = []
oddElems [x] = [x]
oddElems (x:_:rest) = x:(oddElems rest)

evenElems :: [a] -> [a]
evenElems [] = []
evenElems [_] = []
evenElems (_:y:rest) = y:(evenElems rest)

