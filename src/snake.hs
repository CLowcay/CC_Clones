module Main where
import Data.Array
import Graphics.UI.SDL
import IO
import Maybe
import qualified Data.Map as Map
import Time

main :: IO ()
main = do
	initSDL
	time <- getClockTime
	mainLoop time
	quit

initSDL :: IO ()
initSDL = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 680 480 32 [HWSurface, DoubleBuf]
	return ()

mainLoop :: ClockTime -> IO ()
mainLoop time0 = do
	time1 <- getClockTime
	let delay = clockTimeDiff time0 time1
	event <- pollEvent
	case event of
		Quit -> return ()
		_ -> mainLoop time1

clockTimeDiff :: ClockTime -> ClockTime -> Integer
clockTimeDiff time0 time1 = let timeDiff = diffClockTimes time1 time0 in
	max ((tdPicosec timeDiff) + (toInteger$ tdSec timeDiff) * 10^12) 0

data Animation = Animation {
	surface :: Surface,
	frames :: Array Int Rect
}

data Sprite = Digits | Paused | SidePanel |
	HeadDown | HeadLeft | HeadRight | HeadUp |
	SnakeV | SnakeH | SnakeUL | SnakeUR | SnakeDR | SnakeDL |
	SnakeTHL | SnakeTHR | SnakeTVU | SnakeTVD | AppleA | AppleB |
	WallV | WallH | WallUL | WallUR | WallDR | WallDL |
	WallTVU | WallTVD | WallTHL | WallTHR | WallDot |
	WallXR | WallXL | WallXU | WallXD | WallX | DoorH | DoorV
	deriving (Enum, Ord, Eq)

loadSprites :: IO (Map.Map Sprite Animation)
loadSprites = do
	sheet1 <- loadBMP$ getAssetPath "Sheet1.bmp"
	let sheet1Tile = getTile sheet1 16 16
	paused <- loadBMP$ getAssetPath "Paused.bmp"
	sidePanel <- loadBMP$ getAssetPath "SidePanel.bmp"
	digits <- loadBMP$ getAssetPath "Digits.bmp"
	headDown <- loadBMP$ getAssetPath "HeadDown.bmp"
	headLeft <- loadBMP$ getAssetPath "HeadLeft.bmp"
	headRight <- loadBMP$ getAssetPath "HeadRight.bmp"
	headUp <- loadBMP$ getAssetPath "HeadUp.bmp"
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
		spriteAnimation DoorV = sheet1Tile 5 3
		spriteAnimation DoorH = sheet1Tile 5 4
	return$ Map.fromList$ map (\sprite ->
		(sprite, spriteAnimation sprite)) (enumFrom Digits)

getTile :: Surface -> Int -> Int -> Int -> Int -> Animation
getTile surface w h x y =
	Animation {
		surface = surface,
		frames = listArray (0, 0) [Rect (x * w) (y * h) w h]
	}

getAssetPath :: String -> String
getAssetPath fileName = ASSET_PREFIX ++ fileName

