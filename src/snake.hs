module Main where

import Graphics.UI.SDL
import IO
import Maybe
import Time

main :: IO ()
main = do
	initSDL

initSDL :: IO ()
initSDL = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 680 480 32 [HWSurface, DoubleBuf]
	time <- getClockTime
	mainLoop time
	quit

mainLoop :: ClockTime -> IO ()
mainLoop time0 = do
	time <- getClockTime
	let
		timeDiff = diffClockTimes time time0
		delay = max ((tdPicosec timeDiff) + (toInteger$ tdSec timeDiff) * 10^12) 0
	event <- pollEvent
	case event of
		Quit -> return ()
		_ -> mainLoop time

