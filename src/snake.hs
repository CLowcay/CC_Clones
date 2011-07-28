module Main where

import Common.Counters
import Common.Graphics
import Common.Util
import Control.Monad.State
import Graphics.UI.SDL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Snake.Assets
import Snake.Events
import Snake.GameState
import Snake.Render
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
		gs_holdCount = 0,
		gs_snakeCells = [],
		gs_foodCells = Map.empty,
		gs_wallCells = Set.empty,
		gs_inDoor = (0, 0, False),
		gs_inDoorTile = DoorInV,
		gs_outDoor = (0, 0, False),
		gs_outDoorTile = DoorOutV,
		gs_score = 0, gs_scoreCounter = initCounter (gfx Map.! Digits) 5,
		gs_level = 0, gs_levelCounter = initCounter (gfx Map.! Digits) 2,
		gs_gameOver = False, gs_paused = False
	}

mainLoop :: ClockTime -> GameState -> IO ()
mainLoop time0 state = do
	renderFrame state

	time1 <- getClockTime
	let delay = clockTimeDiff time0 time1

	events <- pollAllEvents
	let (continue, state') = runState (handleAllEvents events) state
	let state'' = updateGame delay state'
	if continue then mainLoop time1 state'' else return ()

