module Main where

import Common.Counters
import Common.Events
import Common.Graphics
import Common.Util
import Control.Monad
import Control.Monad.State
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
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
	closeAudio
	quit

initSDL :: IO ()
initSDL = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 680 480 32 [HWSurface, DoubleBuf]
	openAudio defaultFrequency AudioS16Sys 2 4096
	allocateChannels (fromEnum ChannelCount)
	return ()

initGameState :: IO (GameState)
initGameState = do
	gfx <- loadSprites
	wallStamp <- (createRGBSurface [HWSurface] 480 480 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000) >>= displayFormat
	sfx <- loadSounds

	return$ GameState {
		gs_gfx = gfx, gs_sfx = sfx,
		gs_wallStamp = wallStamp,
		gs_nextDirection = DUp, gs_currentDirection = DUp,
		gs_ttFrameSwap = 0,
		gs_fastMode = False,
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
		gs_loadLevel = False,
		gs_sfxEvents = [],
		gs_level = 0, gs_levelCounter = initCounter (gfx Map.! Digits) 2,
		gs_gameOver = False, gs_paused = False,
		gs_eatingApples = []
	}

mainLoop :: ClockTime -> GameState -> IO ()
mainLoop time0 state0 = do
	playSounds state0
	renderFrame state0

	time1 <- getClockTime
	let delay = clockTimeDiff time0 time1

	events <- pollEvents
	let (continue, state1) =
		runState (handleEvents gameEventHandler events) state0
	state1' <- if gs_loadLevel state1
		then loadLevel (gs_level state1) state1 else return state1
	let state2 = updateGame delay state1'
	state2' <- if gs_loadLevel state1
		then loadLevel (gs_level state2) state2 else return state2
	if continue then mainLoop time1 state2' else return ()

playSounds :: GameState -> IO ()
playSounds state =
	mapM_ (\(sound, channel) ->
		playChannel (fromEnum channel) (sfx Map.! sound) 0) sfxEvents
	where
		sfx = gs_sfx state
		sfxEvents = gs_sfxEvents state

