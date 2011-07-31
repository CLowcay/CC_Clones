module Main where

import Common.Counters
import Common.Events
import Common.Graphics
import Common.HighScores
import Common.Util
import Control.Monad
import Control.Monad.State
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import qualified Data.Map as Map
import qualified Data.Set as Set
import Snake.Assets
import Snake.GameState
import Snake.Render
import Time

-- Entry point
main :: IO ()
main = do
	initSDL
	state0 <- initGameState
	state1 <- loadLevel 1 state0
	time <- getClockTime
	mainLoop time state1
	closeAudio
	quit

-- Library initialisation
initSDL :: IO ()
initSDL = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 680 480 32 [HWSurface, DoubleBuf]
	openAudio defaultFrequency AudioS16Sys 2 4096
	allocateChannels (fromEnum ChannelCount)
	return ()

-- The initial GameState
initGameState :: IO (GameState)
initGameState = do
	gfx <- loadSprites
	wallStamp <- (createRGBSurface [HWSurface] 480 480 32
		0x000000FF 0x0000FF00 0x00FF0000 0xFF000000) >>= displayFormat
	sfx <- loadSounds
	highScores <- loadHighScoreTable

	return$ GameState {
		gs_gfx = gfx, gs_sfx = sfx,
		gs_mode = InGameMode,
		gs_highScores = highScores,
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
		gs_eatingApples = []
	}

-- The main game loop
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

-- Handle game events
gameEventHandler :: EventHandler GameState
gameEventHandler Quit = return False
gameEventHandler (KeyDown sym) = do
	state <- get
	let currentDirection = gs_currentDirection state
	case (symKey sym) of
		SDLK_UP -> do
			put$state {
				gs_nextDirection = if currentDirection == DDown
					then gs_nextDirection state else DUp
			}
			return True
		SDLK_DOWN -> do
			put$state {
				gs_nextDirection = if currentDirection == DUp
					then gs_nextDirection state else DDown
			}
			return True
		SDLK_LEFT -> do
			put$state {
				gs_nextDirection = if currentDirection == DRight
					then gs_nextDirection state else DLeft
			}
			return True
		SDLK_RIGHT -> do
			put$state {
				gs_nextDirection = if currentDirection == DLeft
					then gs_nextDirection state else DRight
			}
			return True
		SDLK_ESCAPE -> return False
		SDLK_f -> do
			put$state {gs_fastMode = not$ gs_fastMode state}
			return True
		SDLK_F5 -> do
			let
				mode = gs_mode state
			put$state {
				gs_mode = if mode == PausedMode
					then InGameMode
					else if mode == InGameMode
						then PausedMode else mode
			}
			return True
		SDLK_F2 -> do
			put$state {
				gs_mode = InGameMode,
				gs_levelCounter = resetCounter 0 (gs_levelCounter state),
				gs_scoreCounter = resetCounter 0 (gs_scoreCounter state),
				gs_level = 1, gs_loadLevel = True, gs_score = 0
			}
			return True
		_ -> return True
gameEventHandler _ = return True

-- Play currently scheduled sound effects
playSounds :: GameState -> IO ()
playSounds state =
	mapM_ (\(sound, channel) ->
		playChannel (fromEnum channel) (sfx Map.! sound) 0) sfxEvents
	where
		sfx = gs_sfx state
		sfxEvents = gs_sfxEvents state

