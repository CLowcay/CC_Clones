module Main where

import Common.Counters
import Common.Events
import Common.Graphics
import Common.HighScores
import Common.Util
import Control.Monad
import Control.Monad.State
import Data.Sequence ((|>), (<|))
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.TTF
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Snake.Assets
import Snake.GameState
import Snake.Render
import Time

windowCaption :: String
windowCaption = "Snake"

-- Entry point
main :: IO ()
main = do
	initSDL
	setCaption windowCaption windowCaption
	state0 <- initGameState
	state1 <- loadLevel 0 state0
	time <- getClockTime
	mainLoop time state1
	closeAudio
	Graphics.UI.SDL.quit

-- Library initialisation
initSDL :: IO ()
initSDL = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 680 480 32 [HWSurface, DoubleBuf]
	Graphics.UI.SDL.TTF.init
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
	font <- loadFont
	highScores <- loadHighScoreTable

	introMessage <- renderUTF8Solid font
		"Press F2 to start, Esc to quit" (Color 0 64 255)
	introMessage2 <- renderUTF8Solid font
		"High scores:" (Color 0 64 255)
	highScoreMessage <- renderUTF8Solid font
		"New high score! Enter your name" (Color 0 64 255)

	return$ GameState {
		gs_gfx = gfx, gs_sfx = sfx,
		gs_font = font,
		gs_mode = IntroMode,
		gs_highScores = highScores,
		gs_wallStamp = wallStamp,
		gs_introMessage = introMessage, gs_introMessage2 = introMessage2,
		gs_highScoreMessage = highScoreMessage,
		gs_nextDirections = Seq.empty, gs_currentDirection = DUp,
		gs_ttFrameSwap = 0,
		gs_fastMode = False,
		gs_framesToAlignment = 15,
		gs_holdCount = 0,
		gs_snakeCells = [],
		gs_foodCells = M.empty,
		gs_wallCells = S.empty,
		gs_inDoor = (0, 0, False),
		gs_inDoorTile = DoorInV,
		gs_outDoor = (0, 0, False),
		gs_outDoorTile = DoorOutV,
		gs_score = 0, gs_scoreCounter = initCounter (gfx M.! Digits) 5,
		gs_loadLevel = False,
		gs_sfxEvents = [],
		gs_level = 0, gs_levelCounter = initCounter (gfx M.! Digits) 2,
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
	let (continue, state1) = if (gs_mode state0 == HighScoreMode)
		then let
			(continue', hstate) = runState
				(handleEvents highScoreEventHandler events) (gs_highScores state0)
			in (continue', state0 {gs_highScores = hstate})
		else runState (handleEvents gameEventHandler events) state0

	state1' <- if gs_loadLevel state1
		then loadLevel (gs_level state1) state1 else return state1

	let state2 = updateGame delay state1'
	state2' <- if gs_loadLevel state1
		then loadLevel (gs_level state2) state2 else return state2
	
	let
		isEditing0 = isEditing$ gs_highScores state0
		isEditing1 = isEditing$ gs_highScores state2'

	when (isEditing0 && (not isEditing1)) $ do
		endEditing (gs_highScores state2')
	when ((not isEditing0) && isEditing1) $ do
		startEditing
	
	state3 <- if (isEditing0 && (not isEditing1)) then
		loadLevel 0 (state2' {gs_mode = IntroMode}) else return state2'

	if continue then mainLoop time1 state3 else return ()

-- Handle game events
gameEventHandler :: EventHandler GameState
gameEventHandler Quit = return False
gameEventHandler (KeyDown sym) = do
	state <- get
	let currentDirection = gs_currentDirection state
	case (symKey sym) of
#ifdef CHEATING
		SDLK_KP_PLUS -> do
			put$state {
				gs_level = (gs_level state) + 1,
				gs_loadLevel = True
			}
			return True
#endif
		SDLK_UP -> do
			put$state {gs_nextDirections = (gs_nextDirections state) |> DUp}
			return True
		SDLK_DOWN -> do
			put$state {gs_nextDirections = (gs_nextDirections state) |> DDown}
			return True
		SDLK_LEFT -> do
			put$state {gs_nextDirections = (gs_nextDirections state) |> DLeft}
			return True
		SDLK_RIGHT -> do
			put$state {gs_nextDirections = (gs_nextDirections state) |> DRight}
			return True
		SDLK_ESCAPE -> return False
		SDLK_f -> do
			put$state {
				gs_fastMode =
					(not$ gs_fastMode state) && ((gs_mode state) /= PausedMode)
			}
			return True
		SDLK_F5 -> do
			let
				mode = gs_mode state
			put$state {
				gs_mode = if mode == PausedMode
					then InGameMode
					else if mode == InGameMode
						then PausedMode else mode,
				gs_fastMode = False
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
	forM_ sfxEvents (\(sound, channel) ->
		playChannel (fromEnum channel) (sfx M.! sound) 0)
	where
		sfx = gs_sfx state
		sfxEvents = gs_sfxEvents state

