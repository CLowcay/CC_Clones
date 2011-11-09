{-
CC_Clones - Classic games reimplemented
© Callum Lowcay 2006-2011

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Common.Counters
import Common.Events
import Common.Graphics
import Common.HighScores
import Common.Util
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State hiding (liftIO)
import Control.Monad.Trans.Reader
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.TTF
import qualified Common.Queue as Q
import qualified Data.Map as M
import qualified Data.Set as S
import Snake.Assets
import Snake.GameState
import Snake.Render

windowCaption :: String
windowCaption = "Snake"

-- Entry point
main :: IO ()
main = do
	initSDL
	setCaption windowCaption windowCaption
	assets <- loadAssets
	state0 <- runReaderT initGameState assets
	state1 <- runReaderT (Snake.Assets.loadLevel 0 state0) assets
	time <- fmap fromIntegral$ getTicks
	runReaderT (mainLoop time state1) assets
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
initGameState :: ReaderT Assets IO GameState
initGameState = do
	Assets {..} <- ask
	liftIO$ do
		wallStamp <- createRGBSurface [HWSurface] 480 480 32
			0x000000FF 0x0000FF00 0x00FF0000 0xFF000000 >>= displayFormat
		highScores <- loadHighScoreTable

		introMessage <- renderUTF8Solid font
			"Press F2 to start, Esc to quit" (Color 0 64 255)
		introMessage2 <- renderUTF8Solid font
			"High scores:" (Color 0 64 255)
		highScoreMessage <- renderUTF8Solid font
			"New high score! Enter your name" (Color 0 64 255)

		return GameState {
			mode = IntroMode,
			highScores = highScores,
			wallStamp = wallStamp,
			introMessage = introMessage, introMessage2 = introMessage2,
			highScoreMessage = highScoreMessage,
			nextDirections = Q.empty, currentDirection = DUp,
			ttFrameSwap = 0,
			fastMode = False,
			framesToAlignment = 15,
			holdCount = 0,
			snakeCells = [],
			foodCells = M.empty,
			wallCells = S.empty,
			inDoor = (0, 0, False),
			inDoorTile = DoorInV,
			outDoor = (0, 0, False),
			outDoorTile = DoorOutV,
			score = 0, scoreCounter = initCounter (gfx M.! Digits) 5,
			loadLevel = False,
			sfxEvents = [],
			level = 0, levelCounter = initCounter (gfx M.! Digits) 2,
			eatingApples = []
		}

-- The main game loop
mainLoop :: Int -> GameState -> ReaderT Assets IO ()
mainLoop time0 state0 = do
	playSounds state0
	renderFrame state0

	time1 <- fmap fromIntegral$ liftIO$getTicks
	let delay = time1 - time0

	-- Run event handler, which may alter the game state
	events <- liftIO$pollEvents
	let (continue, state1) = if mode state0 == HighScoreMode
		then let
			(continue', hstate) = runState
				(handleEvents highScoreEventHandler events) (highScores state0)
			in (continue', state0 {highScores = hstate})
		else runState (handleEvents gameEventHandler events) state0
	state1' <- maybeLoadLevel state1

	-- Update the game state based on the current delay
	let state2 = updateGame delay state1'
	state2' <- maybeLoadLevel state2
	
	-- Manage high score editing
	let
		isEditing0 = isEditing$ highScores state0
		isEditing1 = isEditing$ highScores state2'
	when (isEditing0 && not isEditing1) $
		liftIO$endEditing (highScores state2')
	when (not isEditing0 && isEditing1) $
		liftIO$startEditing
	
	-- If high score editing is over, go back to IntroMode
	state3 <- if isEditing0 && not isEditing1 then
		Snake.Assets.loadLevel 0 (state2' {mode = IntroMode}) else return state2'

	when continue $ mainLoop time1 state3

-- Load a new level if required
maybeLoadLevel :: GameState -> ReaderT Assets IO GameState
maybeLoadLevel state = if Snake.GameState.loadLevel state then
	Snake.Assets.loadLevel (level state) state else return state

-- Handle game events
gameEventHandler :: EventHandler GameState
gameEventHandler Quit = return False
gameEventHandler (KeyDown sym) = do
	state@(GameState {mode, currentDirection}) <- get
	case (symKey sym) of
#ifdef CHEATING
		SDLK_KP_PLUS -> do
			put$state {
				level = (level state) + 1,
				Snake.GameState.loadLevel = True
			}
			return True
#endif
		SDLK_UP -> do
			put$state {nextDirections = Q.enqueue (nextDirections state) DUp}
			return True
		SDLK_DOWN -> do
			put$state {nextDirections = Q.enqueue (nextDirections state) DDown}
			return True
		SDLK_LEFT -> do
			put$state {nextDirections = Q.enqueue (nextDirections state) DLeft}
			return True
		SDLK_RIGHT -> do
			put$state {nextDirections = Q.enqueue (nextDirections state) DRight}
			return True
		SDLK_ESCAPE -> return False
		SDLK_f -> do
			put$state {
				fastMode =
					not (fastMode state) && (mode /= PausedMode)
			}
			return True
		SDLK_F5 -> do
			put$state {
				mode = if mode == PausedMode
					then InGameMode
					else if mode == InGameMode
						then PausedMode else mode,
				fastMode = False
			}
			return True
		SDLK_F2 -> do
			put$state {
				mode = InGameMode,
				levelCounter = resetCounter 0 (levelCounter state),
				scoreCounter = resetCounter 0 (scoreCounter state),
				level = 1, Snake.GameState.loadLevel = True, score = 0
			}
			return True
		_ -> return True
gameEventHandler _ = return True

-- Play currently scheduled sound effects
playSounds :: GameState -> ReaderT Assets IO ()
playSounds state = do
	Assets {..} <- ask
	liftIO$ forM_ (sfxEvents state) $ \(sound, channel) ->
		playChannel (fromEnum channel) (sfx M.! sound) 0

