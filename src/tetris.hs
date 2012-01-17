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

import Common.AniTimer
import Common.Counters
import Common.Events
import Common.HighScores
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
import System.Random
import Tetris.Assets
import Tetris.GameState
import Tetris.Render

windowCaption :: String
windowCaption = "Tetris"

-- Entry point
main :: IO ()
main = do
	initSDL
	setCaption windowCaption windowCaption
	assets <- loadAssets
	state0 <- runReaderT initGameState assets
	time <- fmap fromIntegral$ getTicks
	runReaderT (mainLoop time state0) assets
	closeAudio
	Graphics.UI.SDL.quit

-- Library initialisation
initSDL :: IO ()
initSDL = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 481 546 32 [HWSurface, DoubleBuf]
	Graphics.UI.SDL.TTF.init
	openAudio defaultFrequency AudioS16Sys 2 4096
	allocateChannels (fromEnum ChannelCount)
	return ()

-- The initial GameState
initGameState :: ReaderT Assets IO GameState
initGameState = do
	Assets {..} <- ask
	liftIO$ do
		stdGen <- newStdGen
		let (bricks, randomGen) = runState randomBag stdGen

		highScores <- loadHighScoreTable "tetris"

		introMessage <- renderUTF8Solid font
			"Press F2 to start," (Color 0 64 255)
		introMessage2 <- renderUTF8Solid font
			"Esc to quit" (Color 0 64 255)
		introMessage3 <- renderUTF8Solid font
			"High scores:" (Color 0 64 255)
		highScoreMessage <- renderUTF8Solid font
			"New high score! Enter your name" (Color 0 64 255)

		return$ nextBrick$ GameState {
			mode = IntroMode,
			highScores = highScores,
			introMessage = introMessage, introMessage2 = introMessage2,
			introMessage3 = introMessage3,
			highScoreMessage = highScoreMessage,
			randomState = randomGen,
			gracePeriod = False,
			brickQueue = Q.empty `Q.enqueueMany` bricks,
			currentBrick = IBrick,
			currentRotation = RUp,
			currentHeight = srsSpawnHeight,
			currentPos = 0,
			currentSlide = SlideLeft,
			slideActive = False,
			queuedRotations = 0,
			fullLines = [],
			field = clearField,
			downTimer = resetTimer,
			slideTimer = resetTimer,
			lineTimer = resetTimer,
			downFTA = 0,
			slideFTA = 0,
			lineFTA = 0,
			scoreState = ScoreState {
				level = 0, levelCounter = initCounter (gfx M.! Digits) 2,
				score = 0, scoreCounter = initCounter (gfx M.! Digits) 6,
				lastLines = 0, totalLines = 0
			},
			sfxEvents = [],
			dropKey = False,
			showPreview = False
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
	
	let state2 = updateGame delay state1

	-- Manage high score editing
	let
		isEditing0 = isEditing$ highScores state0
		isEditing1 = isEditing$ highScores state2
	when (isEditing0 && not isEditing1) $
		liftIO$endEditing (highScores state2)
	when (not isEditing0 && isEditing1) $
		liftIO$startEditing

	-- If high score editing is over, go back to IntroMode
	let state3 = if isEditing0 && not isEditing1 then
		(state2 {
			mode = IntroMode,
			downTimer = resetTimer,
			field = clearField,
			showPreview = False,
			scoreState = resetScoreState (scoreState state2)
		})
		else state2

	when continue $ mainLoop time1 state3

-- Handle game events
gameEventHandler :: EventHandler GameState
gameEventHandler Quit = return False
gameEventHandler (KeyDown sym) = do
	state@(GameState {mode, scoreState, queuedRotations, showPreview}) <- get
	case (symKey sym) of
		SDLK_UP -> do
			put$state {queuedRotations = queuedRotations + 1}
			return True
		SDLK_DOWN -> do
			put$state {dropKey = True}
			return True
		SDLK_LEFT -> do
			put$state {slideActive = True, currentSlide = SlideLeft}
			return True
		SDLK_RIGHT -> do
			put$state {slideActive = True, currentSlide = SlideRight}
			return True
		SDLK_ESCAPE -> return False
		SDLK_F2 -> do
			put$state {
				mode = if mode == IntroMode then InGameMode else mode,
				field = clearField,
				showPreview = if mode == IntroMode then True else showPreview,
				scoreState = if mode /= IntroMode then scoreState else scoreState {
					level = 1, levelCounter = resetCounter 1 (levelCounter scoreState)
				}
			}
			return True
		SDLK_F5 -> do
			put$state {mode = case mode of
				PausedMode -> InGameMode
				InGameMode -> PausedMode
				x -> x
			}
			return True
		SDLK_p -> do
			put$state {showPreview = not showPreview}
			return True
		_ -> return True
gameEventHandler (KeyUp sym) = do
	state <- get
	case (symKey sym) of
		SDLK_DOWN -> do
			put$state {dropKey = False}
			return True
		SDLK_LEFT -> do
			put$state {slideActive = False}
			return True
		SDLK_RIGHT -> do
			put$state {slideActive = False}
			return True
		_ -> return True
gameEventHandler _ = return True

-- Play currently scheduled sound effects
playSounds :: GameState -> ReaderT Assets IO ()
playSounds state = do
	Assets {..} <- ask
	liftIO$ forM_ (sfxEvents state) $ \(sound, channel) ->
		playChannel (fromEnum channel) (sfx M.! sound) 0

