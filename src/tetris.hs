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
	stdGen <- liftIO$newStdGen
	let (bricks, randomGen) = runState randomBag stdGen
	return$ nextBrick$ GameState {
		mode = InGameMode,
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
			level = 1, levelCounter = setCounter 1 $ initCounter (gfx M.! Digits) 2,
			score = 0, scoreCounter = initCounter (gfx M.! Digits) 6,
			lastLines = 0, totalLines = 0
		},
		sfxEvents = [],
		dropKey = False,
		showPreview = True
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
	let (continue, state1) =
		runState (handleEvents gameEventHandler events) state0
	
	let state2 = updateGame delay state1

	when continue $ mainLoop time1 state2

-- Handle game events
gameEventHandler :: EventHandler GameState
gameEventHandler Quit = return False
gameEventHandler (KeyDown sym) = do
	state@(GameState {mode, queuedRotations, showPreview}) <- get
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

