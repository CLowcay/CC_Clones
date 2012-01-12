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
	setVideoMode 680 598 32 [HWSurface, DoubleBuf]
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
		brickQueue = bricks,
		currentBrick = IBrick,
		currentRotation = RUp,
		currentHeight = 22,
		currentPos = 0,
		currentSlide = Nothing,
		field = clearField,
		downTimer = resetTimer,
		slideTimer = resetTimer,
		downFTA = 0,
		slideFTA = 0,
		score = 0, scoreCounter = initCounter (gfx M.! Digits) 5,
		sfxEvents = [],
		level = 0, levelCounter = initCounter (gfx M.! Digits) 2,
		dropKey = False
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
	state <- get
	case (symKey sym) of
		SDLK_DOWN -> do
			put$state {dropKey = True}
			return True
		SDLK_LEFT -> do
			put$state {currentSlide = Just SlideLeft}
			return True
		SDLK_RIGHT -> do
			put$state {currentSlide = Just SlideRight}
			return True
		SDLK_ESCAPE -> return False
		_ -> return True
gameEventHandler (KeyUp sym) = do
	state <- get
	case (symKey sym) of
		SDLK_DOWN -> do
			put$state {dropKey = False}
			return True
		_ -> return True
gameEventHandler _ = return True

-- Play currently scheduled sound effects
playSounds :: GameState -> ReaderT Assets IO ()
playSounds state = do
	Assets {..} <- ask
	liftIO$ forM_ (sfxEvents state) $ \(sound, channel) ->
		playChannel (fromEnum channel) (sfx M.! sound) 0

