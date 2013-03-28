{-
CC_Clones - Classic games reimplemented
© Callum Lowcay 2006-2013

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

import Control.Monad
import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.TTF

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
	-- openAudio defaultFrequency AudioS16Sys 2 4096
	-- allocateChannels (fromEnum ChannelCount)
	return ()

mainLoop :: Int -> GameState -> ReaderT Assets IO ()
mainLoop time0 state0 = do
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

	when continue $ mainLoop time1 state2

