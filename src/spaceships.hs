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

{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Common.Counters
import Common.Graphics
import Common.HighScores
import Control.Applicative
import Control.Monad
import Control.Monad.Reader (runReaderT)
import Data.IORef
import FRP.Events
import FRP.Yampa
import Graphics.UI.SDL hiding (Event, NoEvent)
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.TTF
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import Spaceships.Assets
import Spaceships.GameState
import Spaceships.Render

windowCaption :: String
windowCaption = "Spaceships!"

-- Entry point
main :: IO ()
main = do
	initSDL
	setCaption windowCaption windowCaption

	assets@(Assets{..}) <- loadAssets
	gs <- initGlobalState$ gfx M.! Digits
	time <- newIORef =<< fromIntegral <$> getTicks

	reactimate
		(Event <$> getSDLEvents)
		(getInput time)
		(handleOutput assets)
		(sfMain gs)

	Graphics.UI.SDL.quit

-- Library initialisation
initSDL :: IO ()
initSDL = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 728 546 32 [HWSurface, DoubleBuf]
	Graphics.UI.SDL.TTF.init
	return ()

getInput :: IORef Int -> Bool -> IO (DTime, Maybe (Event SDLEvents))
getInput time canBlock = do
	t0 <- readIORef time
	t1 <- fromIntegral <$> getTicks
	writeIORef time t1

	let dt = t1 - t0

	sdlevents <- getSDLEvents
	let events = if null sdlevents then NoEvent else Event sdlevents

	return ((fromIntegral dt) / 1000, Just events)
	return (1 / 60, Just events)

handleOutput :: Assets -> Bool -> (GameOutput, Bool) -> IO Bool
handleOutput assets hasChanged (go, quitNow) = do
	case go of
		HighScore _ _ (Event EditingStart) -> startEditing
		HighScore _ hs (Event EditingStop) -> endEditing hs
		_ -> return ()
	
	runReaderT (renderOutput go) assets

	surface <- getVideoSurface
	SDL.flip surface
	return quitNow

initGlobalState :: Sprite -> IO GlobalState
initGlobalState digits = do
	hs <- loadHighScoreTable "spaceships"
	let scoreC = initCounter digits 4
	let levelC = initCounter digits 2
	return$ GlobalState {
		gs_score = 0,
		gs_level = 1,
		gs_scoreC = resetCounter 0 scoreC,
		gs_levelC = resetCounter 1 levelC,
		gs_highScores = hs
	}

sfMain :: GlobalState -> SF (Event SDLEvents) (GameOutput, Bool)
sfMain gs = proc e -> do
	quitEvents <- sdlQuitEvents -< e
	escEvents <- sdlKeyPresses (mkKey SDLK_ESCAPE) True -< e

	out <- introMode gs -< e

	returnA -< (out, isEvent quitEvents || isEvent escEvents)

