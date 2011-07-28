module Snake.Events where

import Common.Counters
import Control.Monad.State
import Graphics.UI.SDL
import Snake.GameState

-- Get all available events, returns the backwards, so be sure reverse
-- the list if order of events is important
pollAllEvents :: IO ([Event])
pollAllEvents = fmap reverse pollAllEvents0
	where
		pollAllEvents0 = do
			event <- pollEvent
			case event of
				NoEvent -> return []
				_ -> (liftM (event:)) pollAllEvents

-- Handle a list of events
-- Returns True if the event loop should continue, otherwise False
handleAllEvents :: [Event] -> State GameState Bool
handleAllEvents =
	foldM (\continue -> \event -> do
		continue' <- handleEvent event
		return$ continue && continue'
	) True

-- Handle an event, which may have side effects on the game state
handleEvent :: Event -> State GameState Bool
handleEvent Quit = return False
handleEvent (KeyDown sym) = do
	state <- get
	case (symKey sym) of
		SDLK_UP -> do
			put$state {gs_nextDirection = DUp}
			return True
		SDLK_DOWN -> do
			put$state {gs_nextDirection = DDown}
			return True
		SDLK_LEFT -> do
			put$state {gs_nextDirection = DLeft}
			return True
		SDLK_RIGHT -> do
			put$state {gs_nextDirection = DRight}
			return True
		SDLK_ESCAPE -> return False
		SDLK_F5 -> do
			put$state {gs_paused = not$ gs_paused state}
			return True
		SDLK_F2 -> do
			put$state {
				gs_levelCounter = resetCounter 0 (gs_levelCounter state),
				gs_scoreCounter = resetCounter 0 (gs_scoreCounter state),
				gs_level = 1, gs_loadLevel = True, gs_score = 0,
				gs_paused = False, gs_gameOver = False}
			return True
		_ -> return True
handleEvent _ = return True

