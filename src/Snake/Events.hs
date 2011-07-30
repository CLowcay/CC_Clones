module Snake.Events where

import Common.Counters
import Common.Events
import Control.Monad.State
import Graphics.UI.SDL
import Snake.GameState

-- Handle an event, which may have side effects on the game state
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
			put$state {gs_paused = not$ gs_paused state, gs_fastMode = False}
			return True
		SDLK_F2 -> do
			put$state {
				gs_levelCounter = resetCounter 0 (gs_levelCounter state),
				gs_scoreCounter = resetCounter 0 (gs_scoreCounter state),
				gs_level = 1, gs_loadLevel = True, gs_score = 0,
				gs_paused = False, gs_gameOver = False}
			return True
		_ -> return True
gameEventHandler _ = return True

