module Common.Events where

import Graphics.UI.SDL
import Control.Monad.State

type EventHandler a = Event -> State a Bool

-- Get all available events
pollEvents :: IO ([Event])
pollEvents = fmap reverse pollAllEvents0
	where
		pollAllEvents0 = do
			event <- pollEvent
			case event of
				NoEvent -> return []
				_ -> (liftM (event:)) pollEvents

-- Handle a list of events
-- Returns True if the event loop should continue, otherwise False
handleEvents :: EventHandler a -> [Event] -> State a Bool
handleEvents handler =
	foldM (\continue -> \event -> do
		continue' <- handler event
		return$ continue && continue'
	) True

