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

module Common.Events where

import Graphics.UI.SDL
import Control.Monad.State

type EventHandler a = Event -> State a Bool

-- Get all available events
pollEvents :: IO [Event]
pollEvents = fmap reverse pollAllEvents0
	where
		pollAllEvents0 = do
			event <- pollEvent
			case event of
				NoEvent -> return []
				_ -> liftM (event:) pollEvents

-- Handle a list of events
-- Returns True if the event loop should continue, otherwise False
handleEvents :: EventHandler a -> [Event] -> State a Bool
handleEvents handler =
	foldM (\continue event -> do
		continue' <- handler event
		return$ continue && continue'
	) True

