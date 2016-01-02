{-# LANGUAGE Arrows #-}

module FRP.Events (
	SDLEvents,
	getSDLEvents,
	sdlKeyPresses,
	sdlKeys,
	sdlAllKeys,
	sdlKeyIsDown,
	sdlMousePos,
	sdlMouseIsDown,
	sdlMouseClicks,
	sdlQuitEvents,
	kEqIgn, mkKey
) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Loops
import Data.List
import Data.Maybe
import Data.Word
import FRP.Yampa
import Graphics.UI.SDL hiding (Event, NoEvent)
import qualified Graphics.UI.SDL as SDL

type SDLEvents = [SDL.Event]

-- TODO: check that these events are in order
getSDLEvents :: IO SDLEvents
getSDLEvents = unfoldWhileM (/= SDL.NoEvent) pollEvent

-- count key presses
sdlKeyPresses :: Keysym -> Bool -> SF (Event SDLEvents) (Event Int)
sdlKeyPresses sym ignModifiers = arr$ \e ->
	length <$> filterSDLEvents (if ignModifiers then fi else f) e
	where
		f (KeyDown k) = k == sym
		f _ = False
		fi (KeyDown k) = k `kEqIgn` sym
		fi _ = False

sdlKeys :: [Keysym] -> Bool -> SF (Event SDLEvents) (Event [Keysym])
sdlKeys syms ignModifiers = arr$ \e ->
	catMaybeSDLEvents$ (fmap.fmap) (if ignModifiers then fi else f)$ e
	where
		f (KeyDown k) = if k `elem` syms then Just k else Nothing
		f _ = Nothing
		fi (KeyDown k) = if isJust$ find (k `kEqIgn`) syms then Just k else Nothing
		fi _ = Nothing

sdlAllKeys :: SF (Event SDLEvents) (Event [Keysym])
sdlAllKeys = arr$ \e -> catMaybeSDLEvents$ (fmap.fmap) f$ e
	where
		f (KeyDown k) = Just k
		f _ = Nothing

-- is a key currently pressed
sdlKeyIsDown :: Keysym -> Bool -> SF (Event SDLEvents) Bool
sdlKeyIsDown sym ignModifiers =
		(hold False) <<< (arr$ \e -> isKeyDown.last <$>
			filterSDLEvents (if ignModifiers then fi else f) e)
	where
		f (KeyDown k) = k == sym
		f (KeyUp k) = k == sym
		f _ = False
		fi (KeyDown k) = k `kEqIgn` sym
		fi (KeyUp k) = k `kEqIgn` sym
		fi _ = False
		isKeyDown (KeyDown _) = True
		isKeyDown _ = False

sdlMousePos :: SF (Event SDLEvents) (Word16, Word16)
sdlMousePos = (hold (0, 0)) <<< (arr$ (last <$>).catMaybeSDLEvents.((map mouseCoords) <$>))
	where
		mouseCoords (MouseMotion x y _ _) = Just (x, y)
		mouseCoords _ = Nothing

sdlMouseIsDown :: MouseButton -> SF (Event SDLEvents) Bool
sdlMouseIsDown bt =
	(hold False) <<< (arr$ \e -> isBtDown.last <$> filterSDLEvents f e)
	where
		f (MouseButtonDown _ _ b) = b == bt
		f (MouseButtonUp _ _ b) = b == bt
		f _ = False
		isBtDown (MouseButtonDown _ _ _) = True
		isBtDown _ = False

sdlMouseClicks :: MouseButton -> SF (Event SDLEvents) (Event Int)
sdlMouseClicks bt = arr$ (length <$>).filterSDLEvents f
	where
		f (MouseButtonDown _ _ b) = b == bt
		f _ = False

sdlQuitEvents :: SF (Event SDLEvents) (Event Int)
sdlQuitEvents = arr$ \ev ->
	case length.filter (== SDL.Quit) <$> ev of
		Event 0 -> NoEvent
		x -> x

filterSDLEvents :: (SDL.Event -> Bool) -> Event SDLEvents -> Event SDLEvents
filterSDLEvents f = (filterE (not.null)).(filter f <$>)

catMaybeSDLEvents :: Event ([Maybe a]) -> Event [a]
catMaybeSDLEvents = (filterE (not.null)).(catMaybes <$>)

kEqIgn :: Keysym -> Keysym -> Bool
kEqIgn a b = noMods a == noMods b
	where noMods (Keysym k _ u) = Keysym k [] u

mkKey :: SDLKey -> Keysym
mkKey k = Keysym k [] '\0'

