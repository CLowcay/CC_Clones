{-# LANGUAGE Arrows #-}

module FRP.Counters (
	CounterCommand(..),
	applyCounterCommand,
	CounterControl,
	counterControl
) where

import Common.Counters
import Data.Monoid
import FRP.Yampa

data CounterCommand =
	CounterAdd Int | CounterSub Int | CounterSet Int | CounterReset Int deriving Show

instance Monoid CounterCommand where
	mempty = CounterAdd 0
	_ `mappend` (CounterSet y) = CounterSet y
	_ `mappend` (CounterReset y) = CounterReset y
	(CounterSet x) `mappend` (CounterAdd y) = CounterSet (x + y)
	(CounterReset x) `mappend` (CounterAdd y) = CounterSet (x + y)
	(CounterSet x) `mappend` (CounterSub y) = CounterSet (0 `max` (x - y))
	(CounterReset x) `mappend` (CounterSub y) = CounterSet (0 `max` (x - y))
	(CounterAdd x) `mappend` (CounterAdd y) = CounterAdd (x + y)
	(CounterAdd x) `mappend` (CounterSub y) = if x >= y then CounterAdd (x - y) else CounterSub (y - x)
	(CounterSub x) `mappend` (CounterAdd y) = if y >= x then CounterAdd (y - x) else CounterSub (x - y)
	(CounterSub x) `mappend` (CounterSub y) = CounterSub (x + y)

applyCounterCommand :: CounterCommand -> Int -> Int
applyCounterCommand (CounterAdd x) z = z + x
applyCounterCommand (CounterSub x) z = z - x
applyCounterCommand (CounterSet x) _ = x
applyCounterCommand (CounterReset x) _ = x

type CounterControl = SF (Event CounterCommand) CounterState

counterControl :: CounterState -> CounterControl
counterControl c0 =
	(arr$ \e -> (e, ())) >>> second deltaTime >>> counterControlInner c0
counterControlInner :: CounterState -> SF (Event CounterCommand, Time) CounterState
counterControlInner = sscan$ \c0 (cmd, dt) ->
	let c1 = case cmd of
		Event (CounterAdd x) -> addCounter x c0
		Event (CounterSub x) -> subCounter x c0
		Event (CounterSet x) -> setCounter x c0
		Event (CounterReset x) -> resetCounter x c0
		NoEvent -> c0
	in updateCounter (floor$ dt * 1000) c1

deltaTime :: SF () Time
deltaTime = (arr$ \(t0, t1) -> t1 - t0) <<< ((iPre 0 <<< time) &&& time)

