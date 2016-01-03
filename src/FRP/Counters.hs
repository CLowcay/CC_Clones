{-# LANGUAGE Arrows #-}

module FRP.Counters (
	CounterCommand(..),
	CounterControl,
	counterControl
) where

import Common.Counters
import FRP.Yampa

data CounterCommand =
	CounterAdd Int | CounterSub Int | CounterSet Int | CounterReset Int deriving Show

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

