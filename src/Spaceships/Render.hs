{-# LANGUAGE RecordWildCards #-}

module Spaceships.Render (
	renderOutput
) where

import Common.Counters
import Common.Graphics
import Common.HighScores
import Control.Monad.Reader
import FRP.Yampa.Event
import Graphics.UI.SDL hiding (Event, NoEvent)
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import Spaceships.Assets
import Spaceships.GameState

renderOutput :: FullGameOutput -> ReaderT Assets IO ()
renderOutput (FullGameOutput o level score) = do
	renderOutput0 o
	renderSidePanel level score

renderOutput0 (Intro (GlobalState {..})) = do
	Assets {..} <- ask
	liftIO$ do
		dst <- getVideoSurface

		fillRect dst Nothing bgColor
		renderSprite dst 0 (0, 0)$ gfx M.! FrameV
		renderSprite dst 0 (26, 0)$ gfx M.! FrameH
		renderSprite dst 0 (26, 520)$ gfx M.! FrameH

		let
			m1 = (getMessage MessageIntro1)
			m2 = (getMessage MessageIntro2)
			w1 = surfaceGetWidth m1
			w2 = surfaceGetWidth m2

		blitSurface m1 Nothing dst$ Just$ Rect ((494 - w1) `div` 2 + 26) 30 0 0
		blitSurface m2 Nothing dst$ Just$ Rect ((494 - w2) `div` 2 + 26) 100 0 0
		renderHighScores dst ((494 - 14 * 26) `div` 2 + 26, 130)
			(14 * 26) font messageColor gs_highScores

renderOutput0 (GameOver (GlobalState {..})) = do
	Assets {..} <- ask
	liftIO$ do
		dst <- getVideoSurface

		fillRect dst Nothing bgColor
		renderSprite dst 0 (0, 0)$ gfx M.! FrameV
		renderSprite dst 0 (26, 0)$ gfx M.! FrameH
		renderSprite dst 0 (26, 520)$ gfx M.! FrameH

		renderSprite dst 0 ((494 - 200) `div` 2 + 26, (494 - 64) `div` 2 + 26)$
			gfx M.! GameOverTile

renderOutput0 (HighScore (GlobalState {..}) hs editingEvent) = do
	Assets {..} <- ask
	renderBackground
	liftIO$ do
		dst <- getVideoSurface

		fillRect dst Nothing bgColor
		renderSprite dst 0 (0, 0)$ gfx M.! FrameV
		renderSprite dst 0 (26, 0)$ gfx M.! FrameH
		renderSprite dst 0 (26, 520)$ gfx M.! FrameH

		renderHighScores dst ((494 - 14 * 26) `div` 2 + 26, 130) (14 * 26) font messageColor hs

renderOutput0 (Playing (GameRound {..})) = do
	Assets {..} <- ask

	renderBackground

	forM_ gr_objects$ \(GameObject kind (lane@(LaneControl _ _ dir))) -> do
		let pos = lanePosition lane
		case kind of
			Player -> renderSpaceship pos dir True
			Enemy -> renderSpaceship pos dir False
			Laser -> renderLaser pos dir

renderSpaceship  :: (Int, Int) -> Direction -> Bool -> ReaderT Assets IO ()
renderSpaceship (x, y) dir isPlayer = do
	Assets {..} <- ask
	liftIO$ do
		dst <- getVideoSurface
		renderSprite dst 0 (x, y)$ gfx M.! (getSpaceshipTile isPlayer dir)

renderLaser :: (Int, Int) -> Direction -> ReaderT Assets IO ()
renderLaser (x, y) dir = do
	Assets {..} <- ask
	liftIO$ do
		dst <- getVideoSurface
		renderSprite dst 0 (x, y)$ gfx M.! (getLaserTile dir)

getSpaceshipTile :: Bool -> Direction -> Tile
getSpaceshipTile True DUp = PlayerU
getSpaceshipTile True DDown = PlayerD
getSpaceshipTile True DLeft = PlayerL
getSpaceshipTile True DRight = PlayerR
getSpaceshipTile False DUp = AiU
getSpaceshipTile False DDown = AiD
getSpaceshipTile False DLeft = AiL
getSpaceshipTile False DRight = AiR

getLaserTile :: Direction -> Tile
getLaserTile DUp = LaserU
getLaserTile DDown = LaserD
getLaserTile DLeft = LaserL
getLaserTile DRight = LaserR

renderBackground :: ReaderT Assets IO ()
renderBackground = do
	Assets {..} <- ask
	liftIO$ do
		dst <- getVideoSurface
		renderSprite dst 0 (0, 0)$ gfx M.! Background

renderSidePanel :: CounterState -> CounterState -> ReaderT Assets IO ()
renderSidePanel levelC scoreC = do
	Assets {..} <- ask
	liftIO$ do
		renderCounter (520+31, 212) levelC
		renderCounter (520+77, 212) scoreC
		dst <- getVideoSurface
		renderSprite dst 0 (520, 0)$ gfx M.! SidePanel

