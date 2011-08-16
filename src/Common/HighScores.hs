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

module Common.HighScores where

import Common.Assets
import Common.Events
import Common.UserData
import Common.Util
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Graphics.UI.SDL
import Graphics.UI.SDL.TTF
import System.Directory
import System.FilePath
import System.IO

data HighScoreState = HighScoreState {
	hs_scores :: [(String, Int)],
	hs_editing :: Maybe Int
} deriving (Show)

maxHighScores :: Int
maxHighScores = 5

maxNameLength :: Int
maxNameLength = 12

highScoresFileName :: IO FilePath
highScoresFileName = do
	userDir <- userDataPath
	return$ userDir </> "snake.highscores"

-- load the high scores table
loadHighScoreTable :: IO (HighScoreState)
loadHighScoreTable = do
	filename <- highScoresFileName

	exists <- doesFileExist filename
	when (not exists) $ do
		copyFile (getAssetPath "highscores") filename
		
	file <- openFile filename ReadMode
	contents <- hGetContents file
	let scoresUnsorted = map (\line ->
			let (name, score) = break (== '=') line in
				(take maxNameLength (trim name), read (trim$ tail score))
		) $ filter (not.null) (map trim (lines contents))
	return $ HighScoreState {
		hs_scores = sortBy (compare `on` snd) scoresUnsorted,
		hs_editing = Nothing
	}

-- write the high scores table
writeHighScoreTable :: HighScoreState -> IO ()
writeHighScoreTable highScores = do
	filename <- highScoresFileName

	file <- openFile filename WriteMode
	forM_ (hs_scores highScores) $ \(name, score) -> do
		hPutStrLn file (name ++ "=" ++ (show score))

	hClose file
	return ()

-- determine if a score is worthy of the hall of fame
isNewHighScore :: Int -> HighScoreState -> Bool
isNewHighScore score (HighScoreState {hs_scores = scores}) =
	(length scores < maxHighScores) ||
		(any (\(_, score') -> score' <= score) scores)

-- add a new high score and start editing
insertHighScore :: Int -> HighScoreState -> HighScoreState
insertHighScore score highScores =
	let
		scores = hs_scores highScores
		i = fromMaybe (length scores) $
			findIndex (\(_, score') -> score' <= score) scores
	in highScores {
		hs_scores = take maxHighScores
			((take i scores) ++ [("", score)] ++ (drop i scores)),
		hs_editing = Just i
	}

-- is the high score table being editing
isEditing :: HighScoreState -> Bool
isEditing (HighScoreState {hs_editing = editing}) = isJust editing

-- render the high scores table
renderHighScores :: Surface -> Int -> Int -> Int ->
	Font -> Color -> HighScoreState -> IO ()
renderHighScores dst x y w font color scores = do
	let highScores = hs_scores scores
	lineSkip <- fontLineSkip font

	forM_ (zip [0..] highScores) $ \(i, (name, score)) -> do
		nameSurface <- renderUTF8Solid font
			(name ++ (if isEditing i then "_" else "")) color
		scoreSurface <- renderUTF8Solid font (show score) color
		let y' = y + (i * lineSkip)
		blitSurface nameSurface Nothing
			dst (Just$ Rect x y' 0 0)
		let scoreWidth = surfaceGetWidth scoreSurface
		blitSurface scoreSurface Nothing
			dst (Just$ Rect (x + w - scoreWidth) y' 0 0)

	return ()

	where
		isEditing i = fromMaybe False (fmap (== i) $ hs_editing scores)

-- call when editing starts
startEditing :: IO ()
startEditing = typingMode True

-- call when editing is done
endEditing :: HighScoreState -> IO ()
endEditing state = do
	typingMode False
	writeHighScoreTable state
	return ()

-- Enable or disable typing mode
typingMode :: Bool -> IO ()
typingMode enable = if enable then do
		enableKeyRepeat 500 30
		enableUnicode True
		return ()
	else do
		enableKeyRepeat 0 0
		enableUnicode False
		return ()

highScoreEventHandler :: EventHandler HighScoreState
highScoreEventHandler Quit = return False
highScoreEventHandler (KeyDown sym) = do
	state <- get
	let
		iEditing = fromJust$ hs_editing state
		scores = hs_scores state
		(editingName, editingScore) = scores !! iEditing
		char = symUnicode sym
		keySym = symKey sym

	when ((isValidNameChar char) && ((length editingName) < maxNameLength)) $ do
		put$state {
			hs_scores =
				updateList iEditing (editingName ++ [char], editingScore) scores
		}
		return ()

	when (char == '\b') $ do
		put$state {
			hs_scores =
				updateList iEditing (safeInit editingName, editingScore) scores
		}
		return ()

	when (keySym == SDLK_RETURN || keySym == SDLK_KP_ENTER) $ do
		put$state {
			hs_editing = Nothing
		}
		return ()

	return True
	where
		safeInit [] = []
		safeInit xs = Data.List.init xs
		isValidNameChar char = isAlphaNum char || isSpace char ||
			((isPunctuation char) && (not (char == '=')))
highScoreEventHandler _ = return True

-- Update a list at an index
updateList :: Int -> a -> [a] -> [a]
updateList i a xs = (take i xs) ++ [a] ++ (drop (i + 1) xs)

