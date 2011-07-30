module Common.HighScores where

import Common.Assets
import Common.UserData
import Common.Util
import Control.Monad
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
	hClose file
	let scoresUnsorted = map (\line ->
			let (name, score) = break (/= '=') line in
				(take 8 (trim name), read (trim score))
		) $ lines contents
	return $ HighScoreState {
		hs_scores = 
			(sortBy (\a -> \b -> (snd a) `compare` (snd b)) scoresUnsorted),
		hs_editing = Nothing
	}

-- write the high scores table
writeHighScoreTable :: HighScoreState -> IO ()
writeHighScoreTable highScores = do
	filename <- highScoresFileName

	file <- openFile filename WriteMode
	mapM_ (\(name, score) ->
			hPutStrLn file (name ++ "=" ++ (show score)))$
		hs_scores highScores
	hClose file
	return ()

-- determine if a score is worthy of the hall of fame
isNewHighScore :: Int -> HighScoreState -> Bool
isNewHighScore score (HighScoreState {hs_scores = scores}) =
	let maxScore =
		foldr (\(_, score') -> \maxScore' -> max score' maxScore') 0 scores
	in score > maxScore || ((score == maxScore) && (
		(not$all (\(_, score') -> score' == maxScore) scores) ||
		length scores < maxHighScores))

-- render the high scores table
renderHighScores :: Surface -> Int -> Int -> Int ->
	Font -> Color -> HighScoreState -> IO ()
renderHighScores dst x y w font color scores = do
	let highScores = hs_scores scores
	lineSkip <- fontLineSkip font
	mapM_ (\(i, (name, score)) -> do
			nameSurface <- renderUTF8Solid font
				(name ++ (if isEditing i then "_" else "")) color
			scoreSurface <- renderUTF8Solid font (show score) color
			let y' = y + (i * lineSkip)
			blitSurface nameSurface Nothing
				dst (Just$ Rect x y' 0 0)
			let scoreWidth = surfaceGetWidth scoreSurface
			blitSurface scoreSurface Nothing
				dst (Just$ Rect (x + w - scoreWidth) y' 0 0)
		) $ zip [0..] highScores
	return ()

	where
		isEditing i = fromMaybe False (fmap (== i) $ hs_editing scores)

