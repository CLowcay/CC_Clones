module Common.HighScores where

import Common.Assets
import Common.UserData
import Common.Util
import System.Directory
import System.FilePath

data HighScoreTable = HighScoreTable {
	scores :: [(String, Int)]
}

highScoresFileName :: IO FilePath
highScoresFileName = do
	userDir <- userDataPath
	return$ useDir </> "highscores"

loadHighScoreTable :: IO (HighScoreTable)
loadHighScoreTable = do
	filename <- highScoresFileName

	exists <- doesFileExist filename
	when (not exists) $ do
		copyFile (getAssetPath "highscores") filename
		
	file <- openFile filename ReadMode
	contents <- hGetContents
	hClose file
	let scoresUnsorted = map (\line ->
			let (name, score) = break (/= '=') line in
				(take 8 (trim name), read (trim score))
		) $ lines contents
	return (sortBy (\a -> \b -> (snd a) <= (snd b)) scoresUnsorted)

