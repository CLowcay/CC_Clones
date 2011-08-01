module Common.UserData where

import System.Directory
import System.FilePath

userDataPath :: IO FilePath
userDataPath = do
	home <- getHomeDirectory
	let path = (normalise home) </> "clones"
	createDirectoryIfMissing True path
	return path

