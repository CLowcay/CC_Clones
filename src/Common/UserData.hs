module Common.UserData where

import System.FilePath
import System.Posix.User

userDataPath :: IO FilePath
userDataPath = do
	entry <- getLoginName >>= getUserEntryForName
	let path = (normalise (homeDirectory entry)) </> "clones"
	createDirectoryIfMissing True path
	return path

