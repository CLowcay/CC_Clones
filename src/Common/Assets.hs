module Common.Assets where

import System.FilePath

getAssetPath :: String -> String
getAssetPath fileName = normalise (ASSET_PREFIX ++ fileName)

