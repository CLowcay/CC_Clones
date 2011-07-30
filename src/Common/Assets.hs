module Common.Assets where

getAssetPath :: String -> String
getAssetPath fileName = ASSET_PREFIX ++ fileName

