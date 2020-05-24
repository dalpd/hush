module Hush.Utils
  ( accessKey
  ) where

------------------------------------------------------------------------------

import Data.Text
import LoadEnv
import System.Environment (lookupEnv)

------------------------------------------------------------------------------
accessKey :: IO Text
accessKey = do
  loadEnv
  keyMaybe <- lookupEnv "ACCESS_KEY"
  case keyMaybe of
    Just k -> pure $ pack k
    Nothing -> error "Couldn't find ACCESS_KEY"
