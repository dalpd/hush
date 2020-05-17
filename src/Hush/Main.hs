{-# LANGUAGE OverloadedStrings #-}

module Hush.Main
  ( main
  ) where

------------------------------------------------------------------------------

import Hush.API (runQuery)
import Hush.Utils as HU
import Hush.Types

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as D

------------------------------------------------------------------------------
  
main :: IO ()
main = do
  key <- HU.accessKey
  searchResults <- runQuery "Hangul" key
  pure ()
