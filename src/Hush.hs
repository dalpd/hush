{-# LANGUAGE OverloadedStrings #-}

module Hush
  ( main
  ) where

------------------------------------------------------------------------------

import Hush.Client (getSearchResults)
import Hush.Utils as HU

------------------------------------------------------------------------------
  
main :: IO ()
main = do
  key <- HU.accessKey
  searchResults <- getSearchResults "Hangul" key
  print searchResults
  pure ()
