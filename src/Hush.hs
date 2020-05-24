{-# LANGUAGE OverloadedStrings #-}

module Hush
  ( main
  ) where

------------------------------------------------------------------------------

import Hush.Client (getPhotoSearchResults, getFullLinks)
import Hush.Utils as HU

------------------------------------------------------------------------------
  
main :: IO ()
main = do
  key <- HU.accessKey
  resPhotos <- getPhotoSearchResults "Chess" (Just 10) (Just 2) key
--  resCollections <- getCollectionSearchResults "Views" (Just 10) (Just 2) key
--  resUsers <- getUserSearchResults "Alp" (Just 10) (Just 2) key
  case resPhotos of
    Left err -> print err
    Right res -> do
      full <- getFullLinks res
      print full
