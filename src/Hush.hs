{-# LANGUAGE OverloadedStrings #-}

module Hush
  ( main
  ) where

------------------------------------------------------------------------------

import Hush.Client (searchPhotos, getFullLinks)
import Hush.Utils as HU

------------------------------------------------------------------------------
  
main :: IO ()
main = do
  key <- HU.accessKey "ACCESS_KEY"
  resPhotos <-
    searchPhotos
      (Just $ "Client-ID " <> key)
      (["Korea"])
      (Just 10)
      (Just 5)
      (Just OrderBy_Relevant)
      (Just ContentFilter_Low)
      Nothing
      Nothing
  --  resCollections <- getCollectionSearchResults "Views" (Just 10) (Just 2) key
  --  resUsers <- getUserSearchResults "Alp" (Just 10) (Just 2) key
  case resPhotos of
    Left err -> print err
    Right res -> do
      full <- getFullLinks res
      print full
