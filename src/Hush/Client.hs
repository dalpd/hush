{-# LANGUAGE DataKinds     #-}
-- |
module Hush.Client
  ( searchPhotos
  , searchCollections
  , searchUsers
  , getFullLinks
  ) where

------------------------------------------------------------------------------

import Control.Lens
import Data.Text (Text)
import qualified Hush.API as Hush
import Hush.Types
import Hush.Utils (defaultEnv)
import Servant.Client (ClientError, runClientM)

------------------------------------------------------------------------------

-- | Convenience function to use `Hush.searchPhotos`.
searchPhotos
  :: Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -> IO (Either ClientError PhotoSearchResult)
searchPhotos authHeader keyword perPage pageNumber = do
  env <- defaultEnv
  runClientM (api authHeader keyword perPage pageNumber) env
  where
    api = Hush.searchPhotos

------------------------------------------------------------------------------

-- | Convenience function to use `Hush.searchCollections`.
searchCollections
  :: Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -> IO (Either ClientError CollectionSearchResult)
searchCollections authHeader keyword perPage pageNumber = do
  env <- defaultEnv
  runClientM (api authHeader keyword perPage pageNumber) env
  where
    api = Hush.searchCollections


------------------------------------------------------------------------------

-- | Convenience function to use `Hush.searchUsers`.
searchUsers
  :: Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -> IO (Either ClientError UserSearchResult)
searchUsers authHeader keyword perPage pageNumber = do
  env <- defaultEnv
  runClientM (api authHeader keyword perPage pageNumber) env
  where
    api = Hush.searchUsers


------------------------------------------------------------------------------
getFullLinks :: PhotoSearchResult -> IO [String]
getFullLinks res = do
  let res' = res ^. photoSearchLens
  return $ map (view (resultLens . urlLens)) res'

