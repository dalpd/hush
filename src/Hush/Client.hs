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

-- | Helper function for searching photos
searchPhotos
  :: Maybe Text
  -> Maybe Text -- keyword
  -> Maybe Int -- pp
  -> Maybe Int --pn
  -> IO (Either ClientError PhotoSearchResult)
searchPhotos authHeader keyword perPage pageNumber = do
  env <- defaultEnv
  runClientM (api authHeader keyword perPage pageNumber) env
  where
    api = Hush.searchPhotos

------------------------------------------------------------------------------

-- | Helper function for searching collections.
searchCollections
  :: Text
  -> Maybe Int
  -> Maybe Int
  -> Text
  -> IO (Either ClientError CollectionSearchResult)
searchCollections keyword perPage pageNumber accessKey = do
  env <- defaultEnv
  runClientM (api keyword perPage pageNumber accessKey) env
  where
    api q pp pg k = Hush.searchCollections (Just q) pp pg (Just k)


------------------------------------------------------------------------------

-- | Helper function for searching users.
searchUsers
  :: Text
  -> Maybe Int
  -> Maybe Int
  -> Text
  -> IO (Either ClientError UserSearchResult)
searchUsers keyword perPage pageNumber accessKey = do
  env <- defaultEnv
  runClientM (api keyword perPage pageNumber accessKey) env
  where
    api q pp pg k = Hush.searchUsers (Just q) pp pg (Just k)


------------------------------------------------------------------------------
getFullLinks :: PhotoSearchResult -> IO [String]
getFullLinks res = do
  let res' = res ^. photoSearchLens
  return $ map (view (resultLens . urlLens)) res'

