module Hush.Client
  ( getPhotoSearchResults
  , getCollectionSearchResults
  , getUserSearchResults
  , getFullLinks
  ) where

------------------------------------------------------------------------------

import qualified Hush.API as Hush
import           Hush.Types

import           Control.Lens
import qualified Data.Text as T
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Client (newManager)
import           Servant.Client (BaseUrl (..), ClientError, ClientEnv,
                                 Scheme (Https), mkClientEnv, runClientM)

------------------------------------------------------------------------------
-- | Helper function for searching photos
getPhotoSearchResults
  :: T.Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> T.Text
  -> IO (Either ClientError PhotoSearchResult)
getPhotoSearchResults keyword perPage pageNumber accessKey = do
  env <- defaultEnv
  runClientM (api keyword perPage pageNumber accessKey) env
  where
    api q pp pg k = Hush.searchPhotos (Just q) pp pg (Just k)


------------------------------------------------------------------------------
-- | Helper function for searching collections
getCollectionSearchResults
  :: T.Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> T.Text
  -> IO (Either ClientError CollectionSearchResult)
getCollectionSearchResults keyword perPage pageNumber accessKey = do
  env <- defaultEnv
  runClientM (api keyword perPage pageNumber accessKey) env
  where
    api q pp pg k = Hush.searchCollections (Just q) pp pg (Just k)


------------------------------------------------------------------------------
-- | Helper function for searching users
getUserSearchResults
  :: T.Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> T.Text
  -> IO (Either ClientError UserSearchResult)
getUserSearchResults keyword perPage pageNumber accessKey = do
  env <- defaultEnv
  runClientM (api keyword perPage pageNumber accessKey) env
  where
    api q pp pg k = Hush.searchUsers (Just q) pp pg (Just k)


------------------------------------------------------------------------------
getFullLinks :: PhotoSearchResult -> IO [String]
getFullLinks res = do
  let res' = res ^. photoSearchLens
  return $ map (view (resultLens . urlLens)) res'


------------------------------------------------------------------------------
defaultEnv :: IO ClientEnv
defaultEnv = do
  manager <- newManager tlsManagerSettings
  pure $ mkClientEnv manager baseUrl


------------------------------------------------------------------------------
baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.unsplash.com" 443 ""
