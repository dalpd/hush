{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- |
module Hush.API
  ( searchPhotos
  , searchCollections
  , searchUsers
  ) where

------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Proxy
import Hush.Types
import Servant.API
import Servant.Client (ClientM, client)

------------------------------------------------------------------------------

-- | TODO:
-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html

type UnsplashAPI = SearchAPI

type SearchAPI
  = "search" :> "photos"
    :> Header "Authorization" Text
    :> QueryParam "query" Text
    :> QueryParam "page" Int
    :> QueryParam "per_page" Int
    :> Get '[JSON] PhotoSearchResult
  :<|> "search" :> "collections"
    :> QueryParam "query" Text
    :> QueryParam "page" Int
    :> QueryParam "per_page" Int
    :> QueryParam "client_id" Text
    :> Get '[JSON] CollectionSearchResult
  :<|> "search" :> "users"
    :> QueryParam "query" Text
    :> QueryParam "page" Int
    :> QueryParam "per_page" Int
    :> QueryParam "client_id" Text
    :> Get '[JSON] UserSearchResult


------------------------------------------------------------------------------
unsplashAPI :: Proxy UnsplashAPI
unsplashAPI = Proxy

searchPhotos
  :: Maybe Text
  -> Maybe Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> ClientM PhotoSearchResult

searchCollections
  :: Maybe Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> Maybe Text
  -> ClientM CollectionSearchResult

searchUsers
  :: Maybe Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> Maybe Text
  -> ClientM UserSearchResult


------------------------------------------------------------------------------
(searchPhotos :<|> searchCollections :<|> searchUsers) = client unsplashAPI


