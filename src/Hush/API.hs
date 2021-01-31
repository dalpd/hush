{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeOperators  #-}
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
import Hush.Utils
import Servant.API
import Servant.Client (ClientM, client)

------------------------------------------------------------------------------

-- | Type representation for Unsplash API.
-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
type UnsplashAPI = SearchAPI

------------------------------------------------------------------------------

-- | Search endpoints for Unsplash API.
-- https://unsplash.com/documentation#search
type SearchAPI =
  "search" :> SearchPhotos :<|> SearchCollections :<|> SearchUsers

-- | A generalization of the GET endpoints used for searches.
type SearchEndpoint returnType =
  AuthHeader :> Query :> Page :> PerPage :> GetJSON returnType

-- | An endpoint to search photos.
-- GET /search/photos
-- https://unsplash.com/documentation#search-photos
type SearchPhotos =
  "photos" :> SearchEndpoint PhotoSearchResult

-- | An endpoint to search collections.
-- GET /search/collections
-- https://unsplash.com/documentation#search-collections
type SearchCollections =
  "collections" :> SearchEndpoint CollectionSearchResult

-- | An endpoint to search users.
-- GET /search/users
-- https://unsplash.com/documentation#search-users
type SearchUsers =
  "users" :> SearchEndpoint UserSearchResult

------------------------------------------------------------------------------
searchAPI :: Proxy SearchAPI
searchAPI = Proxy

------------------------------------------------------------------------------

-- | Function to query Unsplash API to search for photos.
searchPhotos
  :: Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -- ^ Number of results per page, up to a maximum of 30 items.
  -> ClientM PhotoSearchResult

searchCollections
  :: Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -- ^ Number of results per page, up to a maximum of 30 items.
  -> ClientM CollectionSearchResult

searchUsers
  :: Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -- ^ Number of results per page, up to a maximum of 30 items.
  -> ClientM UserSearchResult

------------------------------------------------------------------------------
(searchPhotos :<|> searchCollections :<|> searchUsers) = client searchAPI
