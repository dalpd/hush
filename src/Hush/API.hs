{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hush.API
  ( searchPhotos
  , searchCollections
  , searchUsers
  ) where

------------------------------------------------------------------------------

import           Hush.Types

import qualified Data.Text as T
import           Data.Proxy
import           Servant.API
import           Servant.Client (ClientM, client)

------------------------------------------------------------------------------
-- | TODO:
-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html

type UnsplashAPI = SearchAPI

type SearchAPI =
    "search" :>
             ( "photos"
             :> QueryParam "query" T.Text
             :> QueryParam "per_page" Int
             :> QueryParam "page" Int
             :> QueryParam "client_id" T.Text
             :> Get '[JSON] PhotoSearchResult
             :<|> "collections"
             :> QueryParam "query" T.Text
             :> QueryParam "per_page" Int
             :> QueryParam "page" Int
             :> QueryParam "client_id" T.Text
             :> Get '[JSON] CollectionSearchResult
             :<|> "users"
             :> QueryParam "query" T.Text
             :> QueryParam "per_page" Int
             :> QueryParam "page" Int
             :> QueryParam "client_id" T.Text
             :> Get '[JSON] UserSearchResult
             )


------------------------------------------------------------------------------
searchAPI :: Proxy SearchAPI
searchAPI = Proxy

searchPhotos
  :: Maybe T.Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> Maybe T.Text
  -> ClientM PhotoSearchResult

searchCollections
  :: Maybe T.Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> Maybe T.Text
  -> ClientM CollectionSearchResult

searchUsers
  :: Maybe T.Text
  -> Maybe Int -- up to a maximum of 30 items per page
  -> Maybe Int
  -> Maybe T.Text
  -> ClientM UserSearchResult


------------------------------------------------------------------------------
(searchPhotos :<|> searchCollections :<|> searchUsers) = client searchAPI


