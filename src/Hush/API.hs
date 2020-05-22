{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hush.API
  ( search
  ) where

------------------------------------------------------------------------------

import           Hush.Types

import qualified Data.Text as T
import           Servant (Get, JSON, Proxy (..), QueryParam, (:>))
import           Servant.Client (ClientM, client)

------------------------------------------------------------------------------

type UnsplashAPI = "search"
                :> "photos"
                :> QueryParam "query" T.Text
                :> QueryParam "client_id" T.Text
                :> Get '[JSON] SearchResult


------------------------------------------------------------------------------
unsplashAPI :: Proxy UnsplashAPI
unsplashAPI = Proxy


------------------------------------------------------------------------------
search :: Maybe T.Text -> Maybe T.Text -> ClientM SearchResult
search = client unsplashAPI


