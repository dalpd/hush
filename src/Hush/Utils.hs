{-# LANGUAGE DataKinds     #-}
-- | Module for shared utility functions needed by hush.
module Hush.Utils
  ( -- ^ Misc functions for hush
    accessKey,

    -- ^ Servant API utils
    GetJSON,
    OptionalQueryParam,
    RequiredQueryParam,
    AuthHeader,
    Query,
    Page,
    PerPage,
    
    -- ^ Servant Client utils
    defaultEnv,
  ) where

------------------------------------------------------------------------------

import Data.Text
import LoadEnv
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)
import Servant.API
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (Https), mkClientEnv)
import System.Environment (lookupEnv)

------------------------------------------------------------------------------

-- | Type synonym for GET operations returning JSON.
type GetJSON returnType = Get '[JSON] returnType

------------------------------------------------------------------------------

-- | Type synonym for an optional query param.
type OptionalQueryParam = QueryParam' '[Optional]

-- | Type synonym for a required query param.
type RequiredQueryParam = QueryParam' '[Required]

------------------------------------------------------------------------------

-- | Type synonym for an "Authorization" header.
type AuthHeader = Header "Authorization" Text

------------------------------------------------------------------------------

-- | Type synonym for the "query" parameters, e.g. <url>?query=val1&param=val2
-- TODO(dalp): Look into how to get `Query` to correspond to `NonEmpty Text`
-- or just find out if there's a way to assign a mode to `QueryParams` to make
-- it required therefore making an empty list illegal to use.
type Query = QueryParams "query" Text

-- | Type synonym for the "page" parameter, e.g. <url>?page=2
type Page = OptionalQueryParam "page" Int

-- | Type synonym for the "per_page" parameter, represents the number of items
-- in a page , e.g. <url>?page=2
type PerPage = OptionalQueryParam "per_page" Int

------------------------------------------------------------------------------

-- | The default `ClientEnv` for Unsplash API.
defaultEnv :: IO ClientEnv
defaultEnv = do
  manager <- newManager tlsManagerSettings
  pure $ mkClientEnv manager baseUrl

------------------------------------------------------------------------------

-- | Construct a `BaseUrl` for Unsplash API given an API version.
baseUrl :: BaseUrl
baseUrl =
  BaseUrl Https baseUrlHost baseUrlPort baseUrlPath
  where
    baseUrlHost = "api.unsplash.com"
    baseUrlPort = 443
    baseUrlPath = mempty
    
------------------------------------------------------------------------------

-- | A utility function to lookup a certain key in the .env file in the
-- directory you're calling this function from. If you get a hit `accessKey`
-- will return the associated value and if not, an error.
accessKey :: String -> IO Text
accessKey key = loadEnv >> lookupEnv key >>= \case
  Just k -> pure $ pack k
  Nothing -> error $ errorMsg key
  where
    errorMsg k = "Couldn't find `" <> k <> "` in .env"
