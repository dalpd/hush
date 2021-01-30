{-# LANGUAGE DataKinds     #-}
-- | Module for shared utility functions needed by hush.
module Hush.Utils
  ( accessKey,

    -- ^ Servant API utils
    OptionalQueryParam,
    RequiredQueryParam,
    
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

-- | Type synonym for optional query params.
type OptionalQueryParam = QueryParam' '[Optional]

-- | Type synonym for required query params.
type RequiredQueryParam = QueryParam' '[Required]

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
