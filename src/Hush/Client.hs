module Hush.Client
  ( getSearchResults
  ) where

------------------------------------------------------------------------------

import qualified Hush.API as Hush
import           Hush.Types

import qualified Data.Text as T
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Client (newManager)
import           Servant.Client (BaseUrl (..), ClientError, ClientEnv,
                                 Scheme (Https), mkClientEnv, runClientM)

------------------------------------------------------------------------------
getSearchResults :: T.Text -> T.Text -> IO (Either ClientError SearchResult)
getSearchResults keyword accessKey = do
  env <- defaultEnv
  runClientM (api keyword accessKey) env
  where
    api k a = Hush.search (Just k) (Just a)

defaultEnv :: IO ClientEnv
defaultEnv = do
  manager <- newManager tlsManagerSettings
  pure $ mkClientEnv manager baseUrl

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.unsplash.com" 443 ""
