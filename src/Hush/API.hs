{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hush.API
  ( runQuery
  ) where

------------------------------------------------------------------------------

import           Hush.Types

import qualified Data.Text as T
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Client (newManager)
import           Servant (Get, JSON, Proxy (..), QueryParam, (:>))
import           Servant.Client (BaseUrl (..), ClientM,
                                 Scheme (Https), client, mkClientEnv, runClientM)

------------------------------------------------------------------------------

type UnsplashAPI = "search"
                :> "photos"
                :> QueryParam "query" T.Text
                :> QueryParam "client_id" T.Text
                :> Get '[JSON] ([SearchResult])


------------------------------------------------------------------------------
unsplashAPI :: Proxy UnsplashAPI
unsplashAPI = Proxy


------------------------------------------------------------------------------
search :: Maybe T.Text -> Maybe T.Text ->  ClientM [SearchResult]
search = client unsplashAPI


------------------------------------------------------------------------------
runQuery :: T.Text -> T.Text -> IO [SearchResult]
runQuery keyword accessKey = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "api.unsplash.com" 443 "")
  res <- runClientM (search (Just keyword) (Just accessKey)) env
  case res of
    Left err -> error ("" ++ show err)
    Right returns -> do
      pure returns
