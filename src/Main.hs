{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Maybe
import GHC.Generics
import Network.Curl
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as D

type Key = String

accessKey :: Key
accessKey = "<Token>"

api :: String
api = "https://api.unsplash.com/"

data SearchResult = SearchResult
                  { total       :: Int
                  , total_pages :: Int
                  , results     :: Maybe [Res]
                  }
                  deriving (Generic, Show)

data Res = Res
         { id :: String
         , created_at :: String
         , width  :: Int
         , height :: Int
         , color  :: String
         , likes  :: Maybe Int
         , liked_by_user :: Bool
         , description   :: Maybe String
         , user  :: User
         , current_user_collections :: Maybe [String]
         , urls  :: URLs
         , links :: ResultLinks
         }
         deriving (Generic, Show)

data User = User
          { id :: String
          , username :: String
          , name     :: String
          , first_name :: String
          , last_name  :: Maybe String
          , instagram_username :: Maybe String
          , twitter_username :: Maybe String
          , portfolio_url  :: Maybe String
          , profile_image :: Maybe Image
          , links     :: Maybe UserLinks
          }
          deriving (Generic, Show)

data Image = Image
           { small  :: String
           , medium :: String
           , large  :: String
           }
           deriving (Generic, Show)


data ResultLinks = ResultLinks
           { self    :: String
           , html    :: String
           , download :: String
           }
           deriving (Generic, Show)

data UserLinks = UserLinks
           { self    :: String
           , html    :: String
           , photos  :: String
           , likes :: String
           }
           deriving (Generic, Show)

data URLs = URLs
          { raw     :: String
          , full    :: String
          , regular :: String
          , small   :: String
          , thumb   :: String
          }
          deriving (Generic, Show)

instance FromJSON SearchResult
instance FromJSON Res
instance FromJSON User
instance FromJSON Image
instance FromJSON ResultLinks
instance FromJSON UserLinks
instance FromJSON URLs

searchPhotos :: String -> IO (CurlCode, String)
searchPhotos query =
  curlGetString (api ++ "search/photos?query=" ++ query) options
    where
      options =
        [ CurlHttpHeaders ["Authorization: Client-ID " ++ accessKey],
          CurlVerbose True
        ]

resultsLens :: Lens' SearchResult (Maybe [Res])
resultsLens = lens results (\search_result s -> search_result { results = s})

urlsLens :: Lens' Res URLs
urlsLens = lens urls (\result r -> result { urls = r })

fullLens :: Lens' URLs String
fullLens = lens full (\url u -> url { full = u })

plsCompose :: [Res] -> [String]
plsCompose [] = []
plsCompose (r:rs) = r ^. (urlsLens . fullLens) : plsCompose rs

getLinks :: Either String SearchResult -> Maybe [Res]
getLinks sr = sr ^? (_Right . resultsLens) ^? _Just . _Just

doTheThing :: IO ()
doTheThing = do
  x <- plsCompose . fromJust <$> (getLinks <$> getAesoned)
  mapM_ putStrLn x

getAesoned :: IO (Either String SearchResult)
getAesoned = eitherDecode . B.pack <$> (D.encode . snd <$> searchPhotos "yoneda")

main :: IO ()
main = do
  doTheThing
