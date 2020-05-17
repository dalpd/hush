{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hush.Types
  ( SearchResult (..)
  , Res (..)
  , resultsLens
  , urlsLens
  , fullLens
  ) where

------------------------------------------------------------------------------

import           Control.Lens
import           Data.Aeson
import           GHC.Generics

------------------------------------------------------------------------------
data SearchResult = SearchResult
  { total       :: Int
  , total_pages :: Int
  , results     :: Maybe [Res]
  } deriving (Generic, Show)

    
------------------------------------------------------------------------------
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
  } deriving (Generic, Show)


------------------------------------------------------------------------------
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
  } deriving (Generic, Show)


------------------------------------------------------------------------------
data Image = Image
           { small  :: String
           , medium :: String
           , large  :: String
           }
           deriving (Generic, Show)


------------------------------------------------------------------------------
data ResultLinks = ResultLinks
           { self    :: String
           , html    :: String
           , download :: String
           }
           deriving (Generic, Show)


------------------------------------------------------------------------------
data UserLinks = UserLinks
           { self    :: String
           , html    :: String
           , photos  :: String
           , likes :: String
           }
           deriving (Generic, Show)


------------------------------------------------------------------------------
data URLs = URLs
          { raw     :: String
          , full    :: String
          , regular :: String
          , small   :: String
          , thumb   :: String
          }
          deriving (Generic, Show)


------------------------------------------------------------------------------
resultsLens :: Lens' SearchResult (Maybe [Res])
resultsLens = lens results (\search_result s -> search_result { results = s})

urlsLens :: Lens' Res URLs
urlsLens = lens urls (\result r -> result { urls = r })

fullLens :: Lens' URLs String
fullLens = lens full (\url u -> url { full = u })


------------------------------------------------------------------------------
instance FromJSON SearchResult
instance FromJSON Res
instance FromJSON User
instance FromJSON Image
instance FromJSON ResultLinks
instance FromJSON UserLinks
instance FromJSON URLs


