{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hush.Types
  ( SearchResult(..)
  , Res(..)
  ) where

------------------------------------------------------------------------------

import           Data.Aeson

------------------------------------------------------------------------------
data SearchResult = SearchResult
  { total       :: Int
  , total_pages :: Int
  , results     :: [Res]
  } deriving (Show)

instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \o -> SearchResult
    <$> o .: "total"
    <*> o .: "total_pages"
    <*> o .: "results"


------------------------------------------------------------------------------
data Res = Res
  { id :: String
  , created_at :: String
  , updated_at :: String
  , promoted_at :: Maybe String
  , width  :: Int
  , height :: Int
  , color  :: String
  , description :: Maybe String
  , alt_description :: Maybe String
  , urls  :: URLs
  , links :: ResultLinks
  , categories :: [String]
  , likes  :: Int
  , liked_by_user :: Bool
  , current_user_collections :: [String]
  , sponsorship :: Maybe String
  , user  :: User
  , tags :: Maybe [Tag]
  } deriving (Show)

instance FromJSON Res where
  parseJSON = withObject "Res" $ \o -> Res
    <$> o .: "id"
    <*> o .: "created_at"
    <*> o .: "updated_at"
    <*> o .: "promoted_at"
    <*> o .: "width"
    <*> o .: "height"
    <*> o .: "color"
    <*> o .: "description"
    <*> o .: "alt_description"
    <*> o .: "urls"
    <*> o .: "links"
    <*> o .: "categories"
    <*> o .: "likes"
    <*> o .: "liked_by_user"
    <*> o .: "current_user_collections"
    <*> o .:? "sponsorship"
    <*> o .: "user"
    <*> o .:? "tags"


------------------------------------------------------------------------------
data URLs = URLs
  { raw     :: String
  , full    :: String
  , regular :: String
  , small   :: String
  , thumb   :: String
  } deriving (Show)

instance FromJSON URLs where
  parseJSON = withObject "URLs" $ \o -> URLs
    <$> o .: "raw"
    <*> o .: "full"
    <*> o .: "regular"
    <*> o .: "small"
    <*> o .: "thumb"


------------------------------------------------------------------------------
data ResultLinks = ResultLinks
  { self    :: String
  , html    :: String
  , download :: String
  , download_location :: String
  } deriving (Show)

instance FromJSON ResultLinks where
  parseJSON = withObject "ResultLinks" $ \o -> ResultLinks
    <$> o .: "self"
    <*> o .: "html"
    <*> o .: "download"
    <*> o .: "download_location"


------------------------------------------------------------------------------
data User = User
  { id :: String
  , updated_at :: String
  , username :: String
  , name     :: String
  , first_name :: String
  , last_name  :: Maybe String
  , twitter_username :: Maybe String
  , portfolio_url  :: Maybe String
  , bio :: Maybe String
  , location :: Maybe String
  , links     :: UserLinks
  , profile_image :: Maybe Image
  , instagram_username :: Maybe String
  , total_collections :: Int
  , total_likes :: Int
  , total_photos :: Int
  , accepted_tos :: Bool
  } deriving (Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "id"
    <*> o .: "updated_at"
    <*> o .: "username"
    <*> o .: "name"
    <*> o .: "first_name"
    <*> o .: "last_name"
    <*> o .: "twitter_username"
    <*> o .: "portfolio_url"
    <*> o .: "bio"
    <*> o .: "location"
    <*> o .: "links"
    <*> o .: "profile_image"
    <*> o .: "instagram_username"
    <*> o .: "total_collections"
    <*> o .: "total_likes"
    <*> o .: "total_photos"
    <*> o .: "accepted_tos"


------------------------------------------------------------------------------
data UserLinks = UserLinks
  { self    :: String
  , html    :: String
  , photos  :: String
  , likes :: String
  , portfolio :: String
  , following :: String
  , followers :: String
  } deriving (Show)

instance FromJSON UserLinks where
  parseJSON = withObject "UserLinks" $ \o -> UserLinks
    <$> o .: "self"
    <*> o .: "html"
    <*> o .: "photos"
    <*> o .: "likes"
    <*> o .: "portfolio"
    <*> o .: "following"
    <*> o .: "followers"


------------------------------------------------------------------------------
data Image = Image
  { small  :: String
  , medium :: String
  , large  :: String
  } deriving (Show)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \o -> Image
    <$> o .: "small"
    <*> o .: "medium"
    <*> o .: "large"


------------------------------------------------------------------------------
data Tag = Tag
  { type' :: String
  , title :: String
  , source :: Maybe Source
  } deriving (Show)

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \o -> Tag
    <$> o .: "type"
    <*> o .: "title"
    <*> o .:? "source"


------------------------------------------------------------------------------
data Source = Source
  { ancestry :: Ancestry
  , title :: String
  , subtitle :: Maybe String
  , description :: Maybe String
  , meta_title :: Maybe String
  , meta_description :: Maybe String
  , cover_photo :: Res
  } deriving (Show)

instance FromJSON Source where
  parseJSON = withObject "Source" $ \o -> Source
    <$> o .: "ancestry"
    <*> o .: "title"
    <*> o .: "subtitle"
    <*> o .: "description"
    <*> o .: "meta_title"
    <*> o .: "meta_description"
    <*> o .: "cover_photo"


------------------------------------------------------------------------------
data Ancestry = Ancestry
  { type' :: Type
  , category :: Type
  , subcategory :: Type
  } deriving Show

instance FromJSON Ancestry where
  parseJSON = withObject "Ancestry" $ \o -> Ancestry
    <$> o .: "type"
    <*> o .: "category"
    <*> o .: "subcategory"


------------------------------------------------------------------------------
data Type = Type
  { slug :: String
  , pretty_slug :: String
  } deriving Show

instance FromJSON Type where
  parseJSON = withObject "Type" $ \o -> Type
    <$> o .: "slug"
    <*> o .: "pretty_slug"
