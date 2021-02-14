{-# LANGUAGE DataKinds     #-}
-- | Module for shared utility functions needed by hush.
module Hush.Utils
  ( accessKey,
    defaultEnv,
    
    GetJSON,
    OptionalQueryParam,
    RequiredQueryParam,
    AuthHeader,
    Query,
    Page,
    PerPage,
    OrderByParam,
    ContentFilterParam,
    ColorParam,
    OrientationParam,
    Color (..),
    ContentFilter (..),
    OrderBy (..),
    Orientation (..),       
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

-- | Type synonym for the "query" parameters.
-- Search terms.
-- * <url>?query=val1&param=val2
-- TODO(dalp): Look into how to get `Query` to correspond to `NonEmpty Text`
-- or just find out if there's a way to assign a mode to `QueryParams` to make
-- it required therefore making an empty list illegal to use.
type Query = QueryParams "query" Text

-- | Type synonym for the "page" parameter.
-- Page number to retrieve. (default: 1)
-- * <url>?page=2
type Page = OptionalQueryParam "page" Int

-- | Type synonym for the "per_page" parameter, represents the number of items
-- in a page.
-- Number of items per page. (default: 10)
-- * <url>?per_page=2
type PerPage = OptionalQueryParam "per_page" Int

-- | Type synonym for the "order_by" parameter.
-- How to sort the photos. (default: relevant).
-- Valid values are latest and relevant.
-- * <url>?order_by=latest
type OrderByParam = OptionalQueryParam "order_by" OrderBy

-- | Type synonym for "content_filter" parameter.
-- Limit results by content safety. (default: low).
-- Valid values are low and high.
-- * <url>?content_filter=high
type ContentFilterParam = OptionalQueryParam "content_filter" ContentFilter

-- | Type synonym for "color" parameter.
-- Filter results by color. Valid values are:
-- black_and_white, black, white, yellow, orange, red, purple, magenta, green,
-- teal, and blue.
-- * <url>?color=orange
type ColorParam = OptionalQueryParam "color" Color

-- | Type synonym for "orientation" parameter.
-- Filter by photo orientation. Valid values: landscape, portrait, squarish
type OrientationParam = OptionalQueryParam "orientation" Orientation

------------------------------------------------------------------------------

-- | Ordering options available.
data OrderBy
  = OrderBy_Relevant
  | OrderBy_Latest

-- | >>> toUrlPiece Order_Relevant
-- "relevant"
instance ToHttpApiData OrderBy where
  toUrlPiece OrderBy_Relevant = "relevant"
  toUrlPiece OrderBy_Latest = "latest"

------------------------------------------------------------------------------

-- | Content filter options available.
data ContentFilter
  = ContentFilter_Low
  | ContentFilter_High
  
-- | >>> toUrlPiece ContentFilter_Low
-- "low"
instance ToHttpApiData ContentFilter where
  toUrlPiece ContentFilter_Low = "low"
  toUrlPiece ContentFilter_High = "high"

------------------------------------------------------------------------------

-- | Color options available.
data Color
  = Color_BlackAndWhite
  | Color_Black
  | Color_White
  | Color_Yellow
  | Color_Orange
  | Color_Red
  | Color_Purple
  | Color_Magenta
  | Color_Green
  | Color_Teal
  | Color_Blue

-- | >>> toUrlPiece Color_Black
-- "black"
instance ToHttpApiData Color where
  toUrlPiece Color_BlackAndWhite = "black_and_white"
  toUrlPiece Color_Black = "black"
  toUrlPiece Color_White = "white"
  toUrlPiece Color_Yellow = "yellow"
  toUrlPiece Color_Orange = "orange"
  toUrlPiece Color_Red = "red"
  toUrlPiece Color_Purple = "purple"
  toUrlPiece Color_Magenta = "magenta"
  toUrlPiece Color_Green = "green"
  toUrlPiece Color_Teal = "teal"
  toUrlPiece Color_Blue = "blue"

------------------------------------------------------------------------------

-- | Orientation options available.
data Orientation
  = Orientation_Landscape
  | Orientation_Portrait
  | Orientation_Squarish

-- | >>> toUrlPiece Orientation_Squarish
-- "squarish"
instance ToHttpApiData Orientation where
  toUrlPiece Orientation_Landscape = "landscape"
  toUrlPiece Orientation_Portrait = "portrait"
  toUrlPiece Orientation_Squarish = "squarish"

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
