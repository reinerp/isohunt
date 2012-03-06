{-# LANGUAGE DeriveGeneric #-}
{- |
The 'Response' data type. This should be imported qualified so as not
to conflict with the accessors from the 'Item' and 'Image' data types:

> import qualified IsoHunt.Response as Response
> import qualified IsoHunt.Item as Item
> import qualified IsoHunt.Image as Image
>
> ... Response.title r ... Item.title i ... Image.title im ...

These fields are mostly undocumented; see
<http://ca.isohunt.com/js/json.php?ihq=ubuntu&start=1&rows=4> for
an example response.

-}
module IsoHunt.Response(
  Response(..),
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import Data.Aeson
import Data.Text
import GHC.Generics
import Data.HashMap.Strict as H
import Data.Typeable
import IsoHunt.Item(Item)
import IsoHunt.Image(Image)

data Response
  = Response {
      title :: Text,
      link :: Text,
      category :: Text,
      pubDate :: Text,
      description :: Text,
      language :: Text,
      maxResults :: Integer,
      ttl :: Integer,
      image :: Image,
      lastBuildDate :: Text,
      totalResults :: Integer,
      items :: V.Vector Item, -- ^ search results
      censored :: Integer
 } deriving (Eq, Ord, Show, Typeable, Generic)

-- we supply a custom instance because:
--   * we want to change the names (totalResults vs total_results)
--   * the items field has a weird JSON representation
instance FromJSON Response where
  parseJSON (Object v) = Response <$>
      v .: "title" <*>
      v .: "link" <*>
      v .: "category" <*>
      v .: "pubDate" <*>
      v .: "description" <*>
      v .: "language" <*>
      v .: "max_results" <*>
      v .: "ttl" <*>
      v .: "image" <*>
      v .: "lastBuildDate" <*>
      v .: "total_results" <*>
      list v "items" <*>
      v .: "censored"
  parseJSON _ = mzero

list obj key = 
  case H.lookup key obj of
    Just (Object v) -> v .: "list"
    Just _ -> fail $ "key " ++ show key ++ " is not an object"
    Nothing -> fail $ "key " ++ show key ++ " not found"
