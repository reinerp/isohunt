{-# LANGUAGE DeriveGeneric #-}
{- |
The 'Item' data type. This should be imported qualified so as not
to conflict with the accessors from the 'Response' and 'Image' data types:

> import qualified IsoHunt.Response as Response
> import qualified IsoHunt.Item as Item
> import qualified IsoHunt.Image as Image
>
> ... Response.title r ... Item.title i ... Image.title im ...

These fields are mostly undocumented; see
<http://ca.isohunt.com/js/json.php?ihq=ubuntu&start=1&rows=4> for
an example response.
-}
module IsoHunt.Item(
  Item(..),
  ) where

import GHC.Generics
import Control.Applicative
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text
import Data.Typeable

data Item
 = Item {
     title :: Text,
     link :: Text,
     guid :: Text,
     enclosureUrl :: Text, -- ^ The link for the *.torrent file on
     -- isohunt's website, eg <http://ca.isohunt.com/download/52510650/ubuntu.torrent>
     length :: Integer, -- ^ Size in bytes
     tracker :: Text,
     trackerUrl :: Text,
     kws :: Text,
     exempts :: Text,
     category :: Text,
     originalSite :: Text,
     originalLink :: Text,
     size :: Text, -- ^ human-readable filesize (eg \"1.4GB\")
     files :: Integer,
     seeds :: Maybe Integer,
     leechers :: Maybe Integer,
     downloads :: Maybe Integer,
     votes :: Integer,
     comments :: Integer,
     hash :: Text,
     pubDate :: Text
  } deriving (Generic, Eq, Show, Ord, Typeable)

-- we supply a manual instance so that:
--   * we can handle Seeds, leechers, and downloads occasionally
--     appearing as "" instead of numbers
--   * we can change the names of the Haskell fields
instance FromJSON Item where
  parseJSON (Object v) =
    Item <$>
     v .: "title" <*>
     v .: "link" <*>
     v .: "guid" <*>
     v .: "enclosure_url" <*>
     v .: "length" <*>
     v .: "tracker" <*>
     v .: "tracker_url" <*>
     v .: "kws" <*>
     v .: "exempts" <*>
     v .: "category" <*>
     v .: "original_site" <*>
     v .: "original_link" <*>
     v .: "size" <*>
     v .: "files" <*>
     int v "Seeds" <*>
     int v "leechers" <*>
     int v "downloads" <*>
     v .: "votes" <*>
     v .: "comments" <*>
     v .: "hash" <*>
     v .: "pubDate"
    
int obj key = case H.lookup key obj of
               Nothing -> fail $ "key " ++ show key ++ " not present"
               Just (Number v) -> pure (Just $ floor v)
               Just _ -> pure Nothing
