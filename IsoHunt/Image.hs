{-# LANGUAGE DeriveGeneric #-}
{- |
The 'Image' data type. This should be imported qualified so as not
to conflict with the accessors from the 'Response' and 'Item' data types:

> import qualified IsoHunt.Response as Response
> import qualified IsoHunt.Item as Item
> import qualified IsoHunt.Image as Image
>
> ... Response.title r ... Item.title i ... Image.title im ...

These fields are mostly undocumented; see
<http://ca.isohunt.com/js/json.php?ihq=ubuntu&start=1&rows=4> for
an example response.

-}
module IsoHunt.Image( 
  Image(..),
  ) where

import GHC.Generics
import Data.Text
import Data.Aeson
import Data.Typeable

data Image = 
  Image {
    title :: Text,
    url :: Text,
    link :: Text,
    width :: Integer,
    height :: Integer
    } deriving(Eq, Ord, Show, Typeable, Generic)

instance FromJSON Image
