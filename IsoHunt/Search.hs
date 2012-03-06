{- | 
IsoHunt API; see <http://ca.isohunt.com/forum/viewtopic.php?p=433527#433527>.

Sample use:

> resp <- search (simpleQuery "ubuntu")

The following terms and conditions apply to the IsoHunt API, as stated
in the above link:

   In using our search API, you are free to do with it as you wish on condition that if your app is available publicly to users, you must link to torrent details pages on isoHunt.com, whether you link to the .torrent files or not. We reserve the right to ban you from using our API if you don't follow this simple rule. Refer to Louish's iPhone app for a good example of including links to our torrent details pages. Our torrent details pages have URLs like this: <http://isohunt.com/torrent_details/28289948/ubuntu?tab=summary>

   While we don't require developer tokens or place hard limits on api calls usage, excessive calls will also result in bans. If you think your app will consistently sustain multiple calls per second to our api, email admin at this site's domain first. 

  You are free to promote your app using our API, by replying under this post (<http://ca.isohunt.com/forum/viewtopic.php?p=433527#433527>) or post under this forum (<http://isohunt.com/forum/viewforum.php?f=19>). If your app is really good, we'll likely want to spotlight it on isoHunt's frontpage. Multiple posts to promote your app on our forum or comments is not allowed however, and will be treated as spam.

-}
module IsoHunt.Search( 
  -- * The main function
  search,
  -- * Query
  Query(..),
  simpleQuery,
  Sort(..),
  Order(..),
  -- * Response
  -- $qualifiedimports
  Response,
  Item,
  Image,
  -- * Exceptions
  MalformedJSON(..),
  MalformedResponse(..),
  ) where

import Data.ByteString.Lazy(ByteString)
import Control.Monad
import Control.Applicative
import           Data.Text(Text)
import qualified Data.Text
import qualified Data.Vector as V
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Typeable
import Data.Default
import Text.URI
import Control.Exception
import Network.HTTP.Conduit(simpleHttp)
import IsoHunt.Response
import IsoHunt.Item
import IsoHunt.Image

----------------------------- Queries --------------------------
data Sort
  = Composite 
    -- ^ Overall factors such as age, query relevance
    -- seed/leechers counts and vots
  | Seeds
    -- ^ Seeds + leechers
  | Age
  | Size
 deriving(Eq, Ord, Show, Typeable)

data Order
 = Descending
 | Ascending
  deriving(Eq, Ord, Show, Typeable)

-- | See also 'simpleQuery' and 'def' for constructing queries
data Query 
 = Query {
     searchTerm :: !String,
     start :: !Int, -- ^ start+rows <= 1000
     rows :: !Int, -- ^ <= 100
     sort :: !Sort,
     order :: !Order
     }
  deriving(Eq, Ord, Show, Typeable)

instance Default Query where
  def = Query "" 1 100 Composite Descending

-- | A default query for the given search term
simpleQuery :: String -> Query
simpleQuery s = def{searchTerm = s}

{- $qualifiedimports
The 'Response', 'Item', 'Image' data types all share some field names,
such as 'title'. To resolve ambiguity in these fields, use qualified 
imports as follows:

> import qualified IsoHunt.Response as Response
> import qualified IsoHunt.Item as Item
> import qualified IsoHunt.Image as Image
>
> ... Response.title r ... Item.title i ... Image.title im ...

The fields in these datatypes are mostly undocumented; see
<http://ca.isohunt.com/js/json.php?ihq=ubuntu&start=1&rows=4> for
an example response.

-}


-- | The response was invalid JSON. The unparsed contents are included.
data MalformedJSON = MalformedJSON !ByteString
  deriving(Show, Typeable)
instance Exception MalformedJSON

-- | The response was valid JSON, but not of the expected
-- format. Error message and the JSON value are included.
data MalformedResponse = MalformedResponse !String !Value
  deriving(Show, Typeable)
instance Exception MalformedResponse

{- | Search IsoHunt with the given query.

Throws 'MalformedJSON' or 'MalformedResponse' if the result is of an
expected format.
-}
search :: Query -> IO Response
search q = do
  resp <- simpleHttp (searchUrl q)
  case decode resp of
    Nothing -> throw (MalformedJSON resp)
    Just json -> 
      case fromJSON json of
        Success v -> return v
        Error msg -> throw (MalformedResponse msg json)

searchUrl :: Query -> String    
searchUrl q =
      "http://isohunt.com/js/json.php?" ++
      pairsToQuery (concat
        [
          [("ihq", searchTerm q),
           ("start", show $ start q),
           ("rows", show $ rows q),
           ("order", showOrder $ order q)
          ],
          if sort q == Composite 
          then []
          else [("sort", showSort $ sort q)]
        ])
      where
        showOrder Ascending = "asc"
        showOrder Descending = "desc"

        showSort Seeds = "seeds"
        showSort Age = "age"
        showSort Size = "size"
