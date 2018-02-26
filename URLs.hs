module URLs where

import Data.Text ( Text )
import Network.URI ( URI )

import Post
import Site

url :: Text -> SiteM URI
url = undefined

urlForPost :: Post -> SiteM URI
urlForPost =
   url . slug

homeUrl :: SiteM URI
homeUrl =
   url ""

archiveUrl :: SiteM URI
archiveUrl =
   url "archive"
