module URLs where

import Introit
import qualified Text
import Network.URI ( URI, parseRelativeReference, relativeTo )

import Post
import Site

url :: Text -> SiteM URI
url slug Configuration{baseUrl} =
   fromJust (parseRelativeReference (Text.unpack ("/" <> slug <> ".html"))) `relativeTo` baseUrl

urlForPost :: Post -> SiteM URI
urlForPost =
   url . slug

homeUrl :: SiteM URI
homeUrl =
   url ""

archiveUrl :: SiteM URI
archiveUrl =
   url "archive"
