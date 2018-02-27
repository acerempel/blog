module Routes ( urlForPost, urlForStylesheet, homeUrl, archiveUrl ) where

import Introit
import qualified Text
import Network.URI ( URI, parseRelativeReference )

import Post ( Post(slug, isDraft) )
import qualified Post
import Site


url :: String -> SiteM URI
url finalPiece _config =
   fromJust (parseRelativeReference ("/" <> finalPiece))

urlForPost :: Post -> SiteM URI
urlForPost Post.Post{ slug, isDraft } =
   url $ dir <> Text.unpack slug <> ".html"
 where
   -- TODO: Use System.FilePath operators?
   -- TODO: Should posts/ really be hardcoded here? The thing is it
   -- has to be the same as postsDir in Shakefile.hs.
   dir = if isDraft then "drafts/" else "posts/"

urlForStylesheet :: FilePath -> SiteM URI
urlForStylesheet ss =
   -- These already have stylesDir prepended.
   url ss

homeUrl :: SiteM URI
homeUrl =
   url ""

archiveUrl :: SiteM URI
archiveUrl =
   url "archive.html"
