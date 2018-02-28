module Routes ( urlForPost, urlForStylesheet, homeUrl, archiveUrl ) where

import Introit
import qualified Text
import Network.URI ( URI, parseRelativeReference )

import Post ( Post(slug, isDraft) )
import qualified Post
import Site


url :: MonadSite m => String -> m URI
url finalPiece = return $
   fromJust (parseRelativeReference ("/" <> finalPiece))

urlForPost :: MonadSite m => Post -> m URI
urlForPost Post.Post{ slug, isDraft } =
   url $ dir <> Text.unpack slug <> ".html"
 where
   -- TODO: Use System.FilePath operators?
   -- TODO: Should posts/ really be hardcoded here? The thing is it
   -- has to be the same as postsDir in Shakefile.hs.
   dir = if isDraft then "drafts/" else "posts/"

urlForStylesheet :: MonadSite m => FilePath -> m URI
urlForStylesheet ss =
   -- These already have stylesDir prepended.
   url ss

homeUrl :: MonadSite m => m URI
homeUrl =
   url ""

archiveUrl :: MonadSite m => m URI
archiveUrl =
   url "archive.html"
