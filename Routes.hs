module Routes where

import Introit
import qualified Text
import Network.URI ( URI, parseRelativeReference )

import Post ( Post(slug) )
import Site


data Route
   = Home
   | Archive
   | Post Text
   | Stylesheet FilePath

url :: Route -> SiteM URI
url route _config =
   fromJust (parseRelativeReference ("/" <> finalPiece))
 where
   finalPiece =
      case route of
         Home      -> ""
         Archive   -> "archive.html"
         -- TODO: Use System.FilePath operators?
         Post slug -> "posts/" <> Text.unpack slug <> ".html"
         Stylesheet filename
                   -> filename

urlForPost :: Post -> SiteM URI
urlForPost =
   url . Post . slug

homeUrl :: SiteM URI
homeUrl =
   url Home

archiveUrl :: SiteM URI
archiveUrl =
   url Archive
