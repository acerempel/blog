{-# LANGUAGE LambdaCase #-}
module Routes
   ( Route(..)
   , targetFile
   , url
   ) where

import Introit
import qualified Text

import System.FilePath ( (</>) )
import qualified Network.URI.Encode as URI

type Path = String

data Route
   = HomeR
   | PageR Path
   | AllTagsR
   | TagR Text
   | StylesheetR Path
   | ImageR Path

targetFile :: Route -> FilePath
targetFile = \case
   HomeR             -> htmlTargetFile HomeR
   p@(PageR _)       -> htmlTargetFile p
   AllTagsR          -> htmlTargetFile AllTagsR
   t@(TagR _)        -> htmlTargetFile t
   s@(StylesheetR _) -> urlToTargetFile (url s)
   i@(ImageR _)      -> urlToTargetFile (url i)
 where
   urlToTargetFile url =
     -- This is dumb, see below
      URI.decode $ tail (Text.unpack url)
   htmlTargetFile route =
      urlToTargetFile (url route) </> "index.html"

url :: Route -> Text
url = Text.pack . \case
   HomeR ->
      "/"
   PageR path ->
      "/" </> path
   AllTagsR ->
      "/tags"
   TagR tag ->
      "/tags" </> Text.unpack (URI.encodeText tag)
   StylesheetR path ->
      "/" </> path
   ImageR path ->
      "/" </> path
