{-# LANGUAGE LambdaCase #-}
module Routes
   ( Route(..)
   , ContentType(..)
   , targetFile
   , url
   ) where

import Introit
import qualified Text

import Data.Kind ( Type )
import System.FilePath ( (</>) )
import qualified Network.URI.Encode as URI

type Path = String
type TagName = Text

data ContentType = Html | CSS | Image

data Route :: ContentType -> Type where
  HomeR :: Route Html
  PageR :: Path -> Route Html
  AllTagsR :: Route Html
  TagR :: TagName -> Route Html
  StylesheetR :: Path -> Route CSS
  ImageR :: Path -> Route Image

targetFile :: Route a -> FilePath
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

url :: Route a -> Text
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
