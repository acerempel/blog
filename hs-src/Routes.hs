{-# LANGUAGE DeriveGeneric, DeriveAnyClass
           , LambdaCase
#-}
module Routes
   ( Route(..)
   , targetFile
   , url
   ) where

import Introit
import qualified Text

import Data.Hashable ( Hashable )
import Development.Shake.FilePath
import GHC.Generics ( Generic )

data Route
   = Home
   | Archive
   | Post String
   | Stylesheet String
   | Image String
   deriving ( Eq, Generic, Hashable )

targetFile :: Route -> FilePath
targetFile = \case
   Home             -> htmlTargetFile Home
   Archive          -> htmlTargetFile Archive
   p@(Post _)       -> htmlTargetFile p
   s@(Stylesheet _) -> urlToTargetFile (url s)
   i@(Image _)      -> urlToTargetFile (url i)
 where
   urlToTargetFile url =
      tail (Text.unpack url)
   htmlTargetFile route =
      urlToTargetFile (url route) </> "index.html"

url :: Route -> Text
url = Text.pack . \case
   Home ->
      "/"
   Archive ->
      "/archive"
   Post slug ->
      "/posts" </> slug
   Stylesheet basename ->
      "/styles" </> basename <.> "css"
   Image filename ->
      "/images" </> filename
