{-# LANGUAGE ViewPatterns, GADTs
           , DeriveGeneric, DeriveAnyClass #-}
module Routes
   ( Route(..)
   , SomeRoute(..)
   , Home(..)
   , Archive(..)
   , Post(..)
   , Stylesheet(..)
   , Image(..)
   ) where

import Introit
import qualified Text

import Data.Hashable ( Hashable )
import Development.Shake.FilePath
import GHC.Generics ( Generic )


data Home = Home
data Archive = Archive
data Post = Post FilePath deriving ( Eq, Generic, Hashable )
data Stylesheet = Stylesheet FilePath
data Image = Image FilePath

class Route route where
   url :: route -> Text
   targetFile :: route -> FilePath
   -- TODO: Separate typeclass for this! (See use of 'error' below.)
   sourceFile :: route -> FilePath

data SomeRoute where
   Route :: Route route => route -> SomeRoute

instance Route SomeRoute where
   url (Route r) = url r
   targetFile (Route r) = targetFile r
   sourceFile (Route r) = sourceFile r

instance Route Home where
   url Home = "/"
   targetFile Home = "index.html"
   sourceFile Home = error "Don't have a source file!"

instance Route Archive where
   url Archive = "/archive"
   targetFile Archive = "archive/index.html"
   sourceFile Archive = error "Don't have a source file!"

instance Route Post where
   url (Post (takeBaseName -> slug)) =
      "/posts/" <> Text.pack slug
   targetFile (Post (takeBaseName -> slug)) =
      "posts" </> slug </> "index.html"
   sourceFile (Post source) =
      source

instance Route Stylesheet where
   url (Stylesheet (takeFileName -> filename)) =
      "/styles/" <> Text.pack (filename -<.> "css")
   targetFile (Stylesheet (takeFileName -> filename)) =
      "styles" </> filename -<.> "css"
   sourceFile (Stylesheet source) =
      source -<.> "scss"

instance Route Image where
   url (Image (takeFileName -> filename)) =
      "/images/" <> Text.pack filename
   targetFile (Image (takeFileName -> filename)) =
      "images" </> filename
   sourceFile (Image source) =
      source
