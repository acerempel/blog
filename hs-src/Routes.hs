{-# LANGUAGE ViewPatterns, GADTs
           , DeriveGeneric, DeriveAnyClass
           , DuplicateRecordFields
#-}
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
data Post = Post
   { sourceDirectory :: FilePath
   , sourceExtension :: String
   , slug :: String }
   deriving ( Eq, Generic, Hashable )
data Stylesheet = Stylesheet
   { sourceDirectory :: FilePath
   , sourceExtension :: String
   , baseName :: String }
data Image = Image
   { sourceDirectory :: FilePath
   , sourceFileName :: String }

class Route route where
   url :: route -> Text
   url (targetFile -> file) =
      Text.pack ('/' : file)
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
   targetFile = htmlTargetFile
   sourceFile Home = noSourceFile

instance Route Archive where
   url Archive = "/archive"
   targetFile = htmlTargetFile
   sourceFile Archive = noSourceFile

noSourceFile = error "Don't have a source file!"

instance Route Post where
   url Post{ slug } =
      "/posts/" <> Text.pack slug
   targetFile = htmlTargetFile
   sourceFile (Post sourceDir sourceExt baseName) =
      sourceDir </> baseName <.> sourceExt

htmlTargetFile :: Route r => r -> FilePath
htmlTargetFile route =
   tail (Text.unpack (url route)) </> "index.html"

instance Route Stylesheet where
   targetFile Stylesheet{ baseName } =
      "styles" </> baseName <.> "css"
   sourceFile (Stylesheet sourceDir sourceExt baseName) =
      sourceDir </> baseName <.> sourceExt

instance Route Image where
   targetFile (Image _sourceDir filename) =
      "images" </> filename
   sourceFile (Image sourceDir filename) =
      sourceDir </> filename
