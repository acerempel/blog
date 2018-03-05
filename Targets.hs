{-# LANGUAGE TypeFamilies
           , StandaloneDeriving
           , FlexibleInstances #-}
module Targets ( Which
               , This(..)
               , url
               , postSourceFile
               , file ) where

import Introit
import Data.Hashable
import System.FilePath
import Network.URI ( URI, parseRelativeReference )

import qualified Things


class Which thing where
   data family This thing
   url :: This thing -> URI
   url = makeUrl . file
   file :: This thing -> FilePath

deriving instance Eq (This Things.Post)

instance Hashable (This Things.Post) where
   hashWithSalt salt (Post slug) = hashWithSalt salt slug
   hash (Post slug) = hash slug
   
instance Which Things.Post where
   data This Things.Post = Post String
   file (Post s) = "posts" </> s <.> "html"

postSourceFile :: FilePath -> This Things.Post -> FilePath
postSourceFile postsDir (Post slug) = postsDir </> slug <.> "md"
   
instance Which Things.Home where
   data This Things.Home = Home
   url Home = makeUrl ""
   file Home = "index.html"

instance Which Things.Archive where
   data This Things.Archive = Archive
   file Archive = "archive.html"

makeUrl finalPiece =
   fromJust (parseRelativeReference ("/" <> finalPiece))
