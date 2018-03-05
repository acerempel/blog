{-# LANGUAGE RankNTypes #-}
module Actions ( Context(..), post, home, archive ) where
   
import Introit

import Development.Shake

import qualified Render
import Things
import Site
import Targets


data Context = Context
   { getAllPostTargets :: FilePath -- ^ Directory in which to find post sources
                       -> Action [This Post]
   , getAllPosts :: Bool -- ^ Include drafts?
                 -> Action [Post]
   , getPost :: This Post -> Action Post
   , getDraft :: This Post -> Action Post
   , getSiteConfig :: Action Configuration
   , getStylesheets :: Action [FilePath]
   , writeTarget :: forall thing. Which thing => This thing -> Text -> Action ()
   }

post :: Context -> This Post -> Action ()
post Context{ getPost, getSiteConfig, writeTarget } thisOne = do
   thePost <- getPost thisOne
   config <- getSiteConfig
   let html = Render.post thePost `withConfig` config
   writeTarget thisOne html

home :: Context -> Action ()
home Context{ getAllPosts, getSiteConfig, writeTarget } = do
   config <- getSiteConfig
   allPosts <- getAllPosts (includeDrafts config)
   let html = Render.home allPosts `withConfig` config
   writeTarget Targets.Home html

archive :: Context -> Action ()
archive Context{ getAllPosts, getSiteConfig, writeTarget } = do
   config <- getSiteConfig
   allPosts <- getAllPosts (includeDrafts config)
   let html = Render.archive allPosts `withConfig` config
   writeTarget Targets.Archive html
