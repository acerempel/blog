{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Build ( Options(..), build ) where

import Introit
import Data.List ( sortOn )
import Data.Monoid ( Endo(..) )

import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( (!) )
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import qualified Text
import qualified Text.Sass as Sass

import Actions
import Post
import qualified Templates
import qualified Routes as R
import Utilities


data Options = Options
   { buildDir :: FilePath
   , postsDir :: FilePath
   , draftsDir :: FilePath
   , stylesDir :: FilePath
   , imagesDir :: FilePath
   , siteConfigFile :: FilePath 
   , includeDrafts :: Bool }


build :: Options -> [String] -> Rules ()
build Options { .. } _targets = do

    usingConfigFile siteConfigFile

    getPost <- newCache readPost

    getAllMarkdownSourceFiles <- newCache $ \dir ->
        map (dir </>) <$> getDirectoryFiles dir ["*.md"]

    getAllPosts <- newCache $ \() -> do
        posts <- traverse getPost =<< getAllMarkdownSourceFiles postsDir
        return $ sortOn composed posts

    getAllPostsByTag <- newCache $ \() -> do
        allPosts <- getAllPosts ()
        let catalogPost post =
              foldMap
                (\tag -> Endo $ Map.insertWith (<>) tag [post])
                (tags post)
        return $ appEndo
          (foldMap catalogPost allPosts)
          Map.empty

    -- Specify our build targets.
    action $ do
        let pages = [R.Home, R.AllTags]
        posts  <-
          map (R.Post . takeBaseName) <$> getAllMarkdownSourceFiles postsDir
        images <-
          map R.Image <$> getDirectoryContents imagesDir
        tags   <-
          map R.Tag . Map.keys <$> getAllPostsByTag ()
        let styles = [R.Stylesheet "magenta"]
        let allTargets = pages <> posts <> tags <> images <> styles
        need $ map ((buildDir </>) . R.targetFile) allTargets

    flip runReaderT buildDir $ do

    let titleOfSite = "[three dots]"

    templateRule R.Post $ \(R.Post slug) -> do
        thePost <- getPost (postsDir </> slug <.> "md")
        Templates.page
          (title thePost)
          (Templates.post thePost)

    templateRule (R.Tag . Text.pack) $ \(R.Tag tag) -> do
        postsByTag <- getAllPostsByTag ()
        let postsWithThisTag = postsByTag ! tag
        Templates.page
          ("Tagged as “" <> tag <> "”")
          (Templates.archive postsWithThisTag (Just tag))

    templateRule (const R.Home) $ \_ -> do
       allPosts <- getAllPosts ()
       Templates.page
        titleOfSite
        (Templates.archive allPosts Nothing)

    templateRule (const R.AllTags) $ \_ -> do
        allTags <- Map.toList . fmap length <$> getAllPostsByTag ()
        Templates.page
          "Tags"
          (Templates.tagsList allTags)

    urlRule R.Stylesheet $
      \route@(R.Stylesheet basename) buildDir -> do
        let src = stylesDir </> basename <.> "scss"
            file = buildDir </> R.targetFile route
        need [src]
        scssOrError <- liftIO $
          Sass.compileFile src Sass.def
        either
          (throwFileError src <=< (liftIO . Sass.errorMessage))
          (liftIO . writeFile file)
          scssOrError

    urlRule R.Image $
      \route@(R.Image filename) buildDir -> do
        let src = imagesDir </> filename
            file = buildDir </> R.targetFile route
        copyFile' src file
