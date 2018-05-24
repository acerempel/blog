{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Build ( Options(..), build )where

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
build Options { .. } targets = do

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
              foldMap (\tag -> Endo $ Map.insertWith (<>) tag [post])
                      (tags post)
        return $ appEndo (foldMap catalogPost allPosts) Map.empty

    -- Specify our build targets.
    action $ do
        let pages = [R.Home, R.Archive, R.AllTags]
        posts  <-
          map (R.Post . takeBaseName) <$> getAllMarkdownSourceFiles postsDir
        images <-
          map R.Image <$> getDirectoryContents imagesDir
        tags   <-
          map (R.Tag . Text.unpack) . Map.keys <$> getAllPostsByTag ()
        let styles = [R.Stylesheet "magenta"]
        let allTargets = pages <> posts <> tags <> images <> styles
        need $ map ((buildDir </>) . R.targetFile) allTargets

    flip runReaderT buildDir $ do

    templateRule R.Post $
      \(R.Post slug) -> do
        thePost <- getPost (postsDir </> slug <.> "md")
        Templates.page
          (Just (title thePost))
          (Templates.post thePost)

    templateRule R.Tag $
      \(R.Tag (Text.pack -> tag)) -> do
        postsByTag <- getAllPostsByTag ()
        let postsWithThisTag = postsByTag ! tag
        Templates.page
          (Just ("Tagged as " <> tag))
          (Templates.archive postsWithThisTag)

    templateRule (const R.Home) $
      \R.Home -> do
       allPosts <- getAllPosts ()
       Templates.page
        Nothing
        (Templates.home allPosts)

    templateRule (const R.Archive) $
      \R.Archive -> do
       allPosts <- getAllPosts ()
       Templates.page
        (Just "Archive")
        (Templates.archive allPosts)

    templateRule (const R.AllTags) $
      \R.AllTags -> do
        allTags <- Map.toList . fmap length <$> getAllPostsByTag ()
        Templates.page
          (Just "Tags")
          (Templates.tagsList allTags)

    urlRule R.Stylesheet $
      \route@(R.Stylesheet basename) buildDir -> do
        let src = stylesDir </> basename <.> "scss"
            file = buildDir </> R.targetFile route
        need [src]
        scssOrError <- liftIO $ Sass.compileFile src Sass.def
        either
          (throwFileError src <=< (liftIO . Sass.errorMessage))
          (liftIO . writeFile file)
          scssOrError

    urlRule R.Image $
      \route@(R.Image filename) buildDir -> do
        let src = imagesDir </> filename
            file = buildDir </> R.targetFile route
        copyFile' src file
