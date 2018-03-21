module Build ( Options(..), build )where

import Introit
import Data.List ( sortOn )

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
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
build Options
      { buildDir
      , postsDir
      , stylesDir
      , imagesDir
      , siteConfigFile }
      targets = do

    usingConfigFile siteConfigFile

    want targets

    getPost <- newCache readPost

    getAllMarkdownSourceFiles <- newCache $ \dir ->
        map (dir </>) <$> getDirectoryFiles dir ["*.md"]

    getAllPosts <- newCache $ \() -> do
        posts <- traverse getPost =<< getAllMarkdownSourceFiles postsDir
        return $ sortOn composed posts

    -- Specify our build targets.
    phony "build" $ do
        let pages = [R.Home, R.Archive]
        posts  <- map (R.Post . takeBaseName) <$>
            getAllMarkdownSourceFiles postsDir
        images <- map R.Image <$>
            getDirectoryContents imagesDir
        let styles = [R.Stylesheet "magenta"]
        let allTargets = pages <> posts <> images <> styles
        need $ map ((buildDir </>) . R.targetFile) allTargets

    phony "deploy" $ do
       (Stdout status) <- command [Cwd buildDir] "git" ["status", "--porcelain"]
       if length (lines status) > 0 then do
          command_ [Cwd buildDir] "git" ["add", "."]
          command_ [Cwd buildDir] "git" ["commit", "-a"]
          command_ [Cwd buildDir] "git" ["push"]
       else
          putQuiet "Nothing new to deploy!"

    templateRule buildDir R.Post $ \(R.Post slug) -> do
        thePost <- getPost (postsDir </> slug <.> "md")
        Templates.page (Just (title thePost)) (Templates.post thePost)

    templateRule buildDir (const R.Home) $ \R.Home -> do
       allPosts <- getAllPosts ()
       Templates.page Nothing (Templates.home allPosts)

    templateRule buildDir (const R.Archive) $ \R.Archive -> do
       allPosts <- getAllPosts ()
       Templates.page (Just "Archive") (Templates.archive allPosts)

    urlRule buildDir R.Stylesheet $ \route@(R.Stylesheet basename) -> do
        let src = stylesDir </> basename <.> "scss"
            file = buildDir </> R.targetFile route
        need [src]
        scssOrError <- liftIO $ Sass.compileFile src Sass.def
        either
          (throwFileError src <=< (liftIO . Sass.errorMessage))
          (liftIO . writeFile file)
          scssOrError

    urlRule buildDir R.Image $ \route@(R.Image filename) -> do
        let src = imagesDir </> filename
            file = buildDir </> R.targetFile route
        copyFile' src file
