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
import Routes ( SomeRoute(..) )
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
      , siteConfigFile
      } targets = do

    usingConfigFile siteConfigFile

    getPost <- newCache readPost

    getAllPostRoutes <- newCache $ \dir ->
        map (R.Post . (dir </>)) <$> getDirectoryFiles dir ["*.md"]

    getAllPosts <- newCache $ \() -> do
        posts <- traverse getPost =<< getAllPostRoutes postsDir
        return $ sortOn composed posts

    want targets

    -- Specify our build targets.
    phony "build" $ do
        let pages = [Route R.Home, Route R.Archive]
        posts  <- map Route <$> getAllPostRoutes postsDir
        images <- map (Route . R.Image . (imagesDir </>)) <$>
            getDirectoryContents imagesDir
        let styles = [Route (R.Stylesheet (stylesDir </> "magenta.scss"))]
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

    templateRule buildDir
      (\slug -> R.Post (postsDir </> slug <.> "md")) $
      \thisOne -> do
        thePost <- getPost thisOne
        Templates.page (Just (title thePost)) (Templates.post thePost)

    templateRule buildDir (const R.Home) $ \_homeRoute -> do
       allPosts <- getAllPosts ()
       Templates.page Nothing (Templates.home allPosts)

    templateRule buildDir (const R.Archive) $ \_archiveRoute -> do
       allPosts <- getAllPosts ()
       Templates.page (Just "Archive") (Templates.archive allPosts)

    urlRule buildDir
      (R.Stylesheet . (\basename -> stylesDir </> basename <.> "css")) $
      \route -> do
        let src = R.sourceFile route
            file = buildDir </> R.targetFile route
        need [src]
        scssOrError <- liftIO $ Sass.compileFile src Sass.def
        either
          (throwFileError file <=< (liftIO . Sass.errorMessage))
          (liftIO . writeFile file)
          scssOrError

    urlRule buildDir (R.Image . (imagesDir </>)) $ \route -> do
        let src = R.sourceFile route
            file = buildDir </> R.targetFile route
        copyFile' src file
