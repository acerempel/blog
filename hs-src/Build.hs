module Build ( Options(..), build )where

import Introit
import Data.Bitraversable
import Data.List ( sortOn )
import qualified Text

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import qualified Text.Sass as Sass

import Actions
import Post
import Templates ( liftAction, getThisURL )
import qualified Templates
import Utilities


data Options = Options
   { buildDir :: FilePath
   , postsDir :: FilePath
   , draftsDir :: FilePath
   , stylesDir :: FilePath
   , imagesDir :: FilePath
   , siteConfigFile :: FilePath 
   , includeDrafts :: Bool }


createContext :: Options -> Rules Context
createContext Options
         { buildDir
         , postsDir
         , stylesDir
         , imagesDir
         , siteConfigFile
         } = do

    usingConfigFile siteConfigFile

    getPost <- newCache readPostFromFile

    getAllMarkdownSourceFiles <- newCache $ \dir ->
        map (dir </>) <$> getDirectoryFiles dir ["*.md"]

    getAllPosts <- newCache $ \() -> do
        posts <- traverse getPost =<< getAllMarkdownSourceFiles postsDir
        let sortByDate = sortOn composed
        return $ sortByDate posts

    return Context{ getAllMarkdownSourceFiles
                  , getAllPosts
                  , getPost }


build :: Options -> [String] -> Rules ()
build options@Options
      { buildDir
      , postsDir
      , draftsDir
      , stylesDir
      , imagesDir
      , siteConfigFile
      } targets = do

    context@Context{ getAllMarkdownSourceFiles
                   , getAllPosts
                   , getPost } <-
      createContext options

    want targets

    -- Specify our build targets.
    phony "build" $ do
        let pages = ["/", "/archive"]
        posts  <- map (Text.pack . ("/" <>) . dropExtension) <$>
            getAllMarkdownSourceFiles postsDir
        images <- map (Text.pack . ("/images" </>)) <$>
            getDirectoryContents imagesDir
        let styles = ["/styles/magenta.css"]
        let allTargets = pages <> posts <> images <> styles
        need $ map (urlToTargetFile buildDir) allTargets

    -- phony "deploy" $ do
    --    (StdOut status) <- cmd "git status --porcelain" [CurDir buildDir]
    --    if not (null status) then do
    --       cmd_ "git add ."
    --       cmd_ "git commit"
    --       cmd_ "git push"
    --    else
    --       putQuiet "Nothing new to deploy!"

    templateRule buildDir "/posts/*" $ do
        thisOne <- getThisURL
        thePost <- liftAction (getPost (tail (Text.unpack thisOne) <.> "md"))
        Templates.page (Just (title thePost)) (Templates.post thePost)

    templateRule buildDir "/" $ do
       allPosts <- liftAction (getAllPosts ())
       Templates.page Nothing (Templates.home allPosts)

    templateRule buildDir "/archive" $ do
       allPosts <- liftAction (getAllPosts ())
       Templates.page (Just "Archive") (Templates.archive allPosts)

    urlRule buildDir "/styles/*.css" $ \url file -> do
        let src = dropDirectory1 $ file -<.> "scss"
        need [src]
        scssOrError <- liftIO $ Sass.compileFile src Sass.def
        either
          (throwFileError file <=< (liftIO . Sass.errorMessage))
          (liftIO . writeFile file)
          scssOrError

    urlRule buildDir "/images/*.jpg" $ \url file -> do
        let src = imagesDir </> takeFileName file
        copyFile' src file
