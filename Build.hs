module Build ( Options(..), build )where

import Prelude hiding ( writeFile )
import Introit
import Control.Exception ( throwIO )
import Data.Bitraversable
import Data.List ( sortOn )
import qualified Text

import Data.Time.Format ( parseTimeM, defaultTimeLocale )
import Data.Yaml ( (.:) )
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Network.URI ( parseAbsoluteURI )
import System.Directory ( createDirectoryIfMissing )
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as MMark
import qualified Text.Sass as Sass

import Actions
import Targets
import Things
import Site hiding ( includeDrafts )
import qualified Site


data Options = Options
   { buildDir :: FilePath
   , postsDir :: FilePath
   , draftsDir :: FilePath
   , stylesDir :: FilePath
   , imagesDir :: FilePath
   -- | Read this file for site configuration (see `getSiteConfig`).
   , siteConfigFile :: FilePath 
   , includeDrafts :: Bool }

createContext :: Options -> Rules Context
createContext Options
         { buildDir
         , postsDir
         , draftsDir
         , stylesDir
         , imagesDir
         , siteConfigFile
         } = do

    usingConfigFile siteConfigFile

    getPost <- newCache (readPostFromFile False . Targets.postSourceFile postsDir)
    getDraft <- newCache (readPostFromFile True . Targets.postSourceFile draftsDir)

    getAllPostTargets <- newCache $ \dir ->
        map (Targets.Post . takeBaseName) <$> getDirectoryFiles dir ["*.md"]

    getAllPosts <- newCache $ \includeDrafts -> do
        posts <-
              traverse getPost =<< getAllPostTargets postsDir
        drafts <-
           if includeDrafts then
              traverse getDraft =<< getAllPostTargets draftsDir
           else return []
        let sortByDate = sortOn composed
        return $ sortByDate (posts <> drafts)

    getStylesheets <- fmap ($ ()) $ newCache $ \() ->
        map ((stylesDir </>) . (-<.> "css"))
        <$> getDirectoryFiles stylesDir ["*.scss"]

    -- Get the site configuration.
    getSiteConfig <- fmap ($ ()) $ newCache $ \() -> do
         -- These are decidedly failable patterns --- how useful is the
         -- error message if they do fail?
         Just siteTitle <- fmap Text.pack
            <$> getConfig "site_title"
         Just baseUrl   <- (parseAbsoluteURI =<<)
            <$> getConfig "base_url"
         Just sourceUrl <- (parseAbsoluteURI =<<)
            <$> getConfig "source_url"
         copyrightYear  <- maybe 2018 read
            <$> getConfig "copyright"
         author         <- maybe "Anonymous" Text.pack
            <$> getConfig "author"
         includeDrafts  <- maybe False read
            <$> getConfig "include_drafts"
         stylesheet     <- fromMaybe "main.css"
            <$> getConfig "stylesheet"
         return Site.Configuration
            { siteTitle
            , baseUrl
            , sourceUrl
            , copyrightYear
            , author
            , stylesheet
            , Site.includeDrafts }

    let writeTarget :: Which thing => This thing -> Text -> Action ()
        writeTarget thisOne text = liftIO $ do
            let targetFile = buildDir </> Targets.file thisOne
            createDirectoryIfMissing True (takeDirectory targetFile)
            Text.writeFile targetFile text 

    return Context{ getAllPostTargets
                  , getAllPosts
                  , getPost
                  , getDraft
                  , getSiteConfig
                  , getStylesheets
                  , writeTarget }

build :: Options -> Rules ()
build options@Options
      { buildDir
      , postsDir
      , draftsDir
      , stylesDir
      , imagesDir
      , siteConfigFile
      } = do

    context@Context{ getAllPostTargets, writeTarget } <-
      createContext options

    -- Specify our build targets.
    action $ do
        posts  <- map Targets.file <$> getAllPostTargets postsDir
        drafts <- map Targets.file <$> getAllPostTargets draftsDir
        styles <- getStylesheets context
        images <- map (imagesDir </>) <$> getDirectoryContents imagesDir
        let pages = [Targets.file Targets.Home, Targets.file Targets.Archive]
        need $ map (buildDir </>) (styles <> pages <> posts <> drafts <> images)

    let buildThus target recipe =
           (buildDir </> Targets.file target) %> recipe

    Targets.Post "*" `buildThus` \out ->
        Actions.post context (Targets.Post (takeBaseName out))

    Targets.Home `buildThus` \_out -> Actions.home context

    Targets.Archive `buildThus` \_out -> Actions.archive context

    Targets.Stylesheet "*.css" `buildThus` \out -> do
        let src = stylesDir </> takeBaseName out -<.> "scss"
        need [src]
        scssOrError <- liftIO $
            bitraverse -- This is just massaging types.
               Sass.errorMessage
               (return . Text.pack)
            =<< Sass.compileFile src Sass.def
        either (throwError out) (writeTarget (Targets.Stylesheet src)) scssOrError

    (buildDir </> imagesDir </> "*") %> \out -> do
        let src = dropDirectory1 out
        copyFile' src out

throwError :: FilePath -> String -> Action a
throwError file problem = liftIO $ throwIO $ userError $
   "Error in " <> file <> ", namely: " <> problem

readPostFromFile :: Bool -- ^ Whether this post is a draft.
                 -> FilePath -- ^ Path to the post (/including/ the postsDir).
                 -> Action Post
readPostFromFile isDraft filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    either (throwError filepath) return $ do
      body <-
         first (MMark.parseErrorsPretty contents) $
         second (MMark.useExtension hyphensToDashes) $
         MMark.parse filepath contents
      yaml <- maybe (Left noMetadataError) Right $
         MMark.projectYaml body
      withMetadata body yaml
 where
   withMetadata content = Yaml.parseEither $
      Yaml.withObject "metadata" $ \metadata -> do
         title    <- metadata .: "title"
         date     <- metadata .: "date"
         synopsis <- metadata .: "synopsis"
         composed <- parseTimeM True defaultTimeLocale dateFormat date
         let slug = (Text.pack . takeBaseName) filepath
         return Things.Post
                    { title
                    , synopsis
                    , slug
                    , composed
                    -- TODO: Distinguish these --- maybe.
                    , published = composed
                    , isDraft
                    , content }

   hyphensToDashes :: MMark.Extension
   hyphensToDashes = MMark.inlineTrans $
      MMark.mapInlineRecursively $
      MMark.mapInlineText $
      Text.replace "--" "–" .
      Text.replace "---" "—"

   noMetadataError =
      "Couldn't find a metadata block in file " <> filepath

   dateFormat = "%e %B %Y"
