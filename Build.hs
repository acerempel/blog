module Build where

import Introit
import Control.Exception
import Data.List ( sortOn )
import Data.Typeable ( Typeable )
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
import qualified Text.Sass as Sass

import Pages
import Post
import Site


data Options = Options
   { buildDir :: FilePath
   , postsDir :: FilePath
   , draftsDir :: FilePath
   , stylesDir :: FilePath
   -- | Read this file for site configuration (see `getSiteConfig`).
   , siteConfigFile :: FilePath 
   , includeDrafts :: Bool }

build :: Options -> Rules ()
build Options
         { buildDir
         , postsDir
         , draftsDir
         , stylesDir
         , siteConfigFile
         , includeDrafts
         } = do

    usingConfigFile siteConfigFile

    getPost <- newCache (readPostFromFile False)
    getDraft <- newCache (readPostFromFile True)

    getAllPostSourceFiles <- newCache $ \dir ->
        map (dir </>) <$> getDirectoryFiles dir ["*.md"]

    getAllPosts <- fmap ($ ()) $ newCache $ \() -> do
        posts <-
              traverse getPost =<< getAllPostSourceFiles postsDir
        drafts <-
           if includeDrafts then
              traverse getDraft =<< getAllPostSourceFiles draftsDir
           else return []
        -- We log the same errors individualy in getPost, so we can ignore
        -- them here.
        let (_errors, successes) = partitionEithers (posts <> drafts)
        let sortByDate = sortOn composed
        return $ sortByDate successes

    getStylesheets <- fmap ($ ()) $ newCache $ \() ->
        map ((stylesDir </>) . (-<.> "css"))
        <$> getDirectoryFiles stylesDir ["*.scss"]

    -- Get the site configuration.
    let getSiteConfig = do
         -- These are decidedly failable patterns --- how useful is the
         -- error message if they do fail?
         Just siteTitle <- fmap Text.pack
            <$> getConfig "site_title"
         Just baseUrl   <- (parseAbsoluteURI =<<)
            <$> getConfig "base_url"
         Just sourceUrl <- (parseAbsoluteURI =<<)
            <$> getConfig "source_url"
         copyrightYear  <- fromMaybe 2018 <$> fmap read
            <$> getConfig "copyright"
         author         <- Text.pack <$> fromMaybe "Anonymous"
            <$> getConfig "author"
         styleSheets    <- getStylesheets
         return Site.Configuration
            { siteTitle
            , baseUrl
            , sourceUrl
            , copyrightYear
            , author
            , styleSheets }

    -- Specify our build targets.
    action $ do
        posts  <- map (-<.> "html") <$> getAllPostSourceFiles postsDir
        drafts <-
            if includeDrafts then
                  map (-<.> "html") <$> getAllPostSourceFiles draftsDir
            else return []
        styles <- getStylesheets
        -- TODO: Should these filenames really be hardcoded? It works fine
        -- now of course, but is perhaps a little brittle.
        let pages = ["archive.html", "index.html"]
        need $ map (buildDir </>) (styles <> pages <> posts <> drafts)

    (buildDir </> postsDir </> "*.html") %> \out -> do
        let src = dropDirectory1 out -<.> "md"
        thisPostOrError <- getPost src
        ($ thisPostOrError)
         $ either (putQuiet . (("Error in " <> src <> ", namely: ") <>) . whatHappened)
         $ \thisPost -> do
            renderHtmlToFile out =<<
                Site.withConfig (Pages.post thisPost) <$> getSiteConfig

    (buildDir </> draftsDir </> "*.html") %> \out -> do
        let src = dropDirectory1 out -<.> "md"
        thisPostOrError <- getDraft src
        ($ thisPostOrError)
         $ either (putQuiet . (("Error in " <> src <> ", namely: ") <>) . whatHappened)
         $ \thisPost ->
            renderHtmlToFile out =<<
                Site.withConfig (Pages.post thisPost) <$> getSiteConfig

    (buildDir </> "index.html") %> \out -> do
        allPosts <- getAllPosts
        renderHtmlToFile out =<<
            Site.withConfig (Pages.home allPosts) <$> getSiteConfig

    (buildDir </> "archive.html") %> \out -> do
        allPosts <- getAllPosts
        renderHtmlToFile out =<<
            Site.withConfig (Pages.archive allPosts) <$> getSiteConfig

    (buildDir </> stylesDir </> "*.css") %> \out -> do
        let src = dropDirectory1 out -<.> "scss"
        need [src]
        scssOrError <- liftIO $ Sass.compileFile src Sass.def
        case scssOrError of
            Left err -> do
                message <- liftIO $ Sass.errorMessage err
                putQuiet ("Error in " <> src <> ", namely: " <> show message)
            Right scss -> do
                liftIO $ createDirectoryIfMissing True (takeDirectory out)
                liftIO $ writeFile out scss


renderHtmlToFile :: FilePath -> Text -> Action ()
renderHtmlToFile out html = liftIO $ do
    createDirectoryIfMissing True (takeDirectory out)
    Text.writeFile out html

readPostFromFile :: Bool -- ^ Whether this post is a draft.
                 -> FilePath -- ^ Path to the post (/including/ the postsDir).
                 -> Action (Either Whoops Post)
readPostFromFile isDraft filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    return $ do
      body <- first (Whoops . MMark.parseErrorsPretty contents) $
         MMark.parse filepath contents
      yaml <- maybe (Left noMetadataError) Right $
         MMark.projectYaml body
      withMetadata body yaml
 where
   withMetadata content = (first Whoops .) $ Yaml.parseEither $
      Yaml.withObject "metadata" $ \metadata -> do
         title    <- metadata .: "title"
         date     <- metadata .: "date"
         synopsis <- metadata .: "synopsis"
         composed <- parseTimeM True defaultTimeLocale dateFormat date
         let slug = (Text.pack . takeBaseName) filepath
         return Post{ title
                    , synopsis
                    , slug
                    , composed
                    -- TODO: Distinguish these --- maybe.
                    , published = composed
                    , isDraft
                    , content }
   
   noMetadataError = Whoops $
      "Couldn't find a metadata block in file " <> filepath

   dateFormat = "%e %B %Y"


newtype Whoops = Whoops { whatHappened :: String } deriving ( Typeable )

instance Show Whoops where
   show = show . whatHappened

instance Exception Whoops
