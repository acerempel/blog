module Build ( Options(..), build )where

import Prelude hiding ( writeFile )
import Introit
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

import Pages
import Post
import Site


data Options = Options
   { buildDir :: FilePath
   , postsDir :: FilePath
   , draftsDir :: FilePath
   , stylesDir :: FilePath
   , imagesDir :: FilePath
   -- | Read this file for site configuration (see `getSiteConfig`).
   , siteConfigFile :: FilePath 
   , includeDrafts :: Bool }

build :: Options -> Rules ()
build Options
         { buildDir
         , postsDir
         , draftsDir
         , stylesDir
         , imagesDir
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
    getSiteConfig <- fmap ($ ()) $ newCache $ \() -> do
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

    let renderPostToFile :: FilePath -> Post -> Action ()
        renderPostToFile out thisPost =
           writeFile out =<<
              Site.withConfig (Pages.post thisPost) <$> getSiteConfig

    -- Specify our build targets.
    action $ do
        posts  <- map (-<.> "html") <$> getAllPostSourceFiles postsDir
        drafts <-
            if includeDrafts then
                  map (-<.> "html") <$> getAllPostSourceFiles draftsDir
            else return []
        styles <- getStylesheets
        images <- map (imagesDir </>) <$> getDirectoryContents imagesDir
        -- TODO: Should these filenames really be hardcoded? It works fine
        -- now of course, but is perhaps a little brittle.
        let pages = ["archive.html", "index.html"]
        need $ map (buildDir </>) (styles <> pages <> posts <> drafts <> images)

    (buildDir </> postsDir </> "*.html") %> \out -> do
        let src = dropDirectory1 out -<.> "md"
        thisPostOrError <- getPost src
        handleErrorOr src (renderPostToFile out) thisPostOrError

    (buildDir </> draftsDir </> "*.html") %> \out -> do
        let src = dropDirectory1 out -<.> "md"
        thisPostOrError <- getDraft src
        handleErrorOr src (renderPostToFile out) thisPostOrError

    (buildDir </> "index.html") %> \out -> do
        allPosts <- getAllPosts
        writeFile out =<<
            Site.withConfig (Pages.home allPosts) <$> getSiteConfig

    (buildDir </> "archive.html") %> \out -> do
        allPosts <- getAllPosts
        writeFile out =<<
            Site.withConfig (Pages.archive allPosts) <$> getSiteConfig

    (buildDir </> stylesDir </> "*.css") %> \out -> do
        let src = dropDirectory1 out -<.> "scss"
        need [src]
        scssOrError <- liftIO $
            bitraverse -- This is just massaging types.
               Sass.errorMessage
               (return . Text.pack)
            =<< Sass.compileFile src Sass.def
        handleErrorOr out (writeFile out) scssOrError

    (buildDir </> imagesDir </> "*") %> \out -> do
        let src = dropDirectory1 out
        copyFile' src out

type Whoops = String

handleErrorOr file ifSuccessful =
   either whoopsyDaisy ifSuccessful
 where
   whoopsyDaisy =
      putQuiet . (("Error in " <> file <> ", namely: ") <>)

writeFile :: FilePath -> Text -> Action ()
writeFile out html = liftIO $ do
    createDirectoryIfMissing True (takeDirectory out)
    Text.writeFile out html

readPostFromFile :: Bool -- ^ Whether this post is a draft.
                 -> FilePath -- ^ Path to the post (/including/ the postsDir).
                 -> Action (Either Whoops Post)
readPostFromFile isDraft filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    return $ do
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
         return Post{ title
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
