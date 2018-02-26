module Main where

import Introit
import qualified Text
import Control.Exception
import Data.Typeable ( Typeable )

import Data.Time.Format ( parseTimeM, defaultTimeLocale )
import Data.Yaml ( (.:) )
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.FilePath
import Network.URI ( parseAbsoluteURI )
import System.Directory ( createDirectoryIfMissing )
import qualified Cheapskate
import Text.Blaze.Html ( Html )
import qualified Text.Blaze.Html.Renderer.String as Blaze
import qualified Text.Sass as Sass

import Pages
import Post
import Site

buildDir :: FilePath
buildDir = "_site"

postsDir :: FilePath
postsDir = "posts"

configuration :: Site.Configuration
configuration = Configuration
   { siteTitle = "Mostly nonsense."
   -- TODO: This seems like a weird place to hardcode these urls.
   , baseUrl =
      fromJust (parseAbsoluteURI "https://parsonyorick.github.io/mostlynonsense/")
   , copyrightYear = 2018
   -- TODO this is dumb.
   , styleSheet = "magenta.css"
   -- TODO: See baseUrl above.
   , sourceUrl =
      fromJust (parseAbsoluteURI "https://github.com/parsonyorick/mostlynonsense/")
   }

main :: IO ()
main = do
  shakeVersion <- getHashedShakeVersion ["Shakefile.hs"]
  shakeArgs shakeOptions{shakeVersion} $ do

    getPost <- newCache readPostFromFile

    getAllPostSourceFiles <- fmap ($ ()) $ newCache $ \() ->
        map (postsDir </>) <$> getDirectoryFiles postsDir ["*.md"]

    getAllPosts <- fmap ($ ()) $ newCache $ \() -> do
        posts <- traverse getPost =<< getAllPostSourceFiles
        let (errors, successes) = partitionEithers posts
        -- We log the same errors individualy in getPost.
        return successes

    action $ do
        posts <-
            map (-<.> "html")
            <$> getAllPostSourceFiles
        styles <-
            map (("styles" </>) . (-<.> "css"))
            <$> getDirectoryFiles "styles" ["*.scss"]
        let pages =
                ["archive.html", "index.html"]
        need $ map (buildDir </>) (styles <> pages <> posts)

    (buildDir </> "posts/*.html") %> \out -> do
        let src = dropDirectory1 out -<.> "md"
        thisPostOrError <- getPost src
        ($ thisPostOrError)
         $ either (putQuiet . (("Error in " <> src <> ", namely: ") <>) . whatHappened)
         $ \thisPost -> do
            let html = Pages.post thisPost configuration
            renderHtmlToFile out html

    (buildDir </> "index.html") %> \out -> do
        allPosts <- getAllPosts
        renderHtmlToFile out (Pages.home allPosts configuration)

    (buildDir </> "archive.html") %> \out -> do
        allPosts <- getAllPosts
        renderHtmlToFile out (Pages.archive allPosts configuration)

    (buildDir </> "styles/*.css") %> \out -> do
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

renderHtmlToFile :: FilePath -> Html -> Action ()
renderHtmlToFile out markup = do
    let html = Blaze.renderHtml markup
    liftIO $ createDirectoryIfMissing True (takeDirectory out)
    liftIO $ writeFile out html

readPostFromFile :: FilePath -> Action (Either Whoops Post)
readPostFromFile filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    return $ do
      (meta, body) <- extractMetadataBlockAndBody contents
      reconstructPost meta body

 where
   extractMetadataBlockAndBody :: Text -> Either Whoops (Yaml.Value, Cheapskate.Doc)
   extractMetadataBlockAndBody stuff = do
      afterFirstMarker <-
         maybe (Left noMetadataBlockError) Right
         (Text.stripPrefix metadataBlockMarker stuff)
      let (metadataBlock, rest) =
            Text.breakOn metadataBlockMarker afterFirstMarker
      body <-
         maybe (Left noBodyError) Right
         (Text.stripPrefix metadataBlockMarker rest)
      yaml <-
         (first Whoops . Yaml.decodeEither . Text.encodeUtf8)
         metadataBlock
      let markdown =
            Cheapskate.markdown Cheapskate.def body
      return (yaml, markdown)

   metadataBlockMarker = "---"

   noMetadataBlockError = Whoops $
      "Expecting initial metadata block marker, namely \""
      <> Text.unpack metadataBlockMarker
      <> "\", in " <> filepath
      <> ", but it wasn't there."

   noBodyError = Whoops $
      "There is no text body following the metadata block in " <> filepath <> "."

   reconstructPost :: Yaml.Value -- ^ Markdown body.
                   -> Cheapskate.Doc -- ^ YAML metadata block.
                   -> Either Whoops Post
   reconstructPost yaml content = first Whoops $ ($ yaml) $ Yaml.parseEither $
      Yaml.withObject "metadata" $ \metadata -> do
         title    <- metadata .: "title"
         date     <- metadata .: "date"
         synopsis <- metadata .: "synopsis"
         composed <- parseTimeM True defaultTimeLocale dateFormat date
         let slug = (Text.pack . takeBaseName) filepath
         return Post{ title
                    , synopsis = Cheapskate.markdown Cheapskate.def synopsis
                    , slug
                    , composed
                    -- TODO: Distinguish these --- maybe.
                    , published = composed
                    , content }

   dateFormat = "%e %B %Y"

newtype Whoops = Whoops { whatHappened :: String } deriving ( Typeable )

instance Show Whoops where
   show = show . whatHappened

instance Exception Whoops
