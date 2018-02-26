module Main where

import Control.Exception
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Typeable ( Typeable )

import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
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
        -- Log the errors to the console.
        for_ errors (putQuiet . (\err -> "Error: " <> show err))
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
    return (readPost filepath contents)

readPost :: FilePath -> Text -> Either Whoops Post
readPost filepath filecontents = do
   (meta, body) <- extractMetadataBlockAndBody filecontents
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

   metadataBlockMarker =
      "---"

   noMetadataBlockError =
      Whoops $ "Expecting initial metadata block marker, namely \""
      <> Text.unpack metadataBlockMarker
      <> "\", in " <> filepath
      <> ", but it wasn't there."

   noBodyError =
      Whoops $ "There is no text body following the metadata block in " <> filepath <> "."

   reconstructPost :: Yaml.Value -- ^ Markdown body.
                   -> Cheapskate.Doc -- ^ YAML metadata block.
                   -> Either Whoops Post
   reconstructPost yaml content = first Whoops $ ($ yaml) $ Yaml.parseEither $
      Yaml.withObject "metadata" $ \metadata -> do
         title    <- metadata .: "title"
         composed <- metadata .: "date"
         synopsis <- metadata .: "synopsis"
         let slug = (Text.pack . takeBaseName) filepath
         return Post{ title
                    , synopsis = Cheapskate.markdown Cheapskate.def synopsis
                    , slug
                    , composed
                    -- TODO: Distinguish these --- maybe.
                    , published = composed
                    , content }

newtype Whoops = Whoops { whatHappened :: String } deriving ( Typeable )

instance Show Whoops where
   show = show . whatHappened

instance Exception Whoops
