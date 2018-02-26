module Main where

import Control.Exception
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Typeable ( Typeable )

import Data.Time.Calendar ( Day )
import Data.Time.Format ( defaultTimeLocale, parseTimeM )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
main = shakeArgs shakeOptions $ do

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
         -- If it's an error, do nothing. We already display the errors
         -- above in getAllPosts. But this is actually weird, there must be
         -- a less goofy way to deal with errors.
         $ either (const (return ()))
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
        -- TODO: Do error handling differently, just throw an exception.
        case scssOrError of
            Left err -> do
                message <- liftIO $ Sass.errorMessage err
                putQuiet ("Error: " <> show message)
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
   let content =
          Cheapskate.markdown Cheapskate.def filecontents
       synopsis =
          Cheapskate.markdown Cheapskate.def $
          -- TODO: Where do we get an actual synopsis from? What if there
          -- really is none?
          (mconcat . take 27 . Text.words) filecontents
   date <- parseDate (takeBaseName filepath)
   return Post{ title = Nothing
              , composed = date
              , published = date -- TODO: Distinguish these --- maybe.
              , content
              , synopsis -- TODO!
              , slug = Text.pack (takeBaseName filepath) -- This is actually probably right.
              }

newtype Whoops = Whoops { whatHappened :: String } deriving ( Typeable )

instance Show Whoops where
   show = show . whatHappened

instance Exception Whoops

parseDate :: String -> Either Whoops Day
parseDate =
   first Whoops . parseTimeM True defaultTimeLocale "%Y.%-m.%-d"
