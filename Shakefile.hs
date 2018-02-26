{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad ( (=<<) )
import Data.Bifunctor
import Data.Foldable ( msum )
import Data.List ( sortBy )
import Data.Ord ( comparing, Down(..) )

import qualified Data.ByteString.Char8 as Bytes
import Data.Time.Calendar ( Day )
import Data.Time.Format ( defaultTimeLocale, parseTimeM )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Yaml ( (.:), (.:?), (.!=) )
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.FilePath
import System.Directory ( createDirectoryIfMissing )
import qualified Cheapskate
import Text.Blaze.Html ( Html )
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.String as Blaze
import qualified Text.Sass as Sass

import Pages

buildDir :: FilePath
buildDir = "_site"

postsDir :: FilePath
postsDir = "posts"

main :: IO ()
main = shakeArgs shakeOptions $ do

    getPost <- newCache $ \src -> do
        postOrError <- readPostFromFile src
        -- TODO: Throw an exception instead.
        case postOrError of
            Left message -> do
                putQuiet ("Error: " <> show message)
                return Nothing
            Right post ->
                return (Just post)

    getAllPostSourceFiles <- newCache $ \() ->
        map (postsDir </>) <$> getDirectoryFiles postsDir ["*.md"]

    getAllPosts <- newCache $ \() -> do
        posts <- traverse getPost =<< getAllPostSourceFiles ()
        let sortByDate = sortBy (comparing (Down . date))
        return $ (sortByDate . catMaybes) posts

    action $ do
        posts <-
            map (-<.> "html")
            <$> getAllPostSourceFiles ()
        styles <-
            map (("styles" </>) . (-<.> "css"))
            <$> getDirectoryFiles "styles" ["*.scss"]
        let pages =
                ["archive.html", "index.html"]
        need $ map (buildDir </>) (styles <> pages <> posts)

    (buildDir </> "posts/*.html") %> \out -> do
        let src = dropDirectory1 out -<.> "md"
        getPost src >>= maybe (return ()) (uncurry (renderPageToFile out))

    (buildDir </> "index.html") %> \out -> do
        getAllPosts () >>= renderPageToFile out Home

    (buildDir </> "archive.html") %> \out -> do
        getAllPosts () >>= renderPageToFile out Archive

    (buildDir </> "styles/*.css") %> \out -> do
        let src = dropDirectory1 out -<.> "scss"
        need [src]
        scssOrError <- liftIO $ Sass.compileFile src Sass.def
        -- TODO: Do error handling differently, just throw an exception.
        case scssOrError of
            Left error -> do
                message <- liftIO $ Sass.errorMessage error
                putQuiet ("Error: " <> show message)
            Right scss -> do
                liftIO $ createDirectoryIfMissing True (takeDirectory out)
                liftIO $ writeFile out scss

renderHtmlToFile :: FilePath -> Html -> Action ()
renderHtmlToFile out markup = do
    let html = Blaze.renderHtml markup
    liftIO $ createDirectoryIfMissing True (takeDirectory out)
    liftIO $ writeFile out html

readPostFromFile :: FilePath -> Action Post
readPostFromFile filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    return (readPost filepath contents)

readPost :: FilePath -> Text -> Post
readPost filepath contents = do
   let doc = Cheapskate.markdown Cheapskate.def contents
       identifier = (mconcat . take 4 . Text.words) contents
   date <- parseDate (takeBaseName filepath)
   return $ postWithMetadata doc identifier date

postWithMetadata :: Blaze.Html -> Text -> Day -> Post
postWithMetadata doc title date =
     Post
       { content = Blaze.toHtml doc
       , date = date
       , postTitle = title
       }

data Error = forall e. Show e => Error e

instance Show Error where
    showsPrec i (Error e) =
        showsPrec i e

parseDate :: String -> Either Error Day
parseDate = first Error . parseTimeM True defaultTimeLocale "%Y.%-m.%-d"
