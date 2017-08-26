{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Main where

import Control.Monad ( (=<<) )
import Data.Bifunctor
import Data.Either ( either )
import Data.Foldable ( for_, msum )
import Data.List ( intercalate, sortBy )
import Data.Maybe ( catMaybes, maybe )
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
import System.Directory ( createDirectoryIfMissing, getCurrentDirectory )
import System.FilePath
import qualified Cheapskate
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.String as Blaze
import qualified Text.Sass as Sass

import Page
import Utils

options =
    shakeOptions{ shakeVersion="1.2" }

buildDir :: FilePath
buildDir = "_site"

main :: IO ()
main = shakeArgs options $ do

    getPost <- newCache $ \src -> do
        postOrError <- readPostFromFile src
        case postOrError of
            Left message -> do
                putQuiet ("Error: " <> show message)
                return Nothing
            Right post ->
                return (Just post)

    getAllPostSourceFiles <- newCache $ \() ->
        map ("posts" </>) <$> getDirectoryFiles "posts" ["*.md"]

    getAllPosts <- newCache $ \() -> do
        posts <- traverse getPost =<< getAllPostSourceFiles ()
        let dropDrafts = filter (not . isDraft . snd)
            sortByDate = sortBy (comparing (Down . date . snd))
        return $ (sortByDate . dropDrafts . catMaybes) posts

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
        let src = (tail . dropWhile (not . isPathSeparator)) out -<.> "md"
        mPost <- getPost src
        whenJust mPost $ \(route, post) -> do
            let html = Blaze.renderHtml (page route post)
            liftIO $ createDirectoryIfMissing True (takeDirectory out)
            liftIO $ writeFile out html

    (buildDir </> "index.html") %> \out -> do
        posts <- getAllPosts ()
        let html = Blaze.renderHtml (page Home posts)
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ writeFile out html


    (buildDir </> "archive.html") %> \out -> do
        posts <- getAllPosts ()
        let html = Blaze.renderHtml (page Archive posts)
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ writeFile out html

    (buildDir </> "styles/*.css") %> \out -> do
        let src = (tail . dropWhile (not . isPathSeparator)) out -<.> "scss"
        need [src]
        scssOrError <- liftIO $ Sass.compileFile src Sass.def
        case scssOrError of
            Left error -> do
                message <- liftIO $ Sass.errorMessage error
                putQuiet ("Error: " <> show message)
            Right scss -> do
                liftIO $ createDirectoryIfMissing True (takeDirectory out)
                liftIO $ writeFile out scss


readPostFromFile :: FilePath -> Action (Either Error (WhichPost, Post))
readPostFromFile filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    return (readPost filepath contents)

readPost :: FilePath -> Text -> Either Error (WhichPost, Post)
readPost filepath contents = do
    (yaml, md) <-
      maybe (Left noMetadataBlockError) Right
      . fmap
        ( first (Bytes.pack . Text.unpack)
        . second (Text.drop (Text.length metadataBlockMarker) )
      . Text.breakOn metadataBlockMarker)
      . (Text.stripPrefix metadataBlockMarker)
      $ contents
    let doc = Cheapskate.markdown Cheapskate.def md
    first Error $ Yaml.parseEither (postWithMetadata doc) =<< Yaml.decodeEither yaml

  where
    parseTime str =
        msum $ map (parseTimeWithFormat str) formats

    parseTimeWithFormat dateString format =
        parseTimeM True defaultTimeLocale format dateString

    formats =
        ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y"
        ,"%d %B %Y", "%b. %d, %Y", "%B %d, %Y"
        ,"%Y%m%d", "%Y%m", "%Y"]

    metadataBlockMarker = "---\n"

    noMetadataBlockError =
        Error $ "Could not distinguish YAML metadata block in file " <> filepath

    postWithMetadata doc =
        Yaml.withObject "metadata block" $ \v -> do
            date    <- parseTime =<< v .: "date"
            title   <- v .: "title"
            isDraft <- v .:? "draft" .!= False
            let identifier = (Text.takeWhile (/= '.') . Text.pack) filepath
            return
                ( ThisPost identifier
                , Post
                    { content = Blaze.toHtml doc
                    , date = date
                    , postTitle = title
                    , isDraft = isDraft
                    }
                )

data Error = forall e. Show e => Error e

instance Show Error where
    showsPrec i (Error e) =
        showsPrec i e
