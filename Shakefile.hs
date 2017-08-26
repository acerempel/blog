{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Main where

import Control.Monad ( (=<<) )
import Data.Bifunctor
import Data.Either ( either )
import Data.Foldable ( for_, msum )
import Data.List ( intercalate, sortBy )
import Data.Maybe ( catMaybes, maybe )
import Data.Ord ( comparing, Down(..) )

import Data.Time.Calendar ( Day )
import Data.Time.Format
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
        let dropDrafts = filter (not . isDraft)
            sortByDate = sortBy (comparing (Down . date))
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
        whenJust mPost $ \post -> do
            let html = Blaze.renderHtml (page (ThisPost (identifier post)) post)
            liftIO $ createDirectoryIfMissing True (takeDirectory out)
            liftIO $ writeFile out html

    (buildDir </> "index.html") %> \out -> do
        posts <- map (\p -> (ThisPost (identifier p), p)) <$> getAllPosts ()
        let html = Blaze.renderHtml (page Home posts)
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ writeFile out html


    (buildDir </> "archive.html") %> \out -> do
        posts <- map (\p -> (ThisPost (identifier p), p)) <$> getAllPosts ()
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


readPostFromFile :: FilePath -> Action (Either Error Post)
readPostFromFile filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    return (readPost filepath contents)

readPost :: FilePath -> Text -> Either Error Post
readPost filepath contents = do
    let doc = Cheapskate.markdown Cheapskate.def contents
    let title = Just "ALAN_BLOG_NO_TITLE"
        isDraft = False
        dateError = Error $ "No date could be parsed from metadata block of file " <> filepath
    date <- maybe (Left dateError) Right $ msum $ map (parseTimeWithFormat "Jan 5 2017") formats
    return $ Post
        { content = Blaze.toHtml doc
        , identifier = Text.takeWhile (/= '.') $ Text.pack filepath
        , date = date
        , postTitle = title
        , isDraft = isDraft
        }

  where
    parseTimeWithFormat dateString format =
        parseTimeM True defaultTimeLocale format dateString

    formats =
        ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y"
        ,"%d %B %Y", "%b. %d, %Y", "%B %d, %Y"
        ,"%Y%m%d", "%Y%m", "%Y"]

data Error = forall e. Show e => Error e

instance Show Error where
    showsPrec i (Error e) =
        showsPrec i e
