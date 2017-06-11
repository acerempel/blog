{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad ( (=<<) )
import Data.Bifunctor
import Data.Either ( either )
import Data.Foldable ( for_, msum )
import Data.List ( intercalate )
import Data.Maybe ( catMaybes )

import Data.Time.Calendar ( Day )
import Data.Time.Format
import qualified Data.Text as Text
import Development.Shake
import System.Directory ( createDirectoryIfMissing, getCurrentDirectory )
import System.FilePath
import Text.Pandoc ( Pandoc(..) )
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import qualified Text.Pandoc.Options as Pandoc
import qualified Text.Pandoc.Readers.Markdown as Pandoc
import qualified Text.Pandoc.Writers.HTML as Pandoc
import qualified Text.Blaze.Html.Renderer.String as Blaze
import qualified Text.Sass as Sass

import Page.Meta
import Templates.Page
import Types
import Utils

options =
    shakeOptions{ shakeVersion="1.2" }

mdOptions =
    Pandoc.def
        { Pandoc.readerSmart = True }

htmlOptions =
    Pandoc.def
        { Pandoc.writerHtml5 = True
        , Pandoc.writerSectionDivs = True }

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

    getAllPosts <- newCache $ \() ->
        traverse getPost =<< getAllPostSourceFiles ()

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
            let html = (Blaze.renderHtml . page) (Post post)
            liftIO $ createDirectoryIfMissing True (takeDirectory out)
            liftIO $ writeFile out html

    (buildDir </> "index.html") %> \out -> do
        posts <- catMaybes <$> getAllPosts ()
        let html = (Blaze.renderHtml . page) (Home posts)
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ writeFile out html


    (buildDir </> "archive.html") %> \out -> do
        posts <- catMaybes <$> getAllPosts ()
        let html = (Blaze.renderHtml . page) (Archive posts)
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

  where
    readPostFromFile :: FilePath -> Action (Either Error Post)
    readPostFromFile filepath = do
        contents <- readFile' filepath
        return (readPost filepath contents)

    readPost :: FilePath -> String -> Either Error Post
    readPost filepath contents = do
        md@(Pandoc meta doc) <- first Error $ Pandoc.readMarkdown mdOptions contents
        let title =
                Text.pack <$>
                (extractStringFromMetaValue =<< Pandoc.lookupMeta "title" meta)
            dateOrErr
                  | Just dateString <-
                        extractStringFromMetaValue =<< Pandoc.lookupMeta "date" meta
                  , Just parsedDate <-
                        msum $ map (parseTimeWithFormat dateString) formats
                    = Right parsedDate
                  | otherwise
                    = Left (Error $ "No date could be parsed from metadata block of file " <> filepath)
            isDraft
                  | Just (Pandoc.MetaBool draft) <-
                        Pandoc.lookupMeta "draft" meta
                    = draft
                  | otherwise
                    = False
        date <- dateOrErr
        return $ P
            { content = Pandoc.writeHtml htmlOptions md
            , identifier = Text.pack (takeBaseName filepath)
            , date = date
            , postTitle = title
            , isDraft = isDraft
            }

    parseTimeWithFormat dateString format =
        parseTimeM True defaultTimeLocale format dateString

    formats =
        ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y"
        ,"%d %B %Y", "%b. %d, %Y", "%B %d, %Y"
        ,"%Y%m%d", "%Y%m", "%Y"]

    extractStringFromMetaValue :: Pandoc.MetaValue -> Maybe String
    extractStringFromMetaValue metaValue =
        intercalate " " <$> Pandoc.query extractString metaValue
      where
        extractString (Pandoc.Str string) =
            Just [string]
        extractString _ =
            Nothing

data Error = forall e. Show e => Error e

instance Show Error where
    showsPrec i (Error e) =
        showsPrec i e
