{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Data.Bifunctor
import Data.Either ( partitionEithers )
import Data.Foldable ( for_, msum )
import Data.List ( intercalate )

import Data.Time.Calendar ( Day )
import Data.Time.Format
import qualified Data.Text as Text
import Development.Shake
import System.Directory ( createDirectoryIfMissing, getCurrentDirectory )
import System.FilePath
import Text.Pandoc as Pandoc
import Text.Pandoc.Walk as Pandoc
import qualified Text.Pandoc.Options as Pandoc
import qualified Text.Pandoc.Readers.Markdown as Pandoc
import qualified Text.Pandoc.Writers.HTML as Pandoc
import Text.Blaze.Html.Renderer.String

import Page.Meta
import Templates.Page
import Types
import Utils

options =
    shakeOptions{ shakeVersion="1.1" }

mdOptions =
    def { readerSmart = True }

htmlOptions =
    def { writerHtml5 = True
        , writerSectionDivs = True }

main :: IO ()
main = shakeArgs options $ do
    action $ do
        postsOrErrors <-
            getDirectoryFiles "posts" ["*.md"]
            >>= traverse readPostFromFile
        let (errors, posts) = partitionEithers postsOrErrors
        for_ errors $ \message ->
            putQuiet ("Error: " <> show message)
        let pages = [Home, Archive] <> (Post <$> posts)
        siteDir <- (</> "_site") <$> liftIO getCurrentDirectory
        for_ pages $ \thisPage -> liftIO $ do
            let path = siteDir </> ((tail.Text.unpack.url.meta) thisPage)
            createDirectoryIfMissing True path
            writeFile (path </> "index.html") $ (renderHtml . page posts) thisPage

  where
    readPostFromFile :: FilePath -> Action (Either Error Post)
    readPostFromFile filepath = do
        contents <- (liftIO . readFile) ("posts" </> filepath)
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
                        msum $ parseTimeWithFormat dateString <$> formats
                    = Right parsedDate
                  | otherwise
                    = Left (Error $ "No date could be parsed from metadata block of file " <> filepath)
            draft | Just (Pandoc.MetaBool isDraft) <- Pandoc.lookupMeta "draft" meta
                    = isDraft
                  | otherwise
                    = False
        date <- dateOrErr
        return $ P
            { content = Pandoc.writeHtml htmlOptions md
            , identifier = Text.pack filepath
            , date = date
            , postTitle = title
            , isDraft = draft
            }

    parseTimeWithFormat dateString format =
        parseTimeM True defaultTimeLocale format dateString

    formats =
        ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y"
        ,"%d %B %Y", "%b. %d, %Y", "%B %d, %Y"
        ,"%Y%m%d", "%Y%m", "%Y"]

    extractStringFromMetaValue :: MetaValue -> Maybe String
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
