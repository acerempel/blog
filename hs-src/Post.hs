{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase #-}
module Post ( Post(..), Tag, readPost, firstNWords ) where

import Introit
import qualified Text
import Routes ( Route, ContentType(Html) )
import qualified Routes

import System.FilePath
import Control.Exception ( throwIO )
import Data.Time.Calendar ( Day )
import Data.Time.Format ( parseTimeM, defaultTimeLocale )
import Data.Yaml ( (.:), (.:?), (.!=) )
import qualified Data.Yaml as Yaml
import qualified Network.URI.Encode as URI
import qualified Text.Megaparsec as MP
import Text.MMark ( MMark )
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as MMark
import Text.MMark.Extension.PunctuationPrettifier


data Post = Post
   { slug :: Route 'Html -- ^ Route to this post.
   , mTitle :: Maybe Text -- ^ Title.
   , content :: MMark -- ^ The post body.
   , mSynopsis :: Maybe Text -- ^ A little summary or tagline.
   , description :: Maybe Text -- ^ A slightly longer and self-contained description.
   , composed :: Day -- ^ Date of composition.
   , published :: Day -- ^ Date of publication.
   , isDraft :: Bool -- ^ Whether this post is a draft or is published.
   , tags :: [Tag] -- ^ Some tags.
   }

type Tag = Text

readPost :: FilePath
         -> IO Post
readPost filepath = do
    putStrLn $ "Reading post from " <> filepath
    contents <- Text.readFile filepath
    either (throwIO . userError) return do
      body <-
         first MP.errorBundlePretty $
         second (MMark.useExtension punctuationPrettifier) $
         MMark.parse filepath contents
      yaml <- maybe (Left noMetadataError) Right $
         MMark.projectYaml body
      withMetadata body yaml
 where
   withMetadata content = Yaml.parseEither $
      Yaml.withObject "metadata" \metadata -> do
         mTitle    <- metadata .:? "title"
         date     <- metadata .: "date"
         mSynopsis <- metadata .:? "synopsis"
         description <- metadata .:? "description"
         tags     <- metadata .:? "tags" .!= []
         composed <- parseTimeM True defaultTimeLocale dateFormat date
         let slug = Routes.PageR $ URI.encode $ takeBaseName filepath
         return Post
            { published = composed -- TODO: Distinguish these --- maybe.
            , isDraft = False
            , .. }

   noMetadataError =
      "Couldn't find a metadata block in file " <> filepath

   dateFormat = "%e %B %Y"

firstNWords :: Int -> MMark -> Text
firstNWords n content =
  Text.unwords $ take n $ Text.words $
  MMark.runScanner content $
  MMark.scanner Text.empty appendPlainText
  where
    appendPlainText textSoFar = \case
      MMark.Heading1 inlines -> textSoFar <> MMark.asPlainText inlines
      MMark.Heading2 inlines -> textSoFar <> MMark.asPlainText inlines
      MMark.Heading3 inlines -> textSoFar <> MMark.asPlainText inlines
      MMark.Heading4 inlines -> textSoFar <> MMark.asPlainText inlines
      MMark.Heading5 inlines -> textSoFar <> MMark.asPlainText inlines
      MMark.Heading6 inlines -> textSoFar <> MMark.asPlainText inlines
      MMark.Naked inlines    -> textSoFar <> MMark.asPlainText inlines
      MMark.Paragraph inlines -> textSoFar <> MMark.asPlainText inlines
      MMark.Blockquote blocks ->
        foldl' appendPlainText textSoFar blocks
      MMark.OrderedList _ listItems ->
        fold $ foldl' appendPlainText textSoFar `fmap` listItems
      MMark.UnorderedList listItems ->
        fold $ foldl' appendPlainText textSoFar `fmap` listItems
      MMark.CodeBlock _ text -> textSoFar <> text
      MMark.ThematicBreak -> textSoFar
      MMark.Table _ _ -> textSoFar
