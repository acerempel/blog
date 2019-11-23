{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase #-}
module Post ( Post(..), Tag, readPost ) where

import Introit
import List ( List )
import qualified List
import qualified Text
import Routes ( Route, ContentType(Html) )
import qualified Routes

import System.FilePath
import Control.Exception ( throwIO )
import Control.Foldl ( Fold )
import qualified Data.List.NonEmpty as NE
import Data.Time.Calendar ( Day )
import Data.Time.Format ( parseTimeM, defaultTimeLocale )
import Data.Yaml ( (.:), (.:?), (.!=) )
import qualified Data.Yaml as Yaml
import Development.Shake
import qualified Lucid
import qualified Network.URI.Encode as URI
import qualified Text.Megaparsec as MP
import Text.MMark ( MMark )
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as MMark
import Text.MMark.Type ( MMark(..) )
import Text.MMark.Extension.PunctuationPrettifier
import Text.URI ( uriPath, uriScheme )
import qualified Text.URI as URI


data Post = Post
   { slug :: Route 'Html -- ^ Route to this post.
   , mTitle :: Maybe Text -- ^ Title.
   , firstFewWords :: Text
   , content :: MMark -- ^ The post body.
   , preview :: MMark
   , previewIsFullPost :: Bool
   , mSynopsis :: Maybe Text -- ^ A little summary or tagline.
   , description :: Maybe Text -- ^ A slightly longer and self-contained description.
   , composed :: Day -- ^ Date of composition.
   , published :: Day -- ^ Date of publication.
   , isDraft :: Bool -- ^ Whether this post is a draft or is published.
   , tags :: [Tag] -- ^ Some tags.
   }

type Tag = Text

readPost :: FilePath
         -> Action Post
readPost filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    liftIO $ either (throwIO . userError) return do
      body <-
         first MP.errorBundlePretty $
         second (MMark.useExtension (punctuationPrettifier <> customTags)) $
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
         let (firstFewWords, (firstFewParagraphs, isThereMore)) =
               MMark.runScanner content $
                (,) <$> firstNWords 5 <*> previewParagraphs 2
         let preview = content{mmarkBlocks = firstFewParagraphs}
         return Post
            { published = composed -- TODO: Distinguish these --- maybe.
            , isDraft = False
            , previewIsFullPost = not isThereMore
            , .. }

   noMetadataError =
      "Couldn't find a metadata block in file " <> filepath

   dateFormat = "%e %B %Y"

customTags :: MMark.Extension
customTags =
  MMark.inlineRender \defaultRender inline ->
    case inline of
      MMark.Link innerInlines uri mTitle ->
        case uriScheme uri of
          Just scheme | URI.unRText scheme == "tag" ->
            case uriPath uri of
              Just (False, tag NE.:| []) ->
                Lucid.termWith
                  (URI.unRText tag)
                  (maybe [] ((: []) . Lucid.title_) mTitle)
                  (mapM_ defaultRender innerInlines)
              _ -> defaultRender inline
          _ -> defaultRender inline
      _ -> defaultRender inline

firstNWords :: Int -> Fold MMark.Bni Text
firstNWords n =
  Text.unwords . take n . Text.words <$>
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

previewParagraphs :: Int -> Fold MMark.Bni (List MMark.Bni, Bool)
previewParagraphs n =
  extractResults <$>
    MMark.scanner
      ([], n, undefined)
      \(blocksSoFar, wanted, _areThereMore) thisBlock ->
        if wanted == 0 then
          (blocksSoFar, 0, True)
        else
          appendBlocks blocksSoFar thisBlock wanted
  where
    extractResults (a, _b, c) = (a, c)
    appendBlocks blocksSoFar thisBlock wanted =
      case thisBlock of
        MMark.Blockquote subBlocks ->
          -- Descend into the inner blocks of a blockquote. I do have
          -- multi-paragraph block quotations, and I'll probably add more.
          let blocksToAdd = List.take wanted subBlocks
          in ( blocksSoFar <> List.singleton (MMark.Blockquote blocksToAdd)
             , wanted - length blocksToAdd
             , length blocksToAdd < length subBlocks)
        MMark.OrderedList _ listItems ->
          -- We actually only take N list items, each of which may, in
          -- principle, be composed of multiple blocks. Truly taking
          -- N blocks while preserving the block tree structure is an
          -- interesting problem to solve, but I'm not concerned about it
          -- for now – I don't think I have any multi-paragraph list items
          -- at the moment anyway.
          let blocksToAdd = List.concat . List.fromList $ NE.take wanted listItems
          in ( blocksSoFar <> blocksToAdd
             , wanted - length blocksToAdd
             , length blocksToAdd < length listItems)
        MMark.UnorderedList listItems ->
          -- See above, under `MMark.OrderedList`.
          let blocksToAdd = List.concat . List.fromList $ NE.take wanted listItems
          in ( blocksSoFar <> blocksToAdd
             , wanted - length blocksToAdd
             , length blocksToAdd < length listItems)
        MMark.Table _ _ ->
          -- Abort upon hitting a table. I don't want any tables appearing
          -- on the front page.
          (blocksSoFar, 0, True)
        MMark.ThematicBreak ->
          if wanted == 1 then
            -- If this would be the last block we take, then don't take it,
            -- and abort. It doesn't make sense for a thematic break to be
            -- the last thing in the preview.
            (blocksSoFar, 0, True)
          else
            -- If we have more to take, then keep going, but don't count
            -- the break towards the number we've taken.
            (blocksSoFar <> List.singleton MMark.ThematicBreak, wanted, False)
        _ ->
          -- All other blocks (headings, paragraphs, naked blocks, and code
          -- blocks) we simply take.
          (blocksSoFar <> List.singleton thisBlock, wanted - 1, False)
