{-# LANGUAGE LambdaCase, PatternGuards #-}
module Post ( Post, PostG(..), URL(..), Tag, parse, Html, Problem(..) ) where

import Prelude hiding ( read )

import Introit
import FilePath
import qualified Text

import qualified Data.HashMap.Strict as HashMap
import Data.Time.Calendar ( Day )
import Data.Time.Format ( parseTimeM, defaultTimeLocale )
import Data.Yaml ( (.:?), (.!=) )
import qualified Data.Yaml as Yaml
import qualified Text.MMark as MMark
import qualified Data.List.NonEmpty as NE
import Control.Exception
import Control.Foldl ( Fold )
import Development.Shake.FilePath
import List ( List )
import qualified List
import qualified Lucid
import Lucid.Base ( relaxHtmlT )
import qualified Text.Megaparsec as MP
import Text.MMark ( MMark )
import Text.MMark.Extension.PunctuationPrettifier
import qualified Text.MMark.Extension as MMark
import Text.MMark.Type ( MMark(..) )
import Text.URI ( uriPath, uriScheme )
import qualified Text.URI as URI

type Html = Lucid.HtmlT (Either Problem) ()

data Problem
  = MissingField { what :: URL, field :: Text }
  | MarkdownParseError String
  | YamlParseError String
  deriving Show

instance Exception Problem where
  displayException MissingField { what, field } =
    "The page at \"" <> Text.unpack (fromURL what) <> "\" is missing the required field \"" <> Text.unpack field <> "\"."
  displayException (MarkdownParseError message) =
    message
  displayException (YamlParseError message) =
    message

type Post = PostG Html

data PostG prose = Post
   { url :: URL -- ^ Route to this post.
   , title :: Maybe prose
   , pageTitle :: Text
   , preview :: Maybe prose
   , body :: Html -- ^ The post body.
   , synopsis :: Maybe prose -- ^ A little summary or tagline.
   , description :: Maybe Text -- ^ A slightly longer and self-contained description.
   , published :: Maybe Day -- ^ Date of publication.
   , isDraft :: Bool -- ^ Whether this post is a draft or is published.
   , tags :: [Tag] -- ^ Some tags.
   }

newtype URL = URL { fromURL :: Text } deriving Show

sourcePathToURL :: SourcePath -> URL
sourcePathToURL =
  URL . Text.pack . ('/' :) . dropExtension . fromSourcePath

type Tag = Text

parse :: SourcePath -> Text -> Either Problem Post
parse filepath contents = do
  bodyMarkdown <- parseMarkdown filepath contents
  let yaml = fromMaybe (Yaml.Object HashMap.empty) (MMark.projectYaml bodyMarkdown)
  Post{..} <- first YamlParseError $ withMetadata yaml
  titleMarkdown <- traverse (parseMarkdownSingleParagraph filepath) title
  synopsisMarkdown <- traverse (parseMarkdownSingleParagraph filepath) synopsis
  let
    (incipit, (firstFewParagraphs, isThereMore)) =
      MMark.runScanner bodyMarkdown $
        (,) <$> firstNWords 5 <*> previewParagraphs 2
    previewMarkdown =
      if isThereMore
        then Just bodyMarkdown{mmarkBlocks = firstFewParagraphs}
        else Nothing
  return Post
    { body = renderMarkdown bodyMarkdown
    , title = renderMarkdown <$> titleMarkdown
    , synopsis = renderMarkdown <$> synopsisMarkdown
    , preview = renderMarkdown <$> previewMarkdown
    , pageTitle =
        maybe incipit (flip MMark.runScanner plainText) titleMarkdown
    , .. }
 where
   withMetadata :: Yaml.Value -> Either String (PostG Text)
   withMetadata = Yaml.parseEither $
      Yaml.withObject "metadata" \metadata -> do
         title    <- metadata .:? "title"
         synopsis <- metadata .:? "synopsis"
         description <- metadata .:? "description"
         isDraft <- metadata .:? "draft" .!= False
         tags     <- metadata .:? "tags" .!= []
         published <- traverse (parseTimeM True defaultTimeLocale dateFormat) =<< metadata .:? "date"
         let url = sourcePathToURL filepath
         return Post{..}

   dateFormat = "%e %B %Y"

parseMarkdown :: SourcePath -> Text -> Either Problem MMark
parseMarkdown path contents =
  first (MarkdownParseError . MP.errorBundlePretty) $
  second (MMark.useExtension (punctuationPrettifier <> customTags)) $
  MMark.parse (fromSourcePath path) contents

parseMarkdownSingleParagraph :: SourcePath -> Text -> Either Problem MMark
parseMarkdownSingleParagraph file contents =
  MMark.useExtension unParagraphize <$> parseMarkdown file contents

renderMarkdown :: MMark -> Html
renderMarkdown =
  relaxHtmlT . MMark.render

customTags :: MMark.Extension
customTags = MMark.inlineRender renderCustomTags
  where
    renderCustomTags defaultRender inline
      | MMark.Link innerInlines uri mTitle <- inline
      , Just scheme <- uriScheme uri
      , URI.unRText scheme == "tag"
      , Just (False, tag NE.:| []) <- uriPath uri
        = Lucid.termWith
          (URI.unRText tag)
          (maybe [] ((: []) . Lucid.title_) mTitle)
          (mapM_ defaultRender innerInlines)
      | otherwise
        = defaultRender inline

unParagraphize :: MMark.Extension
unParagraphize = MMark.blockRender \_defaultRender -> \case
  MMark.Paragraph (_ois, html) -> html
  _ -> error "Was ist jetzt los??"

firstNWords n =
  Text.unwords . take n . Text.words <$> plainText

plainText :: Fold MMark.Bni Text
plainText =
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
