{-# LANGUAGE LambdaCase, PatternGuards #-}
module Prose ( Prose, plain) where

import Introit
import Properties
import qualified Text

import qualified Data.List.NonEmpty as NE
import Control.Foldl ( Fold )
import List ( List )
import qualified List
import qualified Lucid
import qualified Text.Megaparsec as MP
import Text.MMark ( MMark )
import qualified Text.MMark as MMark
import Text.MMark.Extension.PunctuationPrettifier
import qualified Text.MMark.Extension as MMark
import Text.MMark.Type ( MMark(..) )
import Text.URI ( uriPath, uriScheme )
import qualified Text.URI as URI

data Prose = Prose
  { proseContent :: !MMark
  , prosePreview :: Maybe MMark
  , proseIncipit :: Text }

instance HasPreview Prose where
  preview = prosePreview

instance HasIncipit Prose where
  incipit = proseIncipit

instance HasContent Prose where
  content = proseContent

instance Parse Prose where
  parse = parseProse

parseProse :: FilePath -> Text -> Either String Prose
parseProse file contents = do
  proseContent <-
    first MP.errorBundlePretty $
    second (MMark.useExtension (punctuationPrettifier <> customTags)) $
    MMark.parse file contents
  let
    (proseIncipit, (firstFewParagraphs, isThereMore)) =
      MMark.runScanner proseContent $
        (,) <$> firstNWords 5 <*> previewParagraphs 2
    prosePreview =
      if isThereMore
        then Just proseContent{mmarkBlocks = firstFewParagraphs}
        else Nothing
  return Prose{..}

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

plain :: MMark -> Text
plain _mmark = undefined -- TODO!
