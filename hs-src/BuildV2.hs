{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass #-}
module BuildV2 ( build, Options(..) ) where

import Introit

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics ( Generic )

import Options
import qualified Post
import qualified Templates
import Rules

data OptionsQ = OptionsQ deriving ( Show, Eq, Generic, Hashable, Binary, NFData )
type instance RuleResult OptionsQ = Options

build :: Options -> IO ()
-- TODO: Set the verbosity from the command line.
-- TODO: Automate the updating of the 'shakeVersion'.
build options = shake shakeOptions{shakeVerbosity = Chatty, shakeVersion = "14"} do
  getPost <- newCache Post.read
  -- This is needed by the "index.html" rule to rebuild when the
  -- command-line options change. That rule really only cares about
  -- include-drafts, though, and in fact we will have to make this oracle
  -- rule more granular somehow if we ever make the verbosity configurable
  -- by the command-line – changing the verbosity should not cause
  -- a rebuild. This granularity could easily be achieved by creating
  -- a datatype representing the possible options, which datatype would
  -- then be the argument desired by the function passed to `addOracle`;
  -- this would be terribly verbose, though. I wanted to say `newtype
  -- OptionsQ = OptionsQ (Options -> a)`, but Shake wants various instances
  -- that are not possible on functions.
  addOracle \OptionsQ -> return options
  run options do
    Rules.oneToOne "posts/*.md"
      ((</> "index.html") . dropExtension . takeBaseName)
      \source -> html $ Templates.post <$> getPost source
    Rules.manyToOne "posts/*.md" "index.html" \sources -> html do
      allPosts <- forP sources getPost
      -- This is silly – we already have the command-line options, of
      -- course, but we need this rule to rebuild if the options are
      -- changed, because of the include-drafts option (the other options
      -- affect the input and output file paths, so changes to that will be
      -- picked up by the file rule).
      options <- askOracle OptionsQ
      let filterOutDrafts =
            if includeDrafts options
              then id
              else filter (not . Post.isDraft)
          allPostsSorted =
            sortBy (compare `on` Post.composed) $
            filterOutDrafts allPosts
      return (Templates.archive allPostsSorted)
    Rules.oneToOne "styles/three-dots.scss" (-<.> ".css")
      \source target -> do
        need [source]
        cmd_ ("sass" :: String) [ "--no-source-map", source, target ]
    -- TODO: Support multiple source file patterns -- that way we can copy
    -- all fonts, stylesheets, and js with one rule.
    Rules.oneToOne "styles/*.css" id copyFileChanged
