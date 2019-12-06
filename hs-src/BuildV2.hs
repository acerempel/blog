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

data IncludeDraftsQ = IncludeDraftsQ deriving ( Show, Eq, Generic, Hashable, Binary, NFData )
type instance RuleResult IncludeDraftsQ = Bool

build :: Options -> IO ()
-- TODO: Set the verbosity from the command line.
-- TODO: Automate the updating of the 'shakeVersion'.
build options = shake shakeOptions{shakeVerbosity = Chatty, shakeVersion = "14"} do
  getPost <- newCache Post.read
  -- This is needed by the "index.html" rule to rebuild when the
  -- `include-drafts` command-line option changes.
  addOracle \IncludeDraftsQ -> return (includeDrafts options)
  run options do
    Rules.oneToOne "posts/*.md"
      ((</> "index.html") . dropExtension . takeBaseName)
      \source -> html $ Templates.post <$> getPost source
    Rules.manyToOne "posts/*.md" "index.html" \sources -> html do
      allPosts <- forP sources getPost
      shouldIncludeDrafts <- askOracle IncludeDraftsQ
      let filterOutDrafts =
            if shouldIncludeDrafts
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
