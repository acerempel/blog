module BuildV2 ( build, Options(..) ) where

import Introit

import Development.Shake
import Development.Shake.FilePath

import Options
import qualified Post
import qualified Templates
import Rules

build :: Options -> IO ()
-- TODO: Set the verbosity from the command line.
-- TODO: Automate the updating of the 'shakeVersion'.
build options = shake shakeOptions{shakeVerbosity = Chatty, shakeVersion = "13"} do
  getPost <- newCache Post.read
  run options do
    Rules.oneToOne "posts/*.md"
      ((</> "index.html") . dropExtension . takeBaseName)
      \source -> html $ Templates.post <$> getPost source
    Rules.manyToOne "posts/*.md" "index.html" \sources -> html do
      allPosts <- forP sources getPost
      let allPostsSorted =
            sortBy (compare `on` Post.composed) $
            filter (not. Post.isDraft) allPosts
      return (Templates.archive allPostsSorted)
    Rules.oneToOne "styles/three-dots.scss" (-<.> ".css")
      \source target -> do
        need [source]
        cmd_ ("sass" :: String) [ "--no-source-map", source, target ]
    -- TODO: Support multiple source file patterns -- that way we can copy
    -- all fonts, stylesheets, and js with one rule.
    Rules.oneToOne "styles/*.css" id copyFileChanged
