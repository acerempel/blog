module BuildV2 ( build, Options(..) ) where

import Introit

import Development.Shake
import Development.Shake.FilePath

import Options
import qualified Post
import qualified Templates
import Rules

build :: Options -> IO ()
build options = shake shakeOptions{shakeVerbosity = Chatty, shakeVersion = "7"} do
  getPost <- newCache Post.read
  run options do
    rule "posts/*.md" $ OneToOne ((</> "index.html") . dropExtension . takeBaseName) \source ->
      html $ Templates.post <$> getPost source
    rule "posts/*.md" $ ManyToOne "index.html" \sources -> html do
      allPosts <- forP sources getPost
      let allPostsSorted = sortOn Post.composed allPosts
      return (Templates.archive allPostsSorted)
    rule "**.scss" $ OneToOne (-<.> ".css") \source target -> do
      need [source]
      cmd_ ("sass" :: String) [ "--disable-source-map", source, target ]
