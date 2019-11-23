module BuildV2 ( build, Options(..) ) where

import Introit

import Development.Shake

import Options
import Post ( readPost )
import qualified Templates
import Rules

build :: Options -> IO ()
build options@Options{..} = shake shakeOptions do
  run options Templates.page do
    html "posts/*.md" "*/index.html" \source -> do
      post <- readPost source
      return (Templates.post includeTags post)
