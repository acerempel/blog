module BuildV2 ( build, Options(..) ) where

import Introit

import Development.Shake

import Options
import qualified Post
import qualified Templates
import Rules

build :: Options -> IO ()
build options@Options{includeTags} = shake shakeOptions do
  getPost <- newCache Post.read
  run options (Templates.page includeTags) do
    html "posts/*.md" "*" \source ->
      Templates.post <$> getPost source
    rule "**.scss" "**.css" \RuleParameters{source, target} ->
      undefined
