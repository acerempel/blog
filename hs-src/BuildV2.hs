module BuildV2 ( build, Options(..) ) where

import Introit

import Development.Shake

import Options
import Post ( readPost )
import qualified Templates
import Rules

build :: Options -> IO ()
build options@Options{includeTags} = shake shakeOptions do
  run options (Templates.page includeTags) do
    html "posts/*.md" "*" \source ->
      Templates.post <$> readPost source
    rule "**.scss" "**.css" \RuleParameters{source, target} ->
      undefined
