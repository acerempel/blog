{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module CLI ( runCLI ) where

import qualified Data.Set as Set
import Data.String

import Prelude hiding ( (>>) )

import Options.Applicative

import BuildV2 ( Options(..) )

(>>) = (<>)

runCLI :: ParserInfo Options
runCLI = info parseCLI mempty

parseCLI :: Parser Options
parseCLI = Options
  <$> strOption do
        long "posts-subdir"
        metavar "DIRECTORY"
        help "Where to find posts, beneath the input directory."
  <*> strOption do
        long "input-dir"
        metavar "DIRECTORY"
        help "Where to search for input files."
  <*> strOption do
        long "output-dir"
        metavar "DIRECTORY"
        help "Where to put the finished site."
  <*> (Set.fromList <$> many (strOption do
        long "exclude-dir"
        metavar "DIRECTORY"
        help "Ignore these subdirectories when searching for files."))
  <*> switch do
        long "include-drafts"
        help "Build drafts too."
  <*> switch do
        long "include-tags"
        help "Also display tags on the site."
