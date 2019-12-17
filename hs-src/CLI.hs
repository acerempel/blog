{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module CLI ( runCLI ) where

import Data.String

import Prelude hiding ( (>>) )

import Options.Applicative

import Options ( Options(..) )

(>>) = (<>)

runCLI :: ParserInfo Options
runCLI = info parseCLI mempty

parseCLI :: Parser Options
parseCLI = Options
  <$> strOption do
        long "input-dir"
        short 'i'
        metavar "DIRECTORY"
        help "Where to search for input files."
  <*> strOption do
        long "output-dir"
        short 'o'
        metavar "DIRECTORY"
        help "Where to put the finished site."
  <*> switch do
        long "include-drafts"
        help "Build drafts too."
  <*> switch do
        long "include-tags"
        help "Also display tags on the site."
  <*> many (strOption do
        long "rebuild-md"
        short 'r'
        metavar "PATTERN"
        help "Mark all files matching this pattern as needing rebuild")
  <*> switch do
        long "upload"
        short 'u'
        help "Upload all changed files"
