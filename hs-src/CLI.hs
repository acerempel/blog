{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module CLI ( runCLI ) where

import Prelude hiding ( (>>) )

import Data.String
import Development.Shake
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
  <*> option readVerbosity do
        long "verbosity"
        short 'v'
        value Chatty
        showDefault
        metavar "[0..5]"
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

readVerbosity :: ReadM Verbosity
readVerbosity = eitherReader $ \input ->
  let inputInt :: Int
      inputInt = read input
  in case inputInt <= 5 && inputInt >= 0 of
       True -> Right (toEnum inputInt)
       False -> Left ("Not a valid verbosity: " <> input)
