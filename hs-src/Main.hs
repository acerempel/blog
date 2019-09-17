module Main where

import qualified Options.Applicative as Options

import BuildV2
import CLI


main :: IO ()
main = do
    options <- Options.execParser runCLI
    build options
