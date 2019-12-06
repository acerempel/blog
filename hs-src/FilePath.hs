module FilePath where

import Introit
import Options
import Development.Shake.FilePath

intoOutputDir :: Options -> FilePath -> FilePath
intoOutputDir Options{..} =
  swapDirs inputDirectory outputDirectory

outOfInputDir :: Options -> FilePath -> FilePath
outOfInputDir Options{..} =
  swapDirs outputDirectory inputDirectory

swapDirs original new path =
  case stripPrefix original path of
    Just p
      | isPathSeparator (head p) -> new </> tail p
      | otherwise -> new </> p
    Nothing ->
      new </> path
