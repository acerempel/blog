{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Options where

import Development.Shake

type DirectoryPath = FilePath

data Options = Options
    { inputDirectory :: DirectoryPath
    , outputDirectory :: DirectoryPath
    , verbosity :: Verbosity
    , includeDrafts :: Bool
    , includeTags :: Bool
    , rebuildPatterns :: [String]
    , upload :: Bool }
