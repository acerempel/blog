module Options where

import Data.Set ( Set )

type DirectoryPath = FilePath

data Options = Options
    { postsSubDirectory :: DirectoryPath
    , inputDirectory :: DirectoryPath
    , outputDirectory :: DirectoryPath
    , excludeDirs :: Set DirectoryPath
    , includeDrafts :: Bool
    , includeTags :: Bool }
