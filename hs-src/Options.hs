module Options where

type DirectoryPath = FilePath

data Options = Options
    { inputDirectory :: DirectoryPath
    , outputDirectory :: DirectoryPath
    , includeDrafts :: Bool
    , includeTags :: Bool }
