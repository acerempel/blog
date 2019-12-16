{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Options where

import Development.Shake.Classes
import GHC.Generics ( Generic )

type DirectoryPath = FilePath

data Options = Options
    { inputDirectory :: DirectoryPath
    , outputDirectory :: DirectoryPath
    , includeDrafts :: Bool
    , includeTags :: Bool
    , upload :: Bool }
    deriving ( Show, Eq, Generic, Hashable, Binary, NFData )
