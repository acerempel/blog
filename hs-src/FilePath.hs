{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module FilePath
  ( SourcePath(..), TargetPath(..)
  , Relative(..)
  ) where

import Options

import Data.String ( IsString )
import Data.Coerce
import Development.Shake.Classes
import Development.Shake.FilePath


newtype SourcePath = SourcePath { fromSourcePath :: FilePath }
  -- Derive all the instances that Shake wants
  deriving newtype ( Show, Eq, Hashable, Binary, NFData, IsString )
newtype TargetPath = TargetPath { fromTargetPath :: FilePath }
  deriving newtype ( Show, Eq, Hashable, Binary, NFData )

class Relative path where
  qualify :: Options -> path -> FilePath
  unqualify :: Options -> FilePath -> path
  default unqualify :: Coercible FilePath path => Options -> FilePath -> path
  unqualify _opts = coerce . dropDirectory1

instance Relative SourcePath where
  qualify Options{inputDirectory} (SourcePath fp) =
    inputDirectory </> fp

instance Relative TargetPath where
  qualify Options{outputDirectory} (TargetPath fp) =
    outputDirectory </> fp
