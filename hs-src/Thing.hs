{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Thing
  ( SourcePath(..), TargetPath(..), Thing(..), URL(..)
  ) where

import Data.ByteString
import Data.String ( IsString )
import Development.Shake.Classes

import Introit

newtype SourcePath = SourcePath { fromSourcePath :: ByteString }
  -- Derive all the instances that Shake wants
  deriving newtype ( Show, Eq, Hashable, Binary, NFData, IsString )

newtype TargetPath = TargetPath { fromTargetPath :: String }
  deriving newtype ( Show, Eq, Hashable, Binary, NFData, IsString )

newtype URL = URL { fromURL :: Text }
  deriving stock ( Show )

data Thing = Thing
  { thingTargetPath :: TargetPath
  , thingSourcePath :: SourcePath
  , thingUrl :: URL }
