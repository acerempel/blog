{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Thing
  ( SourcePath(..), TargetPath(..), Thing(..), URL(..)
  , Problem(..)
  ) where

import Control.Exception
import Data.ByteString
import Data.String ( IsString )
import Development.Shake.Classes
import GHC.Generics ( Generic )

import Introit
import qualified Text

newtype SourcePath = SourcePath { fromSourcePath :: ByteString }
  -- Derive all the instances that Shake wants
  deriving newtype ( Show, Eq, Hashable, Binary, NFData, IsString )

newtype TargetPath = TargetPath { fromTargetPath :: String }
  deriving newtype ( Show, Eq, Hashable, Binary, NFData, IsString )

newtype URL = URL { fromURL :: Text }
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( Hashable )

data Thing = Thing
  { thingTargetPath :: TargetPath
  , thingSourcePath :: SourcePath
  , thingUrl :: URL }
  deriving stock ( Eq, Generic )
  deriving anyclass ( Hashable )

data Problem
  = MissingField { what :: !URL, field :: !Text }
  | MarkdownParseError !String
  | YamlParseError !String
  | ThingNotFound !FilePath
  deriving Show

instance Exception Problem where
  displayException MissingField { what, field } =
    "The page at \"" <> Text.unpack (fromURL what) <> "\" is missing the required field \"" <> Text.unpack field <> "\"."
  displayException (MarkdownParseError message) =
    message
  displayException (YamlParseError message) =
    message
  displayException (ThingNotFound path) =
    "Unknown thing: " <> path
