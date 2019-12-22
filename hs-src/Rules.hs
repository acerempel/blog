{-# LANGUAGE TypeFamilies, DerivingStrategies, DeriveGeneric, DeriveAnyClass,
 GeneralizedNewtypeDeriving #-}
module Rules
  ( addSourceFileRule, addEverythingRule
  , IncludeDraftsQ(..), RunPaths(..)
  , rule, buildFiles, buildEverything
  ) where

import Data.Binary ( encode, decode )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Bytes ( fromStrict, toStrict, readFile )
import Data.Coerce
import Development.Shake hiding ( doesFileExist )
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import GHC.Generics ( Generic )
import System.Directory ( doesFileExist, createDirectoryIfMissing)

import FilePath
import Introit
import Options

data OutputDirectoryQ = OutputDirectoryQ
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult OutputDirectoryQ = FilePath

newtype Everything = Everything { everything :: [TargetPath] }
  deriving newtype ( Eq, Show, Hashable, Binary, NFData )

newtype WhatBuilt = WhatBuilt { whatBuilt :: [TargetPath] }
  deriving newtype ( Eq, Show, Hashable, Binary, NFData )

type instance RuleResult Everything = WhatBuilt

buildEverything :: [TargetPath] -> Action [TargetPath]
buildEverything sourcePaths =
  whatBuilt <$> apply1 (Everything sourcePaths)

addEverythingRule :: Options ->  Rules ()
addEverythingRule options = do
  addOracle \OutputDirectoryQ -> return (outputDirectory options)
  addBuiltinRule noLint noIdentity everythingRun
  where
    store = "" :: ByteString
    everythingRun :: Everything -> Maybe ByteString -> RunMode -> Action (RunResult WhatBuilt)
    everythingRun Everything{everything} _ mode = do
      -- Rerun if the output directory changes. We won't actually rebuild
      -- anything that hasn't changed -- this just ensure we are called /at
      -- all/, so that anything needs to has the /chance/ to rebuild.
      _ <- askOracle OutputDirectoryQ
      changedQualified <- needHasChanged (map (qualify options) everything)
      let status =
            if null changedQualified
              then case mode of
                     RunDependenciesSame -> ChangedNothing
                     RunDependenciesChanged -> ChangedRecomputeSame
              else ChangedRecomputeDiff
          changed =
            map (unqualify options) .
            filter (outputDirectory options `isPrefixOf`) $
            changedQualified
      return (RunResult status store (WhatBuilt changed))


addSourceFileRule :: Options -> Rules ()
addSourceFileRule options = do
  addBuiltinRule noLint noIdentity (sourceFileRun options)

data IncludeDraftsQ = IncludeDraftsQ
  -- Derive all the instances that Shake wants
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( Hashable, Binary, NFData )
type instance RuleResult IncludeDraftsQ = Bool

data SourceFileRule = SourceFileRule
  { -- | The argument passed to this function is actually a /relative/ source
    -- path, without the input directory prefixed; it is passed as
    -- a 'FilePath' so as to allow use of the various existing predicates on
    -- 'FilePath's.
    match :: FilePath -> Bool
    -- | Same note applies as to 'match'.
  , sourceToTarget :: FilePath -> FilePath
  , run :: RunPaths -> Action () }

-- | These paths, however, /are/ fully qualified and point to actual files.
data RunPaths =
  P { source :: !FilePath, target :: !FilePath }

data SourceFileR = SourceFileR
  { targetPath :: !FilePath
  , targetHash :: {-# UNPACK #-} !Int }
  deriving stock ( Eq, Generic )
  deriving anyclass ( Binary )

data TargetPathChanged = TargetPathChanged
  { resultTargetPath :: TargetPath
  , resultChanged :: Bool }
  deriving stock ( Eq, Generic, Show )
  deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult SourcePath = TargetPathChanged

rule :: (FilePath -> Bool) -> (FilePath -> FilePath) -> (RunPaths -> Action ()) -> Rules ()
rule match sourceToTarget run =
  addUserRule SourceFileRule{..}

buildFiles :: [SourcePath] -> Action [TargetPath]
buildFiles sourcePaths =
  map resultTargetPath . filter resultChanged <$> apply sourcePaths

sourceFileRun :: Options -> SourcePath -> Maybe ByteString -> RunMode -> Action (RunResult TargetPathChanged)
sourceFileRun options key mbPreviousEncoded mode = do
  (_version, relevantRule) <- getUserRuleOne key (const Nothing) matchRule
  let newTargetPath =
        (TargetPath . sourceToTarget relevantRule . fromSourcePath) key
      actionPaths =
        P{ source = qualify options key
         , target = qualify options newTargetPath }
      mustRebuild =
        any (?== coerce newTargetPath) (rebuildPatterns options)
  case mode of
    RunDependenciesSame
      | Just previous <- mbPrevious
      , not mustRebuild
        -> do let SourceFileR{ targetPath = previousTargetPathQualified
                             , targetHash = previousTargetHash } =
                    previous
                  newTargetPathQualified = qualify options newTargetPath
              -- We compare the qualified paths, so that we rebuild if the
              -- output directory changes.
              newTargetPathExists <- liftIO $ doesFileExist newTargetPathQualified
              let nothingChanged =
                    -- The 'fromJust' is fine because we are not called
                    -- with 'RunDependenciesSame' unless we have run
                    -- before, and therefore have a database value.
                    RunResult ChangedNothing (fromJust mbPreviousEncoded) (TargetPathChanged newTargetPath False)
              if newTargetPathQualified /= previousTargetPathQualified
                -- If we are building a different file than we did last
                -- time …
                then if newTargetPathExists then
                      do newTargetExistingHash <- liftIO $ hash <$> Bytes.readFile newTargetPathQualified
                         if newTargetExistingHash == previousTargetHash
                           then return nothingChanged
                           -- and the new target file is different than
                           -- what we would build, rebuild. (Shake's
                           -- default file rule does not do this – it only
                           -- rebuilds if the file is missing (or deps have
                           -- changed.))
                           else rebuild relevantRule actionPaths
                      else rebuild relevantRule actionPaths
                else do if not newTargetPathExists
                          -- If the target file does not exist, rebuild.
                          then rebuild relevantRule actionPaths
                          -- If the dependencies are the same, and the
                          -- target file we produced last time is still
                          -- there, do nothing.
                          else return nothingChanged
      | otherwise -> rebuild relevantRule actionPaths
    RunDependenciesChanged ->
      rebuild relevantRule actionPaths
  where
    mbPrevious =
      decode . Bytes.fromStrict <$> mbPreviousEncoded
    -- N.B. that 'paths' contains the real filesystem paths, not the
    -- unqualified ones.
    rebuild rule paths = do
      need [source paths]
      produces [target paths]
      liftIO $ createDirectoryIfMissing True (takeDirectory (target paths))
      run rule paths
      targetContents <- liftIO $ Bytes.readFile (target paths)
      let newTargetHash = hash targetContents
          newStore = SourceFileR (target paths) newTargetHash
          (changedDatabase, changedResult) =
            if Just newStore == mbPrevious
              then (ChangedRecomputeSame, False)
              else (ChangedRecomputeDiff, True)
          store = Bytes.toStrict $ encode newStore
          result = TargetPathChanged (unqualify options (target paths)) changedResult
      return (RunResult changedDatabase store result)
    matchRule rule@SourceFileRule{..} =
      if match (fromSourcePath key) then Just rule else Nothing
