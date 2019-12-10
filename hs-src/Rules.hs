{-# LANGUAGE TypeFamilies, DerivingStrategies, DeriveGeneric, DeriveAnyClass,
 GeneralizedNewtypeDeriving #-}
module Rules ( addSourceFileRule, IncludeDraftsQ(..), RunPaths(..), rule, buildFiles ) where

import Data.Binary ( encode, decode )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Bytes ( fromStrict, toStrict, readFile )
import Development.Shake hiding ( doesFileExist )
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import GHC.Generics ( Generic )
import System.Directory ( doesFileExist, createDirectoryIfMissing)

import FilePath
import Options

addSourceFileRule :: Options -> Rules ()
addSourceFileRule options =
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

type instance RuleResult SourcePath = TargetPath

rule :: (FilePath -> Bool) -> (FilePath -> FilePath) -> (RunPaths -> Action ()) -> Rules ()
rule match sourceToTarget run =
  addUserRule SourceFileRule{..}

buildFiles :: [SourcePath] -> Action [TargetPath]
buildFiles = apply

sourceFileRun :: Options -> SourcePath -> Maybe ByteString -> RunMode -> Action (RunResult TargetPath)
sourceFileRun options key mbPrevious mode = do
  (_version, relevantRule) <- getUserRuleOne key (const Nothing) matchRule
  let newTargetPath =
        (TargetPath . sourceToTarget relevantRule . fromSourcePath) key
      actionPaths =
        P{ source = qualify options key
         , target = qualify options newTargetPath }
  case mode of
    RunDependenciesSame
      | Just previous <- mbPrevious
        -> do let SourceFileR{ targetPath = previousTargetPathQualified } =
                    decode (Bytes.fromStrict previous)
                  newTargetPathQualified = qualify options newTargetPath
              -- We compare the qualified paths, so that we rebuild if the
              -- output directory changes.
              if newTargetPathQualified /= previousTargetPathQualified
                then rebuild relevantRule actionPaths
                else do targetStillExists <- liftIO $ doesFileExist newTargetPathQualified
                        let nothingChanged = RunResult ChangedNothing previous newTargetPath
                        if not targetStillExists
                          then rebuild relevantRule actionPaths
                          else return nothingChanged
      | otherwise -> rebuild relevantRule actionPaths
    RunDependenciesChanged ->
      rebuild relevantRule actionPaths
  where
    -- N.B. that 'paths' contains the real filesystem paths, not the
    -- unqualified ones.
    rebuild rule paths = do
      need [source paths]
      produces [target paths]
      liftIO $ createDirectoryIfMissing True (takeDirectory (target paths))
      run rule paths
      targetContents <- liftIO $ Bytes.readFile (target paths)
      let previousTargetHash = fmap (targetHash . decode . Bytes.fromStrict) mbPrevious
      let newTargetHash = hash targetContents
          changed = if Just newTargetHash == previousTargetHash
                      then ChangedRecomputeSame
                      else ChangedRecomputeDiff
          store = Bytes.toStrict $ encode
                    SourceFileR{ targetHash = newTargetHash
                               , targetPath = target paths }
          result = unqualify options (target paths)
      return (RunResult changed store result)
    matchRule rule@SourceFileRule{..} =
      if match (fromSourcePath key) then Just rule else Nothing
