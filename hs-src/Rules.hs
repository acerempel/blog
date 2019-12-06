{-# LANGUAGE TypeFamilies, DerivingStrategies, DeriveGeneric, DeriveAnyClass,
 GeneralizedNewtypeDeriving #-}
module Rules ( intoOutputDir, outOfInputDir, addSourceFileRule, IncludeDraftsQ(..), RunPaths(..), rule, buildFiles ) where

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
  { match :: FilePath -> Bool
  , sourceToTarget :: FilePath -> FilePath
  , run :: RunPaths -> Action () }

data RunPaths =
  P { source :: !FilePath, target :: !FilePath }

newtype SourceFileQ = SourceFileQ FilePath
  -- Derive all the instances that Shake wants
  deriving newtype ( Show, Eq, Hashable, Binary, NFData )

-- | The path that the rule produced.
newtype SourceFileA = SourceFileA { unA :: FilePath }
  -- Derive all the instances that Shake wants
  deriving newtype ( Show, Eq, Hashable, Binary, NFData )

data SourceFileR = SourceFileR
  { targetPath :: !FilePath
  , targetHash :: {-# UNPACK #-} !Int }
  deriving stock ( Eq, Generic )
  deriving anyclass ( Binary )

type instance RuleResult SourceFileQ = SourceFileA

rule :: (FilePath -> Bool) -> (FilePath -> FilePath) -> (RunPaths -> Action ()) -> Rules ()
rule match sourceToTarget run =
  addUserRule SourceFileRule{..}

buildFiles :: [FilePath] -> Action [FilePath]
buildFiles = fmap (map unA) . apply . map SourceFileQ

sourceFileRun :: Options -> SourceFileQ -> Maybe ByteString -> RunMode -> Action (RunResult SourceFileA)
sourceFileRun options key@(SourceFileQ sourcePath) mbPrevious mode = do
  (_version, relevantRule) <- getUserRuleOne key (const Nothing) matchRule
  let newTargetPath = (intoOutputDir options . sourceToTarget relevantRule) (source actionPaths)
      actionPaths = P{ source = outOfInputDir options sourcePath, target = newTargetPath }
  case mode of
    RunDependenciesSame
      | Just previous <- mbPrevious
        -> do let SourceFileR{ targetPath = previousTargetPath } =
                    decode (Bytes.fromStrict previous)
              if newTargetPath /= previousTargetPath
                then rebuild relevantRule actionPaths
                else do targetStillExists <- liftIO $ doesFileExist newTargetPath
                        let nothingChanged = RunResult ChangedNothing previous (SourceFileA newTargetPath)
                        if not targetStillExists
                          then rebuild relevantRule actionPaths
                          else return nothingChanged
      | otherwise -> rebuild relevantRule actionPaths
    RunDependenciesChanged ->
      rebuild relevantRule actionPaths
  where
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
                    SourceFileR{ targetHash = newTargetHash, targetPath = target paths }
          result = SourceFileA (target paths)
      return (RunResult changed store result)
    matchRule rule@SourceFileRule{..} =
      if match sourcePath then Just rule else Nothing
