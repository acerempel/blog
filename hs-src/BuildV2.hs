{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, ViewPatterns #-}
module BuildV2 ( buildSite, Options(..) ) where

import Introit

import Data.Binary ( encode, decode )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Bytes ( fromStrict, toStrict, readFile )
import Development.Shake hiding ( doesFileExist )
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import GHC.Generics ( Generic )
import System.Directory ( doesFileExist )

import Options
import qualified Post
import qualified Templates
import qualified Rules

data IncludeDraftsQ = IncludeDraftsQ
  -- Derive all the instances that Shake wants
  deriving ( Show, Eq, Generic, Hashable, Binary, NFData )
type instance RuleResult IncludeDraftsQ = Bool

data SourceFileRule = SourceFileRule
  { match :: FilePath -> Bool
  , sourceToTarget :: FilePath -> FilePath
  , run :: RunPaths -> Action () }

data RunPaths =
  P { source :: FilePath, target :: FilePath }

newtype SourceFileQ = SourceFileQ FilePath
  -- Derive all the instances that Shake wants
  deriving ( Show, Eq, Generic, Hashable, Binary, NFData )

-- | The path that the rule produced.
newtype SourceFileA = SourceFileA { unA :: FilePath }
  -- Derive all the instances that Shake wants
  deriving ( Show, Eq, Generic, Hashable, Binary, NFData )

data SourceFileR = SourceFileR
  { targetPath :: FilePath
  , targetHash :: Int }
  deriving ( Eq, Generic, Binary )

type instance RuleResult SourceFileQ = SourceFileA

rule :: (FilePath -> Bool) -> (FilePath -> FilePath) -> (RunPaths -> Action ()) -> Rules ()
rule match sourceToTarget run =
  addUserRule SourceFileRule{..}

buildFiles :: [FilePath] -> Action [FilePath]
buildFiles = fmap (map unA) . apply . map SourceFileQ

sourceFileRun :: SourceFileQ -> Maybe ByteString -> RunMode -> Action (RunResult SourceFileA)
sourceFileRun key@(SourceFileQ sourcePath)
              mbPrevious
              mode = do
  (_version, relevantRule) <- getUserRuleOne key (const Nothing) matchRule
  let targetPath = sourceToTarget relevantRule sourcePath
      actionPaths = P{ source = sourcePath, target = targetPath }
  case mode of
    RunDependenciesSame
      | Just previous <- mbPrevious
        -> do let SourceFileR{ targetPath = previousTargetPath } =
                    decode (Bytes.fromStrict previous)
              if targetPath /= previousTargetPath
                then rebuild relevantRule actionPaths
                else do targetStillExists <- liftIO $ doesFileExist targetPath
                        let nothingChanged = RunResult ChangedNothing previous (SourceFileA targetPath)
                        if not targetStillExists
                          then rebuild relevantRule actionPaths
                          else return nothingChanged
      | otherwise -> rebuild relevantRule actionPaths
    RunDependenciesChanged ->
      rebuild relevantRule actionPaths
  where
    rebuild rule paths = do
      produces [target paths]
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


buildSite :: Options -> IO ()
-- TODO: Set the verbosity from the command line.
-- TODO: Automate the updating of the 'shakeVersion'.
buildSite options = shake shakeOptions{shakeVerbosity = Chatty, shakeVersion = "14"} do
  addBuiltinRule noLint noIdentity sourceFileRun
  -- Make sure we parse each markdown file only once – each file is needed
  -- both for its own page and for the home page.
  getPost <- newCache Post.read
  -- This is needed by the "index.html" rule to rebuild when the
  -- `include-drafts` command-line option changes.
  addOracle \IncludeDraftsQ -> return (includeDrafts options)
  Rules.run options do
    Rules.oneToOne "posts/*.md"
      ((</> "index.html") . dropExtension . takeBaseName)
      \source -> Rules.html $ Templates.post <$> getPost source
    Rules.manyToOne "posts/*.md" "index.html" \sources -> Rules.html do
      allPosts <- forP sources getPost
      shouldIncludeDrafts <- askOracle IncludeDraftsQ
      let filterOutDrafts =
            if shouldIncludeDrafts
              then id
              else filter (not . Post.isDraft)
          allPostsSorted =
            sortBy (compare `on` Post.composed) $
            filterOutDrafts allPosts
      return (Templates.archive allPostsSorted)
    Rules.oneToOne "styles/three-dots.scss" (-<.> ".css")
      \source target -> do
        need [source]
        cmd_ ("sass" :: String) [ "--no-source-map", source, target ]
    -- TODO: Support multiple source file patterns -- that way we can copy
    -- all fonts, stylesheets, and js with one rule.
    Rules.oneToOne "styles/*.css" id copyFileChanged
