{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module BuildV2 ( buildSite, Options(..) ) where

import Introit
import qualified Text

import Control.Exception ( throwIO )
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.Coerce
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Text.Encoding ( decodeUtf8 )
import Development.Shake
import Development.Shake.Classes
import GHC.Conc ( atomically )
import GHC.Generics ( Generic )
import System.Directory ( copyFileWithMetadata )
import System.FilePath.ByteString
import System.Process.Typed

import Thing
import Options
import qualified Post
import qualified Templates
import Write

version = "50"

shakeOptions' = shakeOptions {shakeVersion = version}

contentSubDir = "hypertext"
uploadedStateSubDir = "uploaded-state-cache"

data FdQ = Fd
  { prefixDir :: DirectoryPath
  , extensions :: [String] }
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( Binary, Hashable, NFData )

type instance RuleResult FdQ = Lazy.ByteString

data InputDirQ = InputDir
  -- Derive all the instances that Shake wants
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult InputDirQ = FilePath

data IncludeDraftsQ = IncludeDrafts
  -- Derive all the instances that Shake wants
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult IncludeDraftsQ = Bool

listDirectory dir exts =
  askOracle (Fd dir exts)

sourcePathToThing path =
  (\thing -> thing{thingSourcePath = SourcePath path})
  if | Just sansExt <- Bytes.stripSuffix ".md" path
     -> Thing
        { thingTargetPath = TargetPath $ sansExt </> "index.html"
        , thingUrl = url sansExt }
     | Just sansExt <- Bytes.stripSuffix ".scss" path
     -> Thing
        { thingTargetPath = TargetPath $ sansExt <.> "css"
        , thingUrl = url $ sansExt <.> "css" }
     | otherwise
     -> Thing{ thingTargetPath = TargetPath path
             , thingUrl = url path }
  where
    url = URL . decodeUtf8 . ("/" <>)

buildSite :: Options -> IO ()
-- TODO: Automate the updating of the 'shakeVersion'.
buildSite options@Options{..} =
  let shakeOptions'' =
        shakeOptions'
        { shakeRebuild = [ (RebuildNow, outputDirectory </> pattern) | pattern <- rebuildPatterns ]
        , shakeVerbosity = verbosity }
  in shake shakeOptions'' do

  addOracle \InputDir -> return inputDirectory

  -- This is needed by the "index.html" rule to rebuild when the
  -- `include-drafts` command-line option changes.
  addOracle \IncludeDrafts -> return includeDrafts

  addOracle \Fd{..} -> do
    askOracle InputDir
    let fdConfig =
          setWorkingDir inputDirectory $
          setStdout byteStringOutput $
          proc "fd" $ "." : prefixDir : concat [ ["-e", ext] | ext <- extensions ]
    doProcessWith (atomically . getStdout) options fdConfig

  let assetExts = Set.fromList ["js", "jpg", "jpeg", "png", "woff", "woff2"]
      staticTargets = ["index.html", "posts/index.html", "styles.css"]
      realSourcePath =
        Bytes.unpack . (inputDirectory </>) .
        fromSourcePath . thingSourcePath
      realTargetPath =
        Bytes.unpack . (outputDirectory </>) . (contentSubDir </>) .
        fromTargetPath . thingTargetPath

  let
    lookupThing targetPath =
      liftIO $ error "whoops!"

  -- Make sure we parse each markdown file only once – each file is needed
  -- both for its own page and for the home page.
  getMarkdown <- newCache \targetPath -> do
    thing <- lookupThing targetPath
    needBS [fromSourcePath (thingSourcePath thing)]
    contents <- liftIO $ Text.readFile (realSourcePath thing)
    either (liftIO . throwIO) return $! Post.parse thing contents

  getSources <- newCache \fd@Fd{..} -> do
    fdOutput <- askOracle fd
    let results = map Lazy.toStrict $ Lazy.lines fdOutput
    let things = map sourcePathToThing results
    return things

  getAllPosts <- newCache \() -> do
    sources <- getSources (Fd "posts" ["md"])
    allPosts <- forP sources getMarkdown
    shouldIncludeDrafts <- askOracle IncludeDrafts
    let filterOutDrafts =
          if shouldIncludeDrafts then id else filter (not . Post.isDraft)
    return $
      sortBy (compare `on` (Down . Post.published)) $
      filterOutDrafts allPosts

  action do
    return ()

  {-
  rule ((".md" `isExtensionOf`) &&^ (takeDirectory ==^ "posts"))
    ((</> "index.html") . dropExtension)
    \P{ source, target } -> do
      page <- Templates.post <$> getMarkdown (unqualify options source)
      writeHtml target page

  rule ((".md" `isExtensionOf`) &&^ (takeDirectory ==^ "."))
    ((</> "index.html") . dropExtension)
    \P{ source, target } -> do
      page <- Templates.aboutPage <$> getMarkdown (unqualify options source)
      writeHtml target page

  rule ((`Set.member` assetExts) . takeExtension) id
    \P{ source, target } ->
      liftIO $ copyFileWithMetadata source target
  -}

  let mkTarget t = Bytes.unpack (outputDirectory </> contentSubDir </> t)

  mkTarget "styles.css" %> \target -> do
    let source = inputDirectory </> dropDirectory1 target -<.> ".scss"
    need [source, inputDirectory </> "fonts" </> "fonts.css"]
    doProcess options $ proc "sass" [ "--no-source-map", source, target ]

  mkTarget "index.html" %> \target -> do
    allPosts <- getAllPosts ()
    hi <- getMarkdown "hi.md"
    writeHtml target $ Templates.home hi (take 5 allPosts)

  mkTarget "posts/index.html" %> \target -> do
    allPosts <- getAllPosts ()
    writeHtml target $ Templates.archive allPosts

  (outputDirectory </> uploadedStateSubDir </> "*" <.> "uploaded") %> \target -> do
    let
      realTarget = (dropExtension . dropDirectory1 . dropDirectory1) target
    need [realTarget]


f &&^ g = \a -> f a && g a

f ==^ a = \b -> f b == a

doProcess = doProcessWith (\_ -> return ())

doProcessWith act options processConfig = liftIO do
  withProcessWait_ processConfig
    \process -> do
      when (verbosity options >= Chatty) do
        hPutStrLn stderr (show process)
      act process

doUpload :: Options -> [TargetPath] -> Action ()
doUpload options changed = do
  if null changed then
    putNormal "Nothing to upload!"
  else do
    for_ (Map.toList changedGroupedByDir) \(directory, paths) ->
      doProcess options (uploadCommand directory paths)
  where
    uploadCommand directory paths =
      setWorkingDir (outputDirectory options) $
      let args = "upload" : if directory == "." then paths else ["-d", directory] ++ paths
      in proc "neocities" args
    changedGroupedByDir =
      Map.fromListWith (<>)
        [ (takeDirectory path, [path]) | path <- coerce changed ]
