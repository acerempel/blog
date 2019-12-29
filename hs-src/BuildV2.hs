{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf, TupleSections, LambdaCase #-}
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
import Data.IORef
import Data.Text.Encoding ( decodeUtf8 )
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Conc ( atomically )
import GHC.Generics ( Generic )
import System.Directory ( copyFileWithMetadata )
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

type instance RuleResult FdQ = [Lazy.ByteString]

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
        { thingTargetPath = TargetPath $ Bytes.unpack sansExt </> "index.html"
        , thingUrl = url sansExt }
     | Just sansExt <- Bytes.stripSuffix ".scss" path
     -> Thing
        { thingTargetPath = TargetPath $ Bytes.unpack sansExt <.> "css"
        , thingUrl = url $ sansExt <> ".css" }
     | otherwise
     -> Thing{ thingTargetPath = TargetPath (Bytes.unpack path)
             , thingUrl = url path }
  where
    url = URL . decodeUtf8 . ("/" <>)

data Path
  = Source !SourcePath
  | Target !FilePath
  deriving stock ( Generic, Eq )
  deriving anyclass ( Hashable )

buildSite :: Options -> IO ()
-- TODO: Automate the updating of the 'shakeVersion'.
buildSite options@Options{..} =
  let shakeOptions'' =
        shakeOptions'
        { shakeRebuild = [ (RebuildNow, outputDirectory </> pattern) | pattern <- rebuildPatterns ]
        , shakeVerbosity = verbosity }
  in shake shakeOptions'' do

  thingsDatabase <- liftIO $ newIORef Map.empty

  addOracle \InputDir -> return inputDirectory

  -- This is needed by the "index.html" rule to rebuild when the
  -- `include-drafts` command-line option changes.
  addOracle \IncludeDrafts -> return includeDrafts

  addOracle \Fd{..} -> do
    askOracle InputDir
    let fdConfig =
          setWorkingDir inputDirectory $
          setStdout byteStringOutput $
          proc "fd" $ "." : prefixDir : fdArgs
        fdArgs = ["-L", "--max-depth", "1", "--type", "file"]
                 ++ concat [ ["-e", ext] | ext <- extensions ]
    output <- doProcessWith (atomically . getStdout) options fdConfig
    return (Lazy.lines output)

  let realSourcePath =
        (inputDirectory </>) . Bytes.unpack . fromSourcePath
      realTargetPath =
        (outputDirectory </>) . (contentSubDir </>) . fromTargetPath
      realPath = \case
        Source sp -> realSourcePath sp
        Target tp -> tp

  let
    lookupThing targetPath = do
      mbThing <- Map.lookup targetPath <$> liftIO (readIORef thingsDatabase)
      case mbThing of
        Just thing -> return thing
        Nothing -> liftIO $ throwIO $ Post.ThingNotFound (realPath targetPath)

    registerThings things db =
      foldl' (\db' thing ->
        Map.insert ((Target . realTargetPath . thingTargetPath) thing) thing .
        Map.insert ((Source . thingSourcePath) thing) thing $ db')
      db things

  getMarkdownThing <- newCache \thing -> do
    need [realSourcePath (thingSourcePath thing)]
    contents <- liftIO $ Text.readFile (realSourcePath (thingSourcePath thing))
    either (liftIO . throwIO) return $! Post.parse thing contents

  -- Make sure we parse each markdown file only once – each file is needed
  -- both for its own page and for the home page.
  getMarkdown <- newCache \targetPath -> do
    let
      catchNotFound =
        case targetPath of
          Target _ -> id
          Source (SourcePath sp) ->
            flip actionCatch (\(ThingNotFound _) -> return (sourcePathToThing sp))
    thing <- catchNotFound (lookupThing targetPath)
    getMarkdownThing thing

  getThings <- newCache \fd@Fd{..} -> do
    fdOutput <- askOracle fd
    let results = map Lazy.toStrict fdOutput
    let things = map sourcePathToThing results
    liftIO $ atomicModifyIORef' thingsDatabase ((,()) . registerThings things)
    return things

  getAllPosts <- newCache \() -> do
    sources <- getThings (Fd "posts" ["md"])
    allPosts <- forP sources getMarkdownThing
    shouldIncludeDrafts <- askOracle IncludeDrafts
    let filterOutDrafts =
          if shouldIncludeDrafts then id else filter (not . Post.isDraft)
    return $
      sortBy (compare `on` (Down . Post.published)) $
      filterOutDrafts allPosts

  let target = realTargetPath . TargetPath
      source = realSourcePath . SourcePath

      assetPatterns =
        [ Fd "scripts" ["js"], Fd "fonts" ["woff", "woff2"]
        , Fd "images" ["jpg", "jpeg", "png"] ]
      assetExtensions = foldMap extensions assetPatterns

  action do
    let sourcePatterns =
          Fd "posts" ["md"] : assetPatterns
        staticTargets = map target
          [ "index.html", "introduction/index.html", "posts/index.html", "styles.css" ]
    things <- forP sourcePatterns getThings
    need (map (realTargetPath . thingTargetPath) (concat things) ++ staticTargets)

  target "posts/*/index.html" %> \targetPath -> do
    page <- Templates.post <$> getMarkdown (Target targetPath)
    writeHtml targetPath page

  target "introduction/index.html" %> \targetPath -> do
    page <- Templates.aboutPage <$> getMarkdown (Source "introduction.md")
    writeHtml targetPath page

  map (target . ("**/*" <.>)) assetExtensions |%> \targetPath -> do
    sourcePath <- (realSourcePath . thingSourcePath) <$> lookupThing (Target targetPath)
    liftIO $ copyFileWithMetadata sourcePath targetPath

  target "styles.css" %> \targetPath -> do
    let sourcePath = source "styles.scss"
    need [sourcePath, source "fonts/fonts.css"]
    doProcess options $ proc "sass" [ "--no-source-map", sourcePath, targetPath ]

  target "index.html" %> \targetPath -> do
    allPosts <- getAllPosts ()
    hi <- getMarkdown (Source "hi.md")
    writeHtml targetPath $ Templates.home hi (take 5 allPosts)

  target "posts/index.html" %> \targetPath -> do
    allPosts <- getAllPosts ()
    writeHtml targetPath $ Templates.archive allPosts

  -- (outputDirectory </> uploadedStateSubDir </> "*" <.> "uploaded") %> \target -> do
  --   let realTarget =
  --         (dropExtension . dropDirectory1 . dropDirectory1) target
  --   need [realTarget]


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
