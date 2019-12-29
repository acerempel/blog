{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf, TupleSections, LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module BuildV2 ( buildSite, Options(..) ) where

import Introit
import qualified Text

import Control.Exception ( throwIO )
import Data.ByteString.Builder ( hPutBuilder, intDec )
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
import Lucid ( execHtmlT )
import System.Directory ( copyFileWithMetadata )
import System.IO ( withBinaryFile, IOMode(..) )
import System.Process.Typed

import Thing
import Options
import qualified Post
import qualified Templates

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
        { shakeRebuild = [ (RebuildNow, outputDirectory </> contentSubDir </> pattern) | pattern <- rebuildPatterns ]
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

  let sourceToInputPath =
        (inputDirectory </>) . Bytes.unpack . fromSourcePath
      targetToOutputPath =
        (outputDirectory </>) . (contentSubDir </>) . fromTargetPath
      realPath = \case
        Source sp -> sourceToInputPath sp
        Target tp -> tp
      targetToUploadedPath (TargetPath tp) =
        outputDirectory </> uploadedStateSubDir </> tp <.> "uploaded"
      uploadedToTargetPath =
        TargetPath . dropExtension . tail . fromJust .
        stripPrefix (outputDirectory </> uploadedStateSubDir)

  let
    lookupThing targetPath = do
      mbThing <- Map.lookup targetPath <$> liftIO (readIORef thingsDatabase)
      case mbThing of
        Just thing -> return thing
        Nothing -> liftIO $ throwIO $ Post.ThingNotFound (realPath targetPath)

    registerThings things db =
      foldl' (\db' thing ->
        Map.insert ((Target . targetToOutputPath . thingTargetPath) thing) thing .
        Map.insert ((Source . thingSourcePath) thing) thing $ db')
      db things

  getMarkdownThing <- newCache \thing -> do
    need [sourceToInputPath (thingSourcePath thing)]
    contents <- liftIO $ Text.readFile (sourceToInputPath (thingSourcePath thing))
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

  let target = targetToOutputPath . TargetPath
      source = sourceToInputPath . SourcePath

      assetPatterns =
        [ Fd "scripts" ["js"], Fd "fonts" ["woff", "woff2"]
        , Fd "images" ["jpg", "jpeg", "png"] ]
      assetExtensions = foldMap extensions assetPatterns
      postPattern = Fd "posts" ["md"]

  getAllPosts <- newCache \() -> do
    sources <- getThings postPattern
    allPosts <- forP sources getMarkdownThing
    shouldIncludeDrafts <- askOracle IncludeDrafts
    let filterOutDrafts =
          if shouldIncludeDrafts then id else filter (not . Post.isDraft)
    return $
      sortBy (compare `on` (Down . Post.published)) $
      filterOutDrafts allPosts

  action do
    let sourcePatterns = postPattern : assetPatterns
        staticTargets = map TargetPath
          [ "index.html", "introduction/index.html", "posts/index.html", "styles.css" ]
    things <- forP sourcePatterns getThings
    let allTargets = map thingTargetPath (concat things) <> staticTargets
    if upload
      then need (map targetToUploadedPath allTargets)
      else need (map targetToOutputPath allTargets)

  target "posts/*/index.html" %> \targetPath -> do
    page <- Templates.post <$> getMarkdown (Target targetPath)
    writeHtml targetPath page

  target "introduction/index.html" %> \targetPath -> do
    page <- Templates.aboutPage <$> getMarkdown (Source "introduction.md")
    writeHtml targetPath page

  map (target . ("**/*" <.>)) assetExtensions |%> \targetPath -> do
    sourcePath <- (sourceToInputPath . thingSourcePath) <$> lookupThing (Target targetPath)
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

  targetToUploadedPath (TargetPath "**/*") %> \uploadedPath -> do
    let targetPath = uploadedToTargetPath uploadedPath
    need [targetToOutputPath targetPath]
    doUpload options targetPath
    liftIO do
      targetContents <- Lazy.readFile (targetToOutputPath targetPath)
      let targetHash = intDec (hash targetContents)
      withBinaryFile uploadedPath WriteMode \handle -> hPutBuilder handle targetHash


doProcess = doProcessWith (\_ -> return ())

doProcessWith act options processConfig = liftIO do
  withProcessWait_ processConfig
    \process -> do
      when (verbosity options >= Chatty) do
        hPutStrLn stderr (show process)
      act process

doUpload :: Options -> TargetPath -> Action ()
doUpload options (TargetPath path) = do
  doProcess options $
    setWorkingDir (outputDirectory options </> contentSubDir) $
    let directory = takeDirectory path
        args = "upload" : if directory /= "." then ["-d", directory, path] else [path]
    in proc "neocities" args

writeHtml :: FilePath -> Templates.PageContent -> Action ()
writeHtml outPath content = do
  let htmlOrProblem = execHtmlT (Templates.page False content)
  case htmlOrProblem of
    Left problem ->
      liftIO $ throwIO problem
    Right html -> do
      putNormal $ "Writing HTML to " <> outPath
      liftIO $ withBinaryFile outPath WriteMode \handle -> hPutBuilder handle html
