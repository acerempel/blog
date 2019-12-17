{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module BuildV2 ( buildSite, Options(..) ) where

import Introit
import qualified Text

import Control.Exception ( throwIO )
import Data.Coerce
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Development.Shake
import Development.Shake.FilePath
import System.Directory ( copyFileWithMetadata )
import System.Process.Typed

import FilePath
import Options
import qualified Post
import qualified Templates
import Rules
import Write

version = "50"

shakeOptions' = shakeOptions
  {shakeVerbosity = Chatty, shakeVersion = version}

buildSite :: Options -> IO ()
-- TODO: Set the verbosity from the command line.
-- TODO: Automate the updating of the 'shakeVersion'.
buildSite options@Options{..} =
  let shakeOptions'' =
        shakeOptions'{shakeRebuild = [ (RebuildNow, outputDirectory </> pattern) | pattern <- rebuildPatterns ]}
  in shake shakeOptions'' do
  addSourceFileRule options
  addEverythingRule options

  let assetExts = Set.fromList [".js", ".jpg", ".jpeg", ".png", ".woff", ".woff2"]
      assetPatterns = map ("**/*" <>) $ Set.toList assetExts
      staticTargets = ["index.html", "posts/index.html", "styles.css"]

  -- This is needed by the "index.html" rule to rebuild when the
  -- `include-drafts` command-line option changes.
  addOracle \IncludeDraftsQ -> return includeDrafts

  -- Make sure we parse each markdown file only once – each file is needed
  -- both for its own page and for the home page.
  getMarkdown <- newCache \nominalPath -> do
    let realPath = qualify options nominalPath
    need [realPath]
    contents <- liftIO $ Text.readFile realPath
    either (liftIO . throwIO) return (Post.parse nominalPath contents)

  getSourceFiles <- newCache ((map SourcePath <$>). getDirectoryFiles inputDirectory)

  getAllPosts <- newCache \() -> do
    sources <- getSourceFiles ["posts/*.md"]
    allPosts <- forP sources getMarkdown
    shouldIncludeDrafts <- askOracle IncludeDraftsQ
    let filterOutDrafts =
          if shouldIncludeDrafts then id else filter (not . Post.isDraft)
    return $
      sortBy (compare `on` (Down . Post.published)) $
      filterOutDrafts allPosts

  action do
    sourceFiles <- getSourceFiles ("**/*.md" : assetPatterns)
    changedForward <- buildFiles sourceFiles
    changedBackward <- buildEverything staticTargets
    let changed = changedBackward ++ changedForward
    when upload (runAfter (doUpload options changed))

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

  let mkTarget = qualify @TargetPath options

  mkTarget "styles.css" %> \target -> do
    let source = inputDirectory </> dropDirectory1 target -<.> ".scss"
    need [source]
    cmd_ ("sass" :: String) [ "--no-source-map", source, target ]

  mkTarget "index.html" %> \target -> do
    allPosts <- getAllPosts ()
    hi <- getMarkdown "hi.md"
    writeHtml target $ Templates.home hi (take 5 allPosts)

  mkTarget "posts/index.html" %> \target -> do
    allPosts <- getAllPosts ()
    writeHtml target $ Templates.archive allPosts

f &&^ g = \a -> f a && g a

f ==^ a = \b -> f b == a

doUpload :: Options -> [TargetPath] -> IO ()
doUpload options changed = do
  if null changed then
    hPutStrLn stderr "Nothing to upload!"
  else do
    for_ (Map.toList changedGroupedByDir) \(directory, paths) ->
      withProcessWait_
        (uploadCommand directory paths)
        (\process -> hPutStrLn stderr (show process))
  where
    uploadCommand directory paths =
      setWorkingDir (outputDirectory options) $
      let args = "upload" : if directory == "." then paths else ["-d", directory] ++ paths
      in proc "neocities" args
    changedGroupedByDir =
      Map.fromListWith (<>)
        [ (takeDirectory path, [path]) | path <- coerce changed ]
