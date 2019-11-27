{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules where

import Introit
import Options
import qualified Templates

import Control.Monad.Fail ( MonadFail )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Class ( lift )
import qualified Control.Monad.Trans.Reader as R
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as Map
import Development.Shake
import Development.Shake.FilePath
import qualified Lucid

newtype SiteM a =
  SiteM (R.ReaderT Environment (S.StateT State Rules) a)
  deriving newtype ( Functor, Applicative, Monad, MonadFail, MonadIO )

data Environment =
  Environment
  { options :: !Options }

type DirectoryCache = HashMap FilePattern [SourcePath]

data State = State
  { directoryCache :: !DirectoryCache
  , topLevelTargets :: ![QualifiedTargetPath] }

emptyState :: State
emptyState = State Map.empty []

type SourcePath = FilePath
type TargetPath = FilePath
type QualifiedSourcePath = FilePath
type QualifiedTargetPath = FilePath

getDirectoryFilesCached :: FilePattern -> SiteM [SourcePath]
getDirectoryFilesCached pattern = do
  cache <- SiteM (lift (S.gets directoryCache))
  case Map.lookup pattern cache of
    Just files ->
      return files
    Nothing -> do
      directory <- query inputDirectory
      files <- liftIO $ getDirectoryFilesIO directory [pattern]
      liftState $ S.modify' (\s -> s{directoryCache = Map.insert pattern files cache})
      return files

addTopLevelTarget :: TargetPath -> SiteM ()
addTopLevelTarget path = do
  outDir <- query outputDirectory
  let qualifiedPath = outDir </> path
  liftState $ S.modify' (\s -> s{ topLevelTargets = qualifiedPath : topLevelTargets s })

oneToOne :: FilePattern -> (SourcePath -> TargetPath) -> (QualifiedSourcePath -> QualifiedTargetPath -> Action ()) -> SiteM ()
oneToOne sourcePattern toTargetPath pathsToAction = do
  sourceFiles <- getDirectoryFilesCached sourcePattern
  (inDir, outDir) <- (,) <$> query inputDirectory <*> query outputDirectory
  forM_ sourceFiles \sourceFile -> do
    let targetFile = toTargetPath sourceFile
        qualifiedTargetFile = outDir </> targetFile
        qualifiedSourceFile = inDir </> sourceFile
    addTopLevelTarget targetFile
    liftRules $ qualifiedTargetFile %> \_ -> pathsToAction qualifiedSourceFile qualifiedTargetFile

manyToOne :: FilePattern -> TargetPath -> ([QualifiedSourcePath] -> QualifiedTargetPath -> Action ()) -> SiteM ()
manyToOne sourcePattern targetFile pathsToAction = do
  sourceFiles <- getDirectoryFilesCached sourcePattern
  (inDir, outDir) <- (,) <$> query inputDirectory <*> query outputDirectory
  let qualifiedTargetFile = outDir </> targetFile
      qualifiedSourceFiles = map (inDir </>) sourceFiles
  addTopLevelTarget targetFile
  liftRules $ qualifiedTargetFile %> \_ -> pathsToAction qualifiedSourceFiles qualifiedTargetFile


run :: Options -> SiteM a -> Rules a
run options (SiteM m) = do
  let environment = Environment options
  (result, State{topLevelTargets}) <-
    S.runStateT (R.runReaderT m environment) emptyState
  want topLevelTargets
  return result

query :: (Options -> a) -> SiteM a
query question =
  SiteM (R.asks (question . options))

liftRules :: Rules a -> SiteM a
liftRules = SiteM . lift . lift

liftState :: S.StateT State Rules a -> SiteM a
liftState = SiteM . lift

html :: Action Templates.PageContent -> FilePath -> Action ()
html makeAction = \targetFile -> do
  markup <- makeAction
  -- TODO: propagate IncludeTags and any other information the template
  -- needs!
  liftIO $ Lucid.renderToFile targetFile (Templates.page False markup)
