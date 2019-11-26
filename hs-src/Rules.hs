{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules where

import Introit
import qualified List
import Options
import qualified Templates

import Control.Monad.Fail ( MonadFail )
import Control.Monad.IO.Class ( liftIO )
import qualified Control.Monad.Trans.Accum as A
import Control.Monad.Trans.Class ( lift )
import qualified Control.Monad.Trans.Reader as R
import Data.IORef
import qualified Data.Map.Strict as Map
import Development.Shake
import Development.Shake.FilePath
import qualified Lucid

newtype SiteM a =
  SiteM (R.ReaderT Environment (A.AccumT ([Rule]) Rules) a)
  deriving newtype ( Functor, Applicative, Monad, MonadFail )

data Environment =
  Environment
  { options :: !Options }

data Rule = Rule
  { sourcePattern :: !FilePattern
  , targetsAndAction :: !TargetsAndAction }

type SourcePath = FilePath
type TargetPath = FilePath
type QualifiedTargetPath = FilePath

data TargetsAndAction
  = OneToOne (SourcePath -> TargetPath) (SourcePath -> QualifiedTargetPath -> Action ())
  | ManyToOne TargetPath ([SourcePath] -> QualifiedTargetPath -> Action ())
  | OneToSome (SourcePath -> [TargetPath]) (SourcePath -> [QualifiedTargetPath] -> Action ())
  | ManyToSome [TargetPath] ([SourcePath] -> [QualifiedTargetPath] -> Action ())

run :: Options -> SiteM a -> Rules a
run options (SiteM m) = do
  let environment = Environment options
  (result, rules) <- A.runAccumT (R.runReaderT m environment) List.empty
  topLevelTargets <- liftIO $ newIORef []
  directoryContents <- liftIO $ newIORef Map.empty
  forM_ rules \Rule{sourcePattern, targetsAndAction} -> do
    -- I dunno about this IORef stuff … feels unstylish.
    sourceFiles <- liftIO do
      directoryCache <- readIORef directoryContents
      case Map.lookup sourcePattern directoryCache of
        Just files ->
          return files
        Nothing -> do
          files <- getDirectoryFilesIO (inputDirectory options) [sourcePattern]
          modifyIORef directoryContents (Map.insert sourcePattern files)
          return files
    case targetsAndAction of
      OneToOne sourceToTarget targetToAction ->
        forM_ sourceFiles \sourceFile -> do
          let qualifiedTargetFile = outputDirectory options </> sourceToTarget sourceFile
              qualifiedSourceFile = inputDirectory options </> sourceFile
          liftIO $ modifyIORef topLevelTargets (qualifiedTargetFile :)
          qualifiedTargetFile %> \_ -> targetToAction qualifiedSourceFile qualifiedTargetFile
      ManyToOne targetPath pathsToAction -> do
        let qualifiedTargetFile = outputDirectory options </> targetPath
            qualifiedSourceFiles = map (inputDirectory options </>) sourceFiles
        liftIO $ modifyIORef topLevelTargets (qualifiedTargetFile :)
        qualifiedTargetFile %> \_ -> pathsToAction qualifiedSourceFiles qualifiedTargetFile
      _ -> error "not yet implemented!"
  targets <- liftIO $ readIORef topLevelTargets
  want targets
  return result

query :: (Options -> a) -> SiteM a
query question =
  SiteM (R.asks (question . options))

liftRules :: Rules a -> SiteM a
liftRules = SiteM . lift . lift

rule :: FilePattern -> TargetsAndAction -> SiteM ()
rule pattern tAndA = SiteM . lift . A.add $ [Rule pattern tAndA]

html :: Action Templates.PageContent -> FilePath -> Action ()
html makeAction = \targetFile -> do
  markup <- makeAction
  -- TODO: propagate IncludeTags and any other information the template
  -- needs!
  liftIO $ Lucid.renderToFile targetFile (Templates.page False markup)
