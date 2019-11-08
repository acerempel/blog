{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules where

import Introit
import List ( List )
import qualified List
import Options

import Control.Monad.Fail ( MonadFail )
import Control.Monad.IO.Class ( liftIO )
import qualified Control.Monad.Trans.Accum as A
import Control.Monad.Trans.Class ( lift )
import qualified Control.Monad.Trans.Reader as R
import Development.Shake
import Development.Shake.FilePath
import Lucid ( Html )
import qualified Lucid
import System.Directory ( createDirectoryIfMissing )

newtype SiteM a =
  SiteM (R.ReaderT Environment (A.AccumT (List FilePath) Rules) a)
  deriving newtype ( Functor, Applicative, Monad, MonadFail )

data Environment =
  Environment
  { options :: !Options
  , baseTemplate :: !Template }

type Template = Html () -> Html ()

data RuleParameters =
  RuleParameters
  { inputPath :: !FilePath
  , outputPath :: !FilePath }

run :: Options -> Template -> SiteM a -> Rules a
run options template (SiteM m) = do
  let environment = Environment options template
  (result, targets) <- A.runAccumT (R.runReaderT m environment) List.empty
  let targetsQualifiedWithOutputDir =
        (outputDirectory options </>) `fmap` targets
  want (toList targetsQualifiedWithOutputDir)
  return result

targets :: List FilePath -> SiteM ()
targets paths =
  SiteM (lift (A.add paths))

query :: (Options -> a) -> SiteM a
query question =
  SiteM (R.asks (question . options))

liftRules :: Rules a -> SiteM a
liftRules = SiteM . lift . lift

html :: FilePattern -> (RuleParameters -> Action (Html ())) -> SiteM ()
html pattern makeAction = do
  inputDir <- query inputDirectory
  outputDir <- query outputDirectory
  let outputPattern = outputDir </> pattern
  template <- SiteM (R.asks baseTemplate)
  liftRules $ outputPattern %> \outputPath -> do
    let inputPath = joinPath $ inputDir : List.tail (splitDirectories outputPath)
        parameters = RuleParameters{ inputPath, outputPath }
    markup <- makeAction parameters
    liftIO $ createDirectoryIfMissing True (takeDirectory outputPath)
    liftIO $ Lucid.renderToFile outputPath (template markup)
