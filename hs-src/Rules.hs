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
import Development.Shake
import Development.Shake.FilePath
import Lucid ( Html )
import qualified Lucid
import qualified System.FilePattern as FP

newtype SiteM a =
  SiteM (R.ReaderT Environment (A.AccumT ([RuleParameters FilePattern]) Rules) a)
  deriving newtype ( Functor, Applicative, Monad, MonadFail )

data Environment =
  Environment
  { options :: !Options
  , baseTemplate :: !Template }

type Template = Templates.PageContent -> Html ()

data RuleParameters a =
  RuleParameters
  { source :: !a
  , target :: !a }

run :: Options -> Template -> SiteM a -> Rules a
run options template (SiteM m) = do
  let environment = Environment options template
  (result, rulePatterns) <- A.runAccumT (R.runReaderT m environment) List.empty
  action do
    targetFiles <- forP rulePatterns \RuleParameters{source, target} -> do
      sourceFiles <- getDirectoryFiles (inputDirectory options) [source]
      return $ fmap (FP.substitute target . fromJust . FP.match source) sourceFiles
    -- getDirectoryFiles does not qualify the results with the name of the
    -- given directory, i.e. the results will match one of the pattern
    -- arguments.
    need ((fmap (outputDirectory options </>) . concat) targetFiles)
  return result

query :: (Options -> a) -> SiteM a
query question =
  SiteM (R.asks (question . options))

liftRules :: Rules a -> SiteM a
liftRules = SiteM . lift . lift

html :: FilePattern -> FilePattern -> (FilePath -> Action Templates.PageContent) -> SiteM ()
html sourcePattern targetPattern makeAction = do
  template <- SiteM (R.asks baseTemplate)
  let makeAction' RuleParameters{source, target} = do
        markup <- makeAction source
        liftIO $ Lucid.renderToFile target (template markup)
      targetPattern' =
        targetPattern </> "index.html"
  rule sourcePattern targetPattern' makeAction'

rule :: FilePattern -> FilePattern -> (RuleParameters FilePath -> Action ()) -> SiteM ()
rule sourcePattern targetPattern makeAction
  | FP.arity targetPattern == 0 =
    undefined
  | FP.arity sourcePattern /= FP.arity targetPattern =
    -- TODO: Make a custom exception type and throw that in IO.
    error "Arity of patterns doesn't match!"
  | otherwise = do
    inputDir <- query inputDirectory
    outputDir <- query outputDirectory
    let parameters = RuleParameters{target = targetPattern, source = sourcePattern}
    SiteM (lift (A.add [parameters]))
    let targetPattern' = outputDir </> targetPattern
        sourcePattern' = inputDir </> sourcePattern
    liftRules $ targetPattern' %> \target -> do
      let Just parts = FP.match targetPattern' target
          source = FP.substitute sourcePattern' parts
      let parameters = RuleParameters{ source, target }
      makeAction parameters
