module Actions ( templateRule, urlRule) where

import Introit

import Data.ByteString.Builder ( hPutBuilder )
import Data.Functor.Identity ( runIdentity )
import Development.Shake
import Development.Shake.FilePath
import Control.Monad.Trans.Reader
import qualified Lucid.Base as Lucid
import System.Directory ( createDirectoryIfMissing )
import qualified System.IO as IO

import Templates
import Routes ( Route )
import qualified Routes


templateRule :: (FilePath -> Route) -> (Route -> Action (Html ())) -> ReaderT FilePath Rules ()
templateRule routeBuilder template =
   urlRule routeBuilder $ \route buildDir -> do
      let targetFile = buildDir </> Routes.targetFile route
      htmlBytes <- (runIdentity . Lucid.execHtmlT) <$> template route
      liftIO $ createDirectoryIfMissing True (takeDirectory targetFile)
      liftIO $ IO.withFile targetFile IO.WriteMode $ \targetHandle ->
         hPutBuilder targetHandle htmlBytes

urlRule :: (FilePath -> Route) -> (Route -> FilePath -> Action ()) -> ReaderT FilePath Rules ()
urlRule routeBuilder action = ReaderT $ \buildDir -> do
   let pattern = buildDir </> Routes.targetFile (routeBuilder "*")
   pattern %> \targetFile -> do
      let (Just [filename]) = filePattern pattern targetFile
          route = routeBuilder filename
      putLoud $ "Calling rule for url " <> show (Routes.url route) <> ", file " <> targetFile
      action route buildDir
