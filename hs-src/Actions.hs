module Actions ( templateRule, urlRule) where

import Introit

import Data.ByteString.Builder ( hPutBuilder )
import Development.Shake
import Development.Shake.FilePath
import System.Directory ( createDirectoryIfMissing )
import qualified System.IO as IO

import Templates
import Routes ( Route )
import qualified Routes


templateRule :: Route route => FilePath -> (FilePath -> route) -> (route -> Template ()) -> Rules ()
templateRule buildDir routeBuilder template = do
   urlRule buildDir routeBuilder $ \route -> do
      let url = Routes.url route
          targetFile = buildDir </> Routes.targetFile route
      htmlBytes <- render (template route) route
      putLoud $ "Writing url " <> show url <> " to file " <> targetFile
      liftIO $ createDirectoryIfMissing True (takeDirectory targetFile)
      liftIO $ IO.withFile targetFile IO.WriteMode $ \targetHandle ->
         hPutBuilder targetHandle htmlBytes

urlRule :: Route route => FilePath -> (FilePath -> route) -> (route -> Action ()) -> Rules ()
urlRule buildDir routeBuilder action = do
   let pattern = buildDir </> Routes.targetFile (routeBuilder "*")
   pattern %> \targetFile -> do
      let (Just [filename]) = filePattern pattern targetFile
      action (routeBuilder filename)
