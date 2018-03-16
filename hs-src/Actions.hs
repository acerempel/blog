module Actions ( Context(..)
               , templateRule, urlRule
               , urlToFile, fileToUrl
               ) where

import Introit

import Data.ByteString.Builder ( hPutBuilder )
import Development.Shake
import Development.Shake.FilePath
import System.Directory ( createDirectoryIfMissing )
import qualified System.IO as IO
import qualified Text

import Templates
import Post


data Context = Context
   { getAllMarkdownSourceFiles :: FilePath -- ^ Directory in which to find post sources
                               -> Action [FilePath]
   , getAllPosts :: () -> Action [Post]
   , getPost :: FilePath -> Action Post }

templateRule :: FilePath -> URLPattern -> Template () -> Rules ()
templateRule buildDir pattern template =
   urlRule buildDir pattern $ \url targetFile -> do
      putLoud $ "templateRule " <> show pattern <> " called for url " <> show url
      htmlBytes <- render template url
      putLoud $ "Writing url " <> Text.unpack url <> " to file " <> targetFile
      liftIO $ createDirectoryIfMissing True (takeDirectory targetFile)
      liftIO $ IO.withFile targetFile IO.WriteMode $ \targetHandle ->
         hPutBuilder targetHandle htmlBytes

urlRule :: FilePath -> URLPattern -> (URL -> FilePath -> Action ()) -> Rules ()
urlRule buildDir pattern action = do
   let targetFilePattern = urlToFile buildDir pattern
   targetFilePattern %> \targetFile -> do
      let targetURL = fileToUrl buildDir targetFile
      action targetURL targetFile

urlToTargetFile :: FilePath  -- ^ Build directory
          -> URL -> FilePath
urlToTargetFile buildDir url =
   let base = tail (Text.unpack url) -- Drop the leading path separator.
   in (if hasExtension base
         then id
         else (</> "index.html")) -- For "clean urls".
      $ buildDir </> base

fileToUrl :: FilePath -- ^ Build directory
          -> FilePath -> URL
fileToUrl buildDir path =
   let base = fromMaybe (error "Path does not begin with build directory!") $
               stripPrefix buildDir path
   in Text.pack $
      if takeFileName base == "index.html"
         then takeDirectory base
         else base
