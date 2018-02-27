module Main where

import Introit
import Control.Exception ( throwIO )
import System.Console.GetOpt

import Development.Shake

import Build
import Flags


main :: IO ()
main = do
  -- Let Shake's version stamp depend on all our Haskell source files, so
  -- that if any of them change, everything gets rebuilt.
  hsSourceFiles <- getDirectoryFilesIO "" ["*.hs"]
  shakeVersion <- getHashedShakeVersion hsSourceFiles
  shakeArgsWith
     shakeOptions{ shakeVersion
                 , shakeThreads = 3
                 , shakeColor = True }
     Flags.options
     buildWithFlags

buildWithFlags :: [Flag] -> [String] -> IO (Maybe (Rules ()))
buildWithFlags flags extraArgs = return $ Just $ do
    -- We don't accept extra non-flag arguments.
    unless (null extraArgs) $ liftIO $
        throwIO extraneousArgumentsError
    let configuration = Flags.handle flags
    build configuration
  where
    extraneousArgumentsError =
      userError $ "Found extra arguments, don't know what to do with them: " <> unwords extraArgs
