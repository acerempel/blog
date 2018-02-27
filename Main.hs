module Main where

import Introit
import Control.Exception ( throwIO )
import System.Console.GetOpt

import Development.Shake
import Development.Shake.Config

import Build


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
     extraOptions
     buildWithFlags

buildWithFlags :: [Flag] -> [String] -> IO (Maybe (Rules ()))
buildWithFlags userFlags extraArgs = return $ Just $ do
    unless (null extraArgs) $ liftIO $
        throwIO extraneousArgumentsError
    processFlags userFlags
    build
  where
    extraneousArgumentsError =
      userError $ "Found extra arguments, don't know what to do with them: " <> unwords extraArgs


data Flag
   = UseConfigFile FilePath

extraOptions :: [OptDescr (Either String Flag)]
extraOptions =
   [ Right <$> configFileOption ]

configFileOption :: OptDescr Flag
configFileOption =
   Option []
      ["config", "config-file"]
      (ReqArg UseConfigFile "FILE")
      "Get the site configuration from this file (relative to cwd)."

processFlags :: [Flag] -> Rules ()
processFlags cliFlags =
   for_ flags processFlag
 where
   flags =
      nubBy areSameFlags (cliFlags <> defaultFlags)

processFlag :: Flag -> Rules ()
processFlag (UseConfigFile file) =
   usingConfigFile file

defaultFlags :: [Flag]
defaultFlags =
   [ UseConfigFile "config.local" ]

areSameFlags :: Flag -> Flag -> Bool
areSameFlags (UseConfigFile _) (UseConfigFile _) = True
