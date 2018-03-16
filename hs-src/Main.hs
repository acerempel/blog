module Main where

import Development.Shake

import Build
import Flags


main :: IO ()
main = do
  -- Let Shake's version stamp depend on all our Haskell source files, so
  -- that if any of them change, everything gets rebuilt.
  hsSourceFiles <- getDirectoryFilesIO "" ["hs-src/*.hs"]
  shakeVersion <- getHashedShakeVersion hsSourceFiles
  shakeArgsWith
     shakeOptions{ shakeVersion
                 , shakeThreads = 3
                 , shakeColor = True }
     Flags.options
     buildWithFlags

buildWithFlags :: [Flag] -> [String] -> IO (Maybe (Rules ()))
buildWithFlags flags targets = return $ Just $ do
    let configuration = Flags.handle flags
    build configuration targets
