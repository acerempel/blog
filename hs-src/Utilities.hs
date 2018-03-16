module Utilities ( throwError, throwFileError ) where

import Introit
import Control.Exception ( throwIO )
import Development.Shake


throwError :: FilePath -> Action a
throwError problem = liftIO $ throwIO $ userError problem

throwFileError :: FilePath -> String -> Action a
throwFileError file problem = throwError $
   "Error in " <> file <> ", namely: " <> problem
