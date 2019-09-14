module Utilities ( throwError, throwFileError ) where

import Introit
import Control.Exception ( throwIO )


throwError :: FilePath -> IO a
throwError problem = throwIO $ userError problem

throwFileError :: FilePath -> String -> IO a
throwFileError file problem = throwError $
   "Error in " <> file <> ", namely: " <> problem
