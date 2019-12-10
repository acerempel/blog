module Write ( writeHtml ) where

import Control.Exception ( throwIO )
import Data.ByteString.Builder ( hPutBuilder )
import Development.Shake
import Lucid ( execHtmlT )
import System.IO ( withBinaryFile, IOMode(..) )

import qualified Templates

writeHtml :: FilePath -> Templates.PageContent -> Action ()
writeHtml outPath content = do
  let htmlOrProblem = execHtmlT (Templates.page False content)
  case htmlOrProblem of
    Left problem ->
      liftIO $ throwIO problem
    Right html -> do
      putLoud $ "Writing HTML to " <> outPath
      liftIO $ withBinaryFile outPath WriteMode \handle -> hPutBuilder handle html
