module Write ( writeHtml ) where

import Control.Exception ( throwIO )
import Data.ByteString.Builder ( hPutBuilder )
import Development.Shake
import Lucid ( execHtmlT )
import System.IO ( withBinaryFile, IOMode(..) )

import qualified Templates

writeHtml :: FilePath -> Templates.PageContent -> Action ()
writeHtml outPath content = liftIO do
  let htmlOrProblem = execHtmlT (Templates.page False content)
  case htmlOrProblem of
    Left problem ->
      throwIO problem
    Right html ->
      withBinaryFile outPath WriteMode \handle -> hPutBuilder handle html
