module Write ( writeHtml ) where

import Data.ByteString.Builder ( hPutBuilder )
import Data.Functor.Identity ( runIdentity )
import Development.Shake
import Lucid ( execHtmlT )
import System.IO ( withBinaryFile, IOMode(..) )

import qualified Templates

writeHtml :: FilePath -> Templates.PageContent -> Action ()
writeHtml outPath content = liftIO do
  let html = runIdentity (execHtmlT (Templates.page False content))
  withBinaryFile outPath WriteMode \handle -> hPutBuilder handle html
