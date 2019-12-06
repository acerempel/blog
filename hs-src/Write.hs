module Write ( writeHtml ) where

import Data.ByteString.Builder ( hPutBuilder )
import Data.Functor.Identity ( runIdentity )
import Development.Shake
import Development.Shake.FilePath
import Lucid ( execHtmlT )
import System.Directory ( createDirectoryIfMissing )
import System.IO ( withBinaryFile, IOMode(..) )

import qualified Templates

writeHtml :: FilePath -> Templates.PageContent -> Action ()
writeHtml outPath content = liftIO do
  createDirectoryIfMissing True (takeDirectory outPath)
  let html = runIdentity (execHtmlT (Templates.page False content))
  withBinaryFile outPath WriteMode \handle -> hPutBuilder handle html
