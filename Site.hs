module Site where

import Introit
import qualified Text
import Network.URI ( URI )
import Numeric.Natural ( Natural )

type SiteM result = Configuration -> result

data Configuration = Configuration
   { siteTitle :: Text
   , baseUrl :: URI
   , copyrightYear :: Natural
   , styleSheet :: FilePath
   , sourceUrl :: URI }

copyrightNotice :: SiteM Text
copyrightNotice Configuration{copyrightYear} =
   "Alan Rempel © " <> Text.pack (show copyrightYear)

constructTitle :: Maybe Text -> SiteM Text
constructTitle titleMb Configuration{siteTitle} =
   maybe siteTitle (<> " | " <> siteTitle) titleMb
