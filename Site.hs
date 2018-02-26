module Site where

import Introit
import qualified Text
import Network.URI ( URI )
import Numeric.Natural ( Natural )

type SiteM result = Configuration -> result

data Configuration = Configuration
   { siteTitle :: Text
   , baseUrl :: URI
   , author :: Text
   , copyrightYear :: Natural
   , styleSheets :: [FilePath]
   , sourceUrl :: URI }

copyrightNotice :: SiteM Text
copyrightNotice Configuration{copyrightYear} =
   " © " <> Text.pack (show copyrightYear)

constructTitle :: Maybe Text -> SiteM Text
constructTitle titleMb Configuration{siteTitle} =
   maybe siteTitle (<> " | " <> siteTitle) titleMb
