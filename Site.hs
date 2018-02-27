module Site where

import Introit
import Data.Functor.Identity
import qualified Text
import Network.URI ( URI )
import Numeric.Natural ( Natural )

import Control.Monad.Trans.Reader

type SiteT = ReaderT Configuration

type SiteM = SiteT Identity

data Configuration = Configuration
   { siteTitle :: Text
   , baseUrl :: URI
   , author :: Text
   , copyrightYear :: Natural
   , styleSheets :: [FilePath]
   , sourceUrl :: URI }

withConfig :: (Configuration -> result) -> SiteM result
withConfig = reader

withConfigM :: Monad m => (Configuration -> m result) -> SiteT m result
withConfigM = ReaderT

copyrightNotice :: SiteM Text
copyrightNotice = withConfig $ \Configuration{ author, copyrightYear } ->
   author <> " © " <> Text.pack (show copyrightYear)

constructTitle :: Maybe Text -> SiteM Text
constructTitle titleMb = withConfig $ \Configuration{ siteTitle } ->
   maybe siteTitle (<> " | " <> siteTitle) titleMb
