{-# LANGUAGE ConstraintKinds #-}
module Site where

import Introit
import Control.Monad.Reader
import Data.Functor.Identity
import qualified Text
import Network.URI ( URI )
import Numeric.Natural ( Natural )

type SiteT = ReaderT Configuration

type SiteM = SiteT Identity

data Configuration = Configuration
   { siteTitle :: Text
   , baseUrl :: URI
   , author :: Text
   , copyrightYear :: Natural
   , styleSheets :: [FilePath]
   , sourceUrl :: URI }

type MonadSite m = MonadReader Configuration m

get :: MonadSite m => (Configuration -> a) -> m a
get = asks

withConfig :: SiteM a -> Configuration -> a
withConfig = runReader

copyrightNotice :: MonadSite m => m Text
copyrightNotice = get $ \Configuration{ author, copyrightYear } ->
   author <> " © " <> Text.pack (show copyrightYear)

constructTitle :: MonadSite m => Maybe Text -> m Text
constructTitle titleMb = get $ \Configuration{ siteTitle } ->
   maybe siteTitle (<> " | " <> siteTitle) titleMb
