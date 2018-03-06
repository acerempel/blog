{-# LANGUAGE ConstraintKinds #-}
module Site ( SiteT
            , SiteM
            , Configuration(..)
            , MonadSite
            , get
            , withConfig
            , copyrightNotice
            , constructTitle
            ) where

import Introit
import Control.Monad.Reader
import Data.Functor.Identity
import qualified Text
import Network.URI ( URI )
import Numeric.Natural ( Natural )

import qualified Targets
import qualified Things

type SiteT = ReaderT Configuration

type SiteM = SiteT Identity

data Configuration = Configuration
   { siteTitle :: Text
   , baseUrl :: URI
   , author :: Text
   , copyrightYear :: Natural
   , stylesheet :: Targets.This Things.Stylesheet
   , sourceUrl :: URI
   , includeDrafts :: Bool }

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
