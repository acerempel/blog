module Post where

import Cheapskate ( Doc )
import Data.Text ( Text )
import Data.Time.Calendar ( Day )

data Post = Post
   { slug :: Text -- ^ Identifier to use for the slug in the url.
   , title :: Text -- ^ Title.
   , content :: Cheapskate.Doc -- ^ The post body.
   , synopsis :: Cheapskate.Doc -- ^ A little description or summary or teaser.
   , composed :: Day -- ^ Date of composition.
   , published :: Day -- ^ Date of publication.
   }
