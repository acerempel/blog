module Post ( Post(..) ) where

import Text.MMark ( MMark )
import Data.Text ( Text )
import Data.Time.Calendar ( Day )

data Post = Post
   { slug :: Text -- ^ Identifier to use for the slug in the url.
   , title :: Text -- ^ Title.
   , content :: MMark -- ^ The post body.
   , synopsis :: Text -- ^ A little description or summary or teaser.
   , composed :: Day -- ^ Date of composition.
   , published :: Day -- ^ Date of publication.
   , isDraft :: Bool -- ^ Whether this post is a draft or is published.
   }
