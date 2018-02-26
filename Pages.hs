module Pages where

import Control.Monad ( (=<<), liftM2 )
import Data.Foldable ( foldrM )
import Data.Monoid ( (<>) )
import Text.Blaze.Html5 ( Html )

import qualified Templates
import Post
import Site


post :: Post -> SiteM Html
post thisPost@Post{title} =
   Templates.page title =<< Templates.post thisPost

home :: [Post] -> SiteM Html
home posts =
   Templates.page Nothing =<< foldrMapM Templates.post posts

archive :: [Post] -> SiteM Html
archive posts =
   Templates.page (Just "Archive") =<< foldrMapM Templates.archiveEntry posts

foldrMapM :: (Monoid z, Monad m) => (a -> m z) -> [a] -> m z
foldrMapM f =
   foldrM (\z1 z2 -> (<>) <$> f z1 <*> return z2) mempty
