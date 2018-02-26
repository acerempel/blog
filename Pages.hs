module Pages where

import Introit
import Text.Blaze.Html5 ( Html )

import qualified Templates
import Post
import Site


post :: Post -> SiteM Html
post thisPost@Post{title} =
   Templates.page (Just title) =<< Templates.post thisPost

home :: [Post] -> SiteM Html
home posts =
   Templates.page Nothing =<< foldrMapM Templates.post posts

archive :: [Post] -> SiteM Html
archive posts =
   Templates.page (Just "Archive") =<< foldrMapM Templates.archiveEntry posts
