module Render ( post, home, archive ) where

import Introit

import Lucid
import qualified Data.Text.Lazy as Text

import qualified Templates
import Things
import Site


post :: Post -> SiteM Text
post thisPost@Post{title} =
   render (Just title) (Templates.post thisPost)

home :: [Post] -> SiteM Text
home posts =
   render Nothing (foldrMapM Templates.post posts)

archive :: [Post] -> SiteM Text
archive posts =
   render
      (Just "Archive")
      (div_ [ class_ "archive-listing" ] $
         foldrMapM Templates.archiveEntry posts)

render :: Maybe Text -> HtmlT SiteM () -> SiteM Text
render titleMb html =
   -- TODO: Possibly more suitable to use lazy text in any case?
   Text.toStrict <$> renderTextT (Templates.page titleMb html)


