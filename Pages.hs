module Pages where

import Introit

import Lucid
import qualified Data.Text.Lazy as Text

import qualified Templates
import Post
import Site


post :: Post -> SiteM Text
post thisPost@Post{title} =
   renderPage (Just title) (Templates.post thisPost)

home :: [Post] -> SiteM Text
home posts =
   renderPage Nothing (foldrMapM Templates.post posts)

archive :: [Post] -> SiteM Text
archive posts =
   renderPage (Just "Archive") (foldrMapM Templates.archiveEntry posts)

renderPage :: Maybe Text -> HtmlT SiteM () -> SiteM Text
renderPage titleMb html =
   -- TODO: Possibly more suitable to use lazy text in any case?
   Text.toStrict <$> renderTextT (Templates.page titleMb html)
