module Templates.Home where

import Text.Blaze.Html ( Html )

import Page.Meta as Page
import Templates.Post
import Types

home :: Page.Meta -> [Post] -> Html
home _ posts =
    foldMap recentItem posts
  where
    recentItem p =
        post ((meta.Post) p) p
