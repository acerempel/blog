{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns    #-}
module Templates.Archive where

import Text.Blaze.Html5 as Html hiding ( meta )
import Text.Blaze.Html5.Attributes ( class_, datetime, href )

import Page.Meta as Page
import Types
import Utils

archive :: Page.Meta -> [Post] -> Html
archive meta posts =
    foldMap archiveEntry posts
  where
    archiveEntry post@P{date} =
        Html.div ! class_ "entry" $ do
            time
                ! class_ "info"
                ! datetime ((toValue . showGregorian) date)
                $ (toHtml . showGregorian) date
            h1
                $ a ! href ((toValue.url.Page.meta.Post) post)
                    $ (toHtml.linkTitle.Page.meta.Post) post
