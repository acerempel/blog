{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns   #-}
module Templates.Post where

import Data.Time.Calendar ( showGregorian )
import Data.Time.Format
import Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes ( class_, datetime, href )

import Page.Meta as Page
import Types
import Utils

post :: Page.Meta -> Post -> Html
post Page.M{url} P{content, date, postTitle} =
    article $ do
        Html.div
            ! class_ "info"
            $ time
                ! datetime ((toValue . showGregorian) date)
                $ (toHtml . formatTime defaultTimeLocale "%d %B %Y") date
        guardJust postTitle $ \t ->
            h1 $ a
                ! href (toValue url)
                $ toHtml t
        content
