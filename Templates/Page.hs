{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns    #-}
module Templates.Page where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 hiding ( meta )
import Text.Blaze.Html5.Attributes as A

import Page
import Page.Meta
import Site
import Types
import Utils

page :: Page -> Html
page here =
    docTypeHtml
        ! lang "en"
        $ do
            H.head $ do
                H.meta
                    ! httpEquiv "ContentType"
                    ! A.content "text/html"
                H.meta
                    ! charset "UTF-8"
                H.title
                    $ (toHtml.(fromMaybe siteTitle).pageTitle.meta) here
                link
                    ! rel "stylesheet"
                    ! type_ "text/css"
                    ! href "/styles/magenta.css"
            body $ do
                header $ do
                    H.div
                        ! A.id "logo"
                        $ a ! href "/"
                            $ toHtml siteTitle
                    nav $ do
                        a ! href ((toValue.url.meta) (Home []))
                          $ (toHtml.linkTitle.meta) (Home [])
                        a ! href ((toValue.url.meta) (Archive []))
                          $ (toHtml.linkTitle.meta) (Archive [])
                main $ do
                    pageToHtml here
                footer $ do
                    toHtml copyrightNotice
                    a ! href (toValue sourceUrl)
                      $ "Source"
