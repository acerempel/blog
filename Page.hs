{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns
           , FlexibleInstances
           , FunctionalDependencies #-}
module Page where

import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Time.Format
import Text.Blaze.Html5 ( (!), Html, toValue )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Site
import Utils

newtype WhichPost = ThisPost PostID

data Home = Home

data Archive = Archive

instance Page Home [(WhichPost, Post)] where
    template Home = home
    url Home = "/"
    title Home _ = siteTitle

instance Page Archive [(WhichPost, Post)] where
    template Archive = archive
    url Archive = "/archive"
    title Archive _ = "Archive | " <> siteTitle

data Post = Post
    { content :: Html
    , date :: Day
    , postTitle :: Text
    , isDraft :: Bool
    }

type PostID = Text

class Page route content | route -> content where
    template :: route -> content -> Html
    url :: route -> Text
    title :: route -> content -> Text

instance Page WhichPost Post where
    template =
        post
    url (ThisPost id) =
        "/posts/" <> id
    title _ Post{postTitle} =
        postTitle

post :: WhichPost -> Post -> Html
post route Post{content, date, postTitle} =
    H.article $ do
        H.div
            ! A.class_ "info"
            $ H.time
                ! A.datetime ((toValue . showGregorian) date)
                $ H.toHtml (formatTime defaultTimeLocale "%d %B %Y" date)
        H.h1 $ H.a
            ! A.href (toValue $ url route)
            $ H.toHtml postTitle
        content

home :: [(WhichPost, Post)] -> Html
home posts =
    foldMap (uncurry post) posts

archive :: [(WhichPost, Post)] -> Html
archive items =
    foldMap archiveEntry items
  where
    archiveEntry (postRoute, Post{date, postTitle}) =
        H.div ! A.class_ "entry" $ do
            H.time
                ! A.class_ "info"
                ! A.datetime ((toValue . showGregorian) date)
                $ H.toHtml (showGregorian date)
            H.h1 $
                H.a ! A.href ((toValue . url) postRoute)
                    $ H.toHtml postTitle


page :: Page page content => page -> content -> Html
page here content =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta
                ! A.httpEquiv "ContentType"
                ! A.content "text/html; charset=utf-8"
            H.meta
                ! A.charset "UTF-8"
            H.title
                $ H.toHtml (title here content)
            H.link
                ! A.rel "stylesheet"
                ! A.type_ "text/css"
                ! A.href "/styles/magenta.css"
        H.body $ do
            H.header $ do
                H.div
                    ! A.id "logo"
                    $ H.a
                        ! A.href "/"
                        $ H.toHtml siteTitle
                H.nav $ do
                    H.a ! A.href ((toValue . url) Home)
                        $ "Recent"
                    H.a ! A.href ((toValue . url) Archive)
                        $ "Archive"
            H.main $
                template here content
            H.footer $ do
                H.toHtml copyrightNotice
                H.a ! A.href (toValue sourceUrl)
                    $ "Source"
            H.script
                ! A.id "__bs_script__"
                $ H.preEscapedText bsInjectionScript
  where
    bsInjectionScript =
        Text.unlines
            [ "//<![CDATA["
            , "    document.write(\"<script async src='http://HOST:8001/browser-sync/browser-sync-client.js?v=2.18.13'><\\/script>\".replace(\"HOST\", location.hostname));"
            , "//]]>"
            ]
