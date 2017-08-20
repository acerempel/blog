{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns
           , FlexibleInstances
           , FunctionalDependencies #-}
module Page where

import Data.Text ( Text )
import Data.Time.Calendar ( Day )
import Data.Time.Format
import Text.Blaze.Html5 ( (!), Html, toHtml, toValue )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Site
import Utils

data WhichPost = ThisPost PostID

data Home = Home

data Archive = Archive

instance Page Home [(WhichPost, Post)] where
    template Home = home
    url Home = "/"
    title Home = Just "Home"

instance Page Archive [(WhichPost, Post)] where
    template Archive = archive
    url Archive = "/archive"
    title Archive = Just "Archive"

data Post = Post
    { content :: Html
    , date :: Day
    , identifier :: PostID
    , postTitle :: Maybe Text
    , isDraft :: Bool
    }

type PostID = Text

class Page page content | page -> content where
    template :: page -> content -> Html
    url :: page -> Text
    title :: page -> Maybe Text

instance Page WhichPost Post where
    template =
        post
    url (ThisPost id) =
        "/posts/" <> id
    title (ThisPost id) =
        Just id

post :: WhichPost -> Post -> Html
post postPage Post{content, date, postTitle} =
    H.article $ do
        H.div
            ! A.class_ "info"
            $ H.time
                ! A.datetime ((toValue . showGregorian) date)
                $ (toHtml . formatTime defaultTimeLocale "%d %B %Y") date
        guardJust postTitle $ \t ->
            H.h1 $ H.a
                ! A.href (toValue $ url postPage)
                $ toHtml t
        content

home :: [(WhichPost, Post)] -> Html
home posts =
    foldMap recentItem posts
  where
    recentItem (postPage, postContent) =
        post postPage postContent

archive :: [(WhichPost, Post)] -> Html
archive items =
    foldMap archiveEntry items
  where
    archiveEntry (postPage@(ThisPost postID), postContent) =
        H.div ! A.class_ "entry" $ do
            H.time
                ! A.class_ "info"
                ! A.datetime ((toValue . showGregorian) (date postContent))
                $ (toHtml . showGregorian) (date postContent)
            H.h1
                $ H.a ! A.href ((toValue . url) postPage)
                    $ toHtml postID


page :: Page page content => page -> content -> Html
page here content =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta
                ! A.httpEquiv "ContentType"
                ! A.content "text/html"
            H.meta
                ! A.charset "UTF-8"
            H.title
                $ toHtml $ fromMaybe siteTitle (title here)
            H.link
                ! A.rel "stylesheet"
                ! A.type_ "text/css"
                ! A.href "/styles/magenta.css"
        H.body $ do
            H.header $ do
                H.div
                    ! A.id "logo"
                    $ H.a ! A.href "/"
                        $ toHtml siteTitle
                H.nav $ do
                    H.a ! A.href ((toValue . url) Home)
                      $ "Recent"
                    H.a ! A.href ((toValue . url) Archive)
                      $ "Archive"
            H.main $
                template here content
            H.footer $ do
                toHtml copyrightNotice
                H.a ! A.href (toValue sourceUrl)
                  $ "Source"
