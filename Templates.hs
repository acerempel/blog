module Templates where

import Introit
import qualified Text

import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Network.URI
import Text.Blaze.Html5 ( (!), Html, toValue )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Post
import Site
import qualified Routes
import Routes ( url, homeUrl, archiveUrl )


post :: Post -> SiteM Html
post thePost@Post
      { content
      , composed }
     configuration =
    H.article $ do
        displayDate composed
        displayPostHeading thePost configuration
        H.toHtml content

archiveEntry :: Post -> SiteM Html
archiveEntry thePost@Post
               { synopsis
               , composed }
             configuration =
   H.div ! A.class_ "entry" $ do
      displayDate composed
      displayPostHeading thePost configuration
      H.toHtml synopsis


displayDate :: Day -> Html
displayDate date =
   H.div
       ! A.class_ "info"
       $ H.time
           ! A.datetime ((toValue . showGregorian) date)
           $ H.toHtml (formatTime defaultTimeLocale "%d %B %Y" date)

displayPostHeading :: Post -> SiteM Html
displayPostHeading Post{ title, slug } configuration =
   H.h1 $ H.a
       ! A.href ((toValue . show) theUrl)
       $ H.toHtml title
  where
   theUrl = url (Routes.Post slug) configuration


page :: Maybe Text -- ^ This page's title.
     -> Html -- ^ This page's content.
     -> SiteM Html
page thisTitleMb content
     conf@Configuration{styleSheets, siteTitle} =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta
                ! A.httpEquiv "ContentType"
                ! A.content "text/html; charset=utf-8"
            H.meta
                ! A.charset "UTF-8"
            H.title
                $ H.toHtml (constructTitle thisTitleMb conf)
            (flip foldMap) styleSheets $ \ss ->
              H.link
                ! A.rel "stylesheet"
                ! A.type_ "text/css"
                ! A.href ((toValue . show) (url (Routes.Stylesheet ss) conf))
        H.body $ do
            H.header $ do
                H.div
                    ! A.id "logo"
                    $ link homeUrl (H.toHtml siteTitle) conf
                H.nav $ ($ conf) $ liftA2 (<>)
                    (link homeUrl "Recent")
                    (link archiveUrl "Archive")
            H.main $
                content
            H.footer $ ($ conf) $ liftA2 (<>)
                (H.toHtml . copyrightNotice)
                (link sourceUrl "Source")
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

link :: SiteM URI -> Html -> SiteM Html
link getUrl linkText conf =
   H.a ! A.href ((toValue . show) (getUrl conf))
       $ linkText
