module Templates where

import qualified Cheapskate
import Data.Maybe ( fromJust )
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Network.URI
import qualified Text.Blaze.Html as Blaze
import Text.Blaze.Html5 ( (!), Html, toValue )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Post
import Site
import URLs


post :: Post -> SiteM Html
post Post{ title = titleMb
         , content
         , composed = date
         , slug }
     configuration =
    H.article $ do
        displayDate date
        displayPostHeading titleMb slug configuration
        H.toHtml content

archiveEntry :: Post -> SiteM Html
archiveEntry Post{ title = titleMb
                 , synopsis
                 , composed = date
                 , slug }
             configuration =
   H.div ! A.class_ "entry" $ do
      displayDate date
      displayPostHeading titleMb slug configuration
      H.toHtml synopsis


displayDate :: Day -> Html
displayDate date =
   H.div
       ! A.class_ "info"
       $ H.time
           ! A.datetime ((toValue . showGregorian) date)
           $ H.toHtml (formatTime defaultTimeLocale "%d %B %Y" date)

displayPostHeading :: Maybe Text
                   -> Text
                   -> SiteM Html
displayPostHeading titleMb slug configuration =
   H.h1 $ H.a
       ! A.href ((toValue . show) $ url slug configuration)
       $ H.toHtml (fromJust titleMb) -- TODO: What if there is no title?


page :: Maybe Text -- ^ This page's title.
     -> Html -- ^ This page's content.
     -> SiteM Html
page thisTitleMb content
     conf@Configuration{styleSheet, siteTitle} =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta
                ! A.httpEquiv "ContentType"
                ! A.content "text/html; charset=utf-8"
            H.meta
                ! A.charset "UTF-8"
            H.title
                $ H.toHtml (constructTitle thisTitleMb conf)
            H.link
                ! A.rel "stylesheet"
                ! A.type_ "text/css"
                ! A.href (toValue styleSheet)
        H.body $ do
            H.header $ do
                H.div
                    ! A.id "logo"
                    $ link homeUrl (H.toHtml siteTitle) conf
                H.nav $ ($ conf) $ do
                    link homeUrl "Recent"
                    link archiveUrl "Archive"
            H.main $
                content
            H.footer $ ($ conf) $ do
                H.toHtml . copyrightNotice
                link sourceUrl "Source"
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
