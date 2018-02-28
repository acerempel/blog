module Templates where

import Introit
import qualified Text

import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Lucid
import Lucid.Base ( relaxHtmlT )
import Text.MMark ( render )
import Network.URI

import Post
import Site
import Routes


post :: Post -> HtmlT SiteM ()
post thePost@Post
      { content
      , composed } =
    article_ $ do
        displayDate composed
        displayPostHeading thePost
        relaxHtmlT $ render content

archiveEntry :: Post -> HtmlT SiteM ()
archiveEntry thePost@Post
               { synopsis
               , composed } =
   div_ [ class_ "entry" ] $ do
      displayDate composed
      displayPostHeading thePost
      toHtml synopsis


displayDate :: Day -> HtmlT SiteM ()
displayDate date =
   div_
       [ class_ "info" ]
       $ time_
           [ datetime_ ((Text.pack . showGregorian) date) ]
           $ toHtml (formatTime defaultTimeLocale "%d %B %Y" date)

displayPostHeading :: Post -> HtmlT SiteM ()
displayPostHeading thePost@Post{ title, isDraft } = do
   theUrl <- urlForPost thePost
   h1_ $ a_
       [ href_ ((Text.pack . show) theUrl) ]
       $ toHtml theTitle
  where
   theTitle :: Text
   theTitle =
      if isDraft then "[DRAFT] " <> title else title


page :: Maybe Text -- ^ This page's title.
     -> HtmlT SiteM () -- ^ This page's content.
     -> HtmlT SiteM ()
page thisTitleMb content = do
    baseURL <- get baseUrl
    titleOfSite <- get siteTitle
    stylesheetUrls <- traverse urlForStylesheet =<< get styleSheets

    doctype_
    html_ [ lang_ "en" ] $ do
        head_ $ do
            meta_
                [ httpEquiv_ "ContentType"
                , content_ "text/html; charset=utf-8" ]
            meta_
                [ charset_ "UTF-8" ]
            title_
                $ toHtml =<< constructTitle thisTitleMb
            link_
                [ rel_ "canonical"
                , href_ ((Text.pack . show) baseURL) ]
            (flip foldMap) stylesheetUrls $ \ssu ->
              link_
                [ rel_ "stylesheet"
                , type_ "text/css"
                , href_ ((Text.pack . show) ssu) ]
        body_ $ do
            header_ $ do
                div_
                    [ id_ "logo" ]
                    $ link (toHtml titleOfSite) =<< homeUrl
                nav_ $ do
                    link "Recent" =<< homeUrl
                    link "Archive" =<< archiveUrl
            main_
                content
            footer_ $ do
                toHtml =<< copyrightNotice
                link "Source" =<< get sourceUrl
            script_
                [ id_ "__bs_script__" ]
                bsInjectionScript
  where
    bsInjectionScript =
        Text.unlines
            [ "//<![CDATA["
            , "    document.write(\"<script async src='http://HOST:8001/browser-sync/browser-sync-client.js?v=2.18.13'><\\/script>\".replace(\"HOST\", location.hostname));"
            , "//]]>"
            ]

link :: HtmlT SiteM () -> URI -> HtmlT SiteM ()
link linkText url =
   a_
      [ href_ ((Text.pack . show) url ) ]
      linkText
