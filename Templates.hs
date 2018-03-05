module Templates ( post, archiveEntry, page ) where

import Introit
import qualified Text

import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Lucid
import Lucid.Base ( relaxHtmlT )
import Text.MMark ( render )
import Network.URI

import Things
import Site
import qualified Targets


post :: Post -> HtmlT SiteM ()
post thePost@Post
      { content
      , composed } =
    article_ $ do
        div_ [ class_ "info" ] $ date composed
        h1_ $ postLink thePost
        relaxHtmlT $ render content

archiveEntry :: Post -> HtmlT SiteM ()
archiveEntry thePost@Post
               { synopsis
               , composed } =
   div_ [ class_ "archive-entry" ] $ do
      div_ [ class_ "date" ] $
         date composed
      div_ [ class_ "title" ] $
         h2_ $ postLink thePost
      div_ [ class_ "synopsis" ] $
         p_ $ toHtml synopsis


date :: Day -> HtmlT SiteM ()
date theDate =
   time_
       [ datetime_ ((Text.pack . showGregorian) theDate) ]
       $ toHtml (formatTime defaultTimeLocale "%d %B %Y" theDate)

postLink :: Post -> HtmlT SiteM ()
postLink thePost@Post{ title, isDraft, slug } = do
   let theUrl = Targets.url (Targets.Post (Text.unpack slug))
   a_
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
    stylesh <- get stylesheet

    doctype_
    html_ [ lang_ "en" ] $ do
        head_ $ do
            meta_
                [ httpEquiv_ "ContentType"
                , content_ "text/html; charset=utf-8" ]
            meta_
                [ name_ "viewport"
                , content_ "width=device-width, initial-scale=1" ]
            meta_
                [ charset_ "UTF-8" ]
            title_
                $ toHtml =<< constructTitle thisTitleMb
            link_
                [ rel_ "canonical"
                , href_ ((Text.pack . show) baseURL) ]
            link_
                [ rel_ "stylesheet"
                , type_ "text/css"
                , href_ (Text.pack stylesh) ]
        body_ $ do
            header_ $ do
                div_
                    [ id_ "logo" ]
                    $ link (toHtml titleOfSite) $ Targets.url Targets.Home
                nav_ $ do
                    link "Recent" $ Targets.url Targets.Home
                    link "Archive" $ Targets.url Targets.Archive
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
