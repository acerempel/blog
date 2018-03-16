{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Templates ( URL, URLPattern
                 , Template, render
                 , liftAction, getThisURL, getSiteConfig
                 , home, archive, post
                 , page ) where

import Introit
import qualified Text

import Control.Monad.Reader
import Data.ByteString.Builder ( Builder )
import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Development.Shake ( Action )
import Development.Shake.Config ( getConfig )
import Lucid
import Lucid.Base ( relaxHtmlT )
import qualified Text.MMark as MMark

import Post
import Utilities


type URL = Text
type URLPattern = URL

type Template a = HtmlT TemplateM a

newtype TemplateM a =
   TemplateM { runTemplateM :: ReaderT URL Action a }
   deriving ( Functor, Applicative, Monad )

render :: Template a -> URL -> Action Builder
render template thisURL =
   let action = runTemplateM $ execHtmlT template
   in runReaderT action thisURL

getThisURL :: Template URL
getThisURL = lift $ TemplateM ask

liftAction :: Action a -> Template a
liftAction = lift . TemplateM . lift

getSiteConfig :: String -> Template Text
getSiteConfig key = liftAction $
      maybe
         (throwError ("No such key in config file: " <> key))
         (return . Text.pack)
      =<< getConfig key


home :: [Post] -> Template ()
home posts =
   foldrMapM post posts

archive :: [Post] -> Template ()
archive posts =
   div_ [ class_ "archive-listing" ] $
      foldrMapM archiveEntry posts

post :: Post -> Template ()
post thePost@Post
      { content
      , composed } =
    article_ $ do
        div_ [ class_ "info" ] $ date composed
        h1_ $ postLink thePost
        relaxHtmlT $ MMark.render content


archiveEntry :: Post -> Template ()
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

date :: Day -> Template ()
date theDate =
   time_
       [ datetime_ ((Text.pack . showGregorian) theDate) ]
       $ toHtml (formatTime defaultTimeLocale "%d %B %Y" theDate)

postLink :: Post -> Template ()
postLink Post{ title, isDraft, slug } =
   a_ [ href_ ("/posts/" <> slug) ]
      $ toHtml theTitle
  where
   theTitle :: Text
   theTitle =
      if isDraft then "[DRAFT] " <> title else title

page :: Maybe Text -- ^ This page's title.
     -> Template () -- ^ This page's content.
     -> Template ()
page thisTitleMb content = do
    baseURL <- getSiteConfig "base_url"
    titleOfSite <- getSiteConfig "site_title"
    useBrowserSync <- getSiteConfig "browsersync"

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
            title_ $ toHtml $
                maybe titleOfSite (<> " | " <> titleOfSite) thisTitleMb
            link_
                [ rel_ "canonical"
                , href_ baseURL ]
            link_
                [ rel_ "stylesheet"
                , type_ "text/css"
                , href_ "/styles/magenta.css" ]
        body_ $ do
            header_ $ do
                div_
                    [ id_ "logo" ] $
                    link (toHtml titleOfSite) "/"
                nav_ $ do
                    link "Recent" "/"
                    link "Archive" "/archive"
            main_
                content
            footer_ $ do
                author <- getSiteConfig "author"
                toHtml $ author <> " © 2018"
            when (useBrowserSync == "true") $
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

link :: Template () -> URL -> Template ()
link linkText url =
   a_
      [ href_ url ]
      linkText
