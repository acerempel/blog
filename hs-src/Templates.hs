{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Templates ( Html
                 , archive, post, tagsList
                 , postLink
                 , page ) where

import Introit
import qualified Text

import Control.Monad ( when )
import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Lucid
import qualified Lucid.Base as Lucid
import qualified Text.MMark as MMark

import Post
import Routes ( Route, url )
import qualified Routes


type IncludeTags = Bool

archive :: [Post] -> Maybe Text -> IncludeTags -> Html ()
archive posts heading includeTags =
   div_ [ class_ "archive-listing" ] $ do
      maybe mempty (h1_ . toHtml) heading
      foldrMapM (archiveEntry includeTags) posts

post :: Post -> Maybe Link -> Maybe Link -> IncludeTags -> PageContent
post thePost@Post
      { content
      , composed
      , tags } mbPreceding mbFollowing includeTags =
    let
        mainContent =
            article_ $ do
                div_ [ class_ "post-meta" ] $ do
                    date composed
                h1_ $ link $ postLink thePost
                Lucid.relaxHtmlT $ MMark.render content
                when includeTags $
                    p_ [ class_ "tags" ] $
                        tagLinks tags
        footerContent = do
                maybe mempty (\preceding -> div_ [ class_ "footer-block" ] $ do
                            p_ [ class_ "post-meta" ] $ "Preceding post"
                            p_ (link preceding)) mbPreceding
                div_ [ class_ "footer-block" ] $
                    link Link{ linkRoute = Routes.Home
                             , linkAttributes = []
                             , linkText = "Home" }
                maybe mempty (\following -> div_ [ class_ "footer-block" ] $ do
                            p_ [ class_ "post-meta" ] $ "Following post"
                            p_ (link following)) mbFollowing
    in PageContent{ mainContent, footerContent }

tagsList :: [(Tag, Int)] -> Html ()
tagsList tagsWithCounts = do
    h1_ "Tags"
    div_ [ class_ "tags" ]$ ul_ $
      foldMap tagWithCount tagsWithCounts
  where
    tagWithCount :: (Tag, Int) -> Html ()
    tagWithCount (tag, count) =
      li_ $ do
        link $ tagLink tag
        small_ $ " (" <> toHtml (show count) <> " posts)"


archiveEntry :: IncludeTags -> Post -> Html ()
archiveEntry includeTags thePost@Post
               { synopsis
               , composed
               , tags } =
   div_ [ class_ "archive-entry" ] $ do
      div_ [ class_ "post-meta" ] $
         p_ $ date composed
      div_ $
         p_ $ link $ postLink thePost
      div_ [ class_ "synopsis" ] $
         p_ $ toHtml synopsis
      when includeTags $
         div_ [ class_ "tags" ] $
            p_ $ tagLinks tags

date :: Day -> Html ()
date theDate =
   time_
       [ datetime_ ((Text.pack . showGregorian) theDate) ]
       $ toHtml (formatTime defaultTimeLocale "%d %B %Y" theDate)

postLink :: Post -> Link
postLink Post{ title, slug } =
   Link{ linkText = toHtml title
       , linkAttributes = [ class_ "post-title" ]
       , linkRoute = Routes.Post slug }

tagLinks :: [Tag] -> Html ()
tagLinks [] = mempty
tagLinks theTags =
    small_ $
      "Tagged as " <> mconcat (intersperse ", " (map (link . tagLink) theTags)) 

tagLink :: Tag -> Link
tagLink tagName =
  Link{ linkText = toHtml tagName
      , linkAttributes = []
      , linkRoute = Routes.Tag tagName }

data PageContent = PageContent
    { mainContent :: Html ()
    , footerContent :: Html ()
    }

data Link = Link
    { linkRoute :: Route
    , linkAttributes :: [Attribute]
    , linkText :: Html () }

page :: Text -- ^ This page's title.
     -> PageContent
     -> Html ()
page pageTitle PageContent{mainContent, footerContent} = do
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
                [ charset_ "utf-8" ]
            title_ $ toHtml pageTitle
            link_
                [ rel_ "stylesheet"
                , href_ (url (Routes.Stylesheet "magenta")) ]
            link_
                [ rel_ "stylesheet"
                , href_ "https://fonts.googleapis.com/css?family=Lato:regular,bold,regularitalic|Crimson+Text:regular,regularitalic,bold" ]
        body_ $ do
            main_ $ do
                mainContent
            footer_ $ do
                footerContent

link :: Link -> Html ()
link Link{ linkText, linkAttributes, linkRoute } =
   a_ (href_ (url linkRoute) : linkAttributes)
      linkText
