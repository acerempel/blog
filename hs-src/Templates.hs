{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Templates ( Html
                 , archive, post, tagsList
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

post :: Post -> IncludeTags -> Html ()
post thePost@Post
      { content
      , composed
      , tags } includeTags =
    article_ $ do
        div_ [ class_ "date" ] $ do
          date composed
        h1_ $ postLink thePost
        Lucid.relaxHtmlT $ MMark.render content
        when includeTags $
           p_ [ class_ "tags" ] $
              tagLinks tags

tagsList :: [(Tag, Int)] -> Html ()
tagsList tagsWithCounts = do
    h1_ "Tags"
    div_ [ class_ "tags" ]$ ul_ $
      foldMap tagWithCount tagsWithCounts
  where
    tagWithCount :: (Tag, Int) -> Html ()
    tagWithCount (tag, count) =
      li_ $ do
        tagLink tag
        small_ $ " (" <> toHtml (show count) <> " posts)"


archiveEntry :: IncludeTags -> Post -> Html ()
archiveEntry includeTags thePost@Post
               { synopsis
               , composed
               , tags } =
   div_ [ class_ "archive-entry" ] $ do
      div_ [ class_ "date" ] $
         date composed
      div_ [ class_ "title" ] $
         h2_ $ postLink thePost
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

postLink :: Post -> Html ()
postLink Post{ title, isDraft, slug } =
   link (toHtml theTitle) (Routes.Post slug)
  where
   theTitle :: Text
   theTitle =
      if isDraft then "[DRAFT] " <> title else title

tagLinks :: [Tag] -> Html ()
tagLinks [] = mempty
tagLinks theTags =
    small_ $
      "Tagged as " <> mconcat (intersperse ", " (map tagLink theTags)) 

tagLink :: Tag -> Html ()
tagLink tagName =
  link (toHtml tagName) (Routes.Tag tagName)

page :: Text -- ^ This page's title.
     -> Html () -- ^ This page's content.
     -> Html ()
page pageTitle content = do
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
                content

link :: Html () -> Route -> Html ()
link linkText route =
   a_ [ href_ (url route) ]
      linkText
