{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Templates ( Template, Html
                 , getSiteConfig
                 , archive, post, tagsList
                 , page ) where

import Introit
import qualified Text

import Control.Monad ( when )
import qualified Control.Monad.Trans.Class as Trans
import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Development.Shake ( Action )
import Development.Shake.Config ( getConfig )
import Lucid
import qualified Lucid.Base as Lucid
import qualified Text.MMark as MMark

import Post
import Routes ( Route, url )
import qualified Routes
import Utilities


type Template a = HtmlT TemplateM a

newtype TemplateM a =
   TemplateM { runTemplateM :: Action a }
   deriving ( Functor, Applicative, Monad )

type IncludeTags = Bool

getSiteConfig :: String -> Template Text
getSiteConfig key = Trans.lift $ TemplateM $
      maybe
         (throwError ("No such key in config file: " <> key))
         (return . Text.pack)
      =<< getConfig key


archive :: [Post] -> Maybe Text -> IncludeTags -> Template ()
archive posts heading includeTags =
   div_ [ class_ "archive-listing" ] $ do
      maybe mempty (h1_ . toHtml) heading
      foldrMapM (archiveEntry includeTags) posts

post :: Post -> IncludeTags -> Template ()
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

tagsList :: [(Tag, Int)] -> Template ()
tagsList tagsWithCounts = do
    h1_ "Tags"
    div_ [ class_ "tags" ]$ ul_ $
      foldMap tagWithCount tagsWithCounts
  where
    tagWithCount :: (Tag, Int) -> Template ()
    tagWithCount (tag, count) =
      li_ $ do
        tagLink tag
        small_ $ " (" <> toHtml (show count) <> " posts)"


archiveEntry :: IncludeTags -> Post -> Template ()
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

date :: Day -> Template ()
date theDate =
   time_
       [ datetime_ ((Text.pack . showGregorian) theDate) ]
       $ toHtml (formatTime defaultTimeLocale "%d %B %Y" theDate)

postLink :: Post -> Template ()
postLink Post{ title, isDraft, slug } =
   link (toHtml theTitle) (Routes.Post slug)
  where
   theTitle :: Text
   theTitle =
      if isDraft then "[DRAFT] " <> title else title

tagLinks :: [Tag] -> Template ()
tagLinks [] = mempty
tagLinks theTags =
    small_ $
      "Tagged as " <> mconcat (intersperse ", " (map tagLink theTags)) 

tagLink :: Tag -> Template ()
tagLink tagName =
  link (toHtml tagName) (Routes.Tag tagName)

page :: Text -- ^ This page's title.
     -> Template () -- ^ This page's content.
     -> Action (Html ())
page pageTitle content = runTemplateM $ Lucid.commuteHtmlT $ do
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
            header_ $ do
                h1_ $ a_ [ href_ "/" ] "three dots …"
            main_ $ do
                content
            footer_ $ do
                toHtml $ ("Composed with love in Halifax, Nova Scotia" :: Text)

link :: Template () -> Route -> Template ()
link linkText route =
   a_ [ href_ (url route) ]
      linkText
