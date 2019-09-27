module Templates ( Html
                 , archive, post, tagsList
                 , postLink
                 , page
                 , IncludeTags ) where

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

archive :: IncludeTags -> [Post] -> Html ()
archive includeTags posts =
   div_ [ class_ "archive-listing" ] do
      foldrMapM (archiveEntry includeTags) posts

post :: IncludeTags -> Post -> PageContent
post includeTags thePost@Post{ content, composed, tags } =
    let
        mainContent =
            article_ do
                p_ [ class_ "post-meta" ] (date composed)
                h1_ [ class_ "post-title" ] (link (postLink thePost))
                Lucid.relaxHtmlT $ MMark.render content
                when includeTags $ footer_ $
                    p_ [ class_ "tags" ] (tagLinks tags)
        footerContent = return ()
    in PageContent{ mainContent, footerContent }

tagsList :: [(Tag, Int)] -> Html ()
tagsList tagsWithCounts = do
    h1_ "Tags"
    p_ [ class_ "tags" ] $ ul_ $
      foldMap tagWithCount tagsWithCounts
  where
    tagWithCount :: (Tag, Int) -> Html ()
    tagWithCount (tag, count) =
      li_ $ p_ do
        link $ tagLink tag
        small_ $ " (" <> toHtml (show count) <> " posts)"


archiveEntry :: IncludeTags -> Post -> Html ()
archiveEntry includeTags thePost@Post{ synopsis, composed, tags } =
   section_ [ class_ "archive-entry" ] do
      p_ [ class_ "post-meta" ] (date composed)
      h2_ [ class_ "post-title" ] (link (postLink thePost))
      p_ [ class_ "synopsis" ] (toHtml synopsis)
      when includeTags $
         p_ [ class_ "tags" ] (tagLinks tags)

date :: Day -> Html ()
date theDate =
   time_
       [ datetime_ ((Text.pack . showGregorian) theDate) ]
       $ toHtml (formatTime defaultTimeLocale "%d %B %Y" theDate)

postLink :: Post -> Link 'Routes.Html
postLink Post{ title, slug } =
   Link{ linkText = toHtml title
       , linkAttributes = []
       , linkRoute = Routes.PageR slug }

tagLinks :: [Tag] -> Html ()
tagLinks [] = mempty
tagLinks theTags =
    small_ $
      "Tagged as " <> mconcat (intersperse ", " (map (link . tagLink) theTags)) 

tagLink :: Tag -> Link 'Routes.Html
tagLink tagName =
  Link{ linkText = toHtml tagName
      , linkAttributes = []
      , linkRoute = Routes.TagR tagName }

data PageContent = PageContent
    { mainContent :: Html ()
    , footerContent :: Html () }

data Link a = Link
    { linkRoute :: Route a
    , linkAttributes :: [Attribute]
    , linkText :: Html () }

page :: Text -- ^ This page's title.
     -> PageContent
     -> Html ()
page pageTitle PageContent{mainContent, footerContent} = do
    doctype_
    html_ [ lang_ "en" ] do
        head_ do
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
                , href_ (url (Routes.StylesheetR "magenta")) ]
            link_
                [ rel_ "stylesheet"
                , href_ "https://fonts.googleapis.com/css?family=Lato:regular,bold,regularitalic|Crimson+Text:regular,regularitalic,bold" ]
        body_ do
            main_ mainContent
            footer_ footerContent

link :: Link a -> Html ()
link Link{ linkText, linkAttributes, linkRoute } =
   a_ (href_ (url linkRoute) : linkAttributes) linkText
