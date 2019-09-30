module Templates ( Html
                 , archive, post, tagsList
                 , page
                 , IncludeTags ) where

import Introit
import qualified Text

import Control.Monad ( when )
import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Lucid
import qualified Text.MMark as MMark

import Post
import Routes ( Route, url )
import qualified Routes


type IncludeTags = Bool

archive :: IncludeTags -> [Post] -> PageContent
archive includeTags posts =
  let mainContent =
        foldrMapM (archiveEntry includeTags) posts
  in PageContent{mainContent, pageTitle = "All Posts"}

post :: IncludeTags -> Post -> PageContent
post includeTags Post{ content, composed, tags, title, slug } =
    let
        mainContent =
            article_ do
                header_ do
                  date composed
                  h1_ $ a_ [ href_ (url (Routes.PageR slug)) ] (toHtml title)
                MMark.render content
                when includeTags $ footer_ $
                    p_ (tagLinks tags)
    in PageContent{ mainContent, pageTitle = title }

tagsList :: [(Tag, Int)] -> Html ()
tagsList tagsWithCounts = do
    h1_ "Tags"
    p_ $ ul_ $
      foldMap tagWithCount tagsWithCounts
  where
    tagWithCount :: (Tag, Int) -> Html ()
    tagWithCount (tag, count) =
      li_ $ p_ do
        link $ tagLink tag
        small_ $ " (" <> toHtml (show count) <> " posts)"


archiveEntry :: IncludeTags -> Post -> Html ()
archiveEntry includeTags Post{ synopsis, composed, tags, title, slug } =
   section_ [ class_ "margin-bottom-half" ] do
      date composed
      h2_ $ a_ [ href_ (url (Routes.PageR slug)) ] (toHtml title)
      p_ (toHtml synopsis)
      when includeTags $
         p_ (tagLinks tags)

date :: Day -> Html ()
date theDate =
   div_ $ time_
       [ datetime_ ((Text.pack . showGregorian) theDate) ]
       $ toHtml (formatTime defaultTimeLocale "%d %B %Y" theDate)

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
    , pageTitle :: Text }

data Link a = Link
    { linkRoute :: Route a
    , linkAttributes :: [Attribute]
    , linkText :: Html () }

page :: PageContent -> Html ()
page PageContent{mainContent, pageTitle} = do
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
            title_ $ toHtml $ pageTitle <> " … ‹three dots›"
            link_
                [ rel_ "stylesheet" , href_ "/fonts/fonts.css" ]
            link_
                [ rel_ "stylesheet" , href_ "/styles/three-dots.css" ]
        body_ do
            header_ do
              h1_ $ a_ [ href_ "/" ] "Three dots …"
            main_ mainContent
            footer_ footerContent

link :: Link a -> Html ()
link Link{ linkText, linkAttributes, linkRoute } =
   a_ (href_ (url linkRoute) : linkAttributes) linkText
