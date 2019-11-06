module Templates ( Html
                 , archive, post, tagsList
                 , page
                 , IncludeTags ) where

import Introit hiding ( for_ ) -- I use 'Lucid.for_', meaning the HTML5 attribute, below
import qualified Text

import Control.Monad ( when )
import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Lucid
import Lucid.Base ( makeAttribute )
import qualified Text.MMark as MMark

import Post
import Routes ( url )
import qualified Routes


type IncludeTags = Bool

archive :: IncludeTags -> [Post] -> PageContent
archive includeTags posts =
  let mainContent =
        foldrMapM (archiveEntry includeTags) posts
      footerContent = Just $ address_ do
         "If you wish to comment, enquire, inquire, or muse upon anything on this website, "
         "please send me an electronic mail message at "
         a_ [ href_ "mailto:alan.rempel@gmail.com" ] "Alan Rempel ‹alan\x200B•rempel\x200B@gmail\x200B•com›"
         "."
  in PageContent
       { mainContent
       , footerContent
       , pageDescription = "A blog by Alan Rempel, featuring posts both fictional and non-fictional on a variety of topics."
       , pageTitle = "All Posts"}

post :: IncludeTags -> Post -> PageContent
post includeTags Post{ content, composed, tags, title, description, slug } =
    let
        mainContent =
            article_ do
                header_ do
                  date composed
                  h1_ $ a_ [ href_ (url slug) ] (toHtml title)
                MMark.render content
                when includeTags $ footer_ $
                    p_ (tagLinks tags)
    in PageContent
        { mainContent
        , footerContent = Nothing
        , pageDescription = description
        , pageTitle = title }

tagsList :: [(Tag, Int)] -> Html ()
tagsList tagsWithCounts = do
    h1_ "Tags"
    p_ $ ul_ $
      foldMap tagWithCount tagsWithCounts
  where
    tagWithCount :: (Tag, Int) -> Html ()
    tagWithCount (tag, count) =
      li_ $ p_ do
        tagLink tag
        " (" <> toHtml (show count) <> " posts)"


archiveEntry :: IncludeTags -> Post -> Html ()
archiveEntry includeTags Post{ synopsis, composed, tags, title, slug } =
   section_ [ class_ "post-listing" ] do
      date composed
      h2_ [ class_ "post-title" ] $ a_ [ href_ (url slug) ] (toHtml title)
      p_ [ class_ "post-synopsis" ] (toHtml synopsis)
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
  "Tagged as " <> mconcat (intersperse ", " (map tagLink theTags))

tagLink :: Tag -> Html ()
tagLink tagName =
  a_ [ href_ (url (Routes.TagR tagName)) ] $ toHtml tagName

data PageContent = PageContent
    { mainContent :: Html ()
    , footerContent :: Maybe (Html ())
    , pageDescription :: Text
    , pageTitle :: Text }

page :: PageContent -> Html ()
page PageContent{mainContent, footerContent, pageDescription, pageTitle} = do
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
            meta_
                [ name_ "description", content_ pageDescription ]
            link_ [ rel_ "stylesheet" , href_ "/styles/normalize.css" ]
            link_ [ rel_ "stylesheet" , href_ "/fonts/fonts.css" ]
            link_ [ rel_ "stylesheet" , href_ "/styles/three-dots.css" ]
            script_
              [ src_ "/scripts/colour-scheme.js"
              , defer_ "" {- re. defer_: I wish boolean attributes in Lucid could be written without the argument -} ]
              ("" :: String) {- Also, it would be nice if this argument were optional for script_ -}
        body_ [ class_ "colour-scheme-auto" ] do
          div_ [ class_ "container" ] do
            header_ do
              div_ [ class_ "row" ] do
                h1_ [ class_ "slightly-bigger semibold" ] $ a_ [ href_ "/" ] "Three dots …"
                section_ do
                  h2_ [ class_ "slightly-bigger light" ] $
                    button_ [ type_ "button", class_ "unstyled", id_ "settings-toggle" ] "Settings"
                  settings
            main_ mainContent
            maybe mempty footer_ footerContent

settings :: Html ()
settings =
  div_ [ id_ "settings-panel", hidden_ "" ] do
    div_ do
      label_ [ for_ "colour-scheme-select" ] "Colour scheme:"
      select_ [ id_ "colour-scheme-select", required_ "" {- See above re. defer_ -} ] do
        option_ [ value_ "auto", selected_ "" {- See above re. defer_ -} ] "System setting"
        option_ [ value_ "light" ] "Light"
        option_ [ value_ "dark" ] "Dark"
    button_ [ class_ "unstyled", id_ "settings-close", title_ "Close", makeAttribute "aria-label" "Close" ] "×"

