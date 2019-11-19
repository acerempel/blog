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
import qualified Text.MMark as MMark

import Post
import Routes ( url )
import qualified Routes


type IncludeTags = Bool

archive :: IncludeTags -> [Post] -> PageContent
archive includeTags posts =
  let mainContent =
        foldrMapM (archiveEntry includeTags) posts
      pageDescription =
        Just "A blog by Alan Rempel, featuring posts both fictional and non-fictional on a variety of topics."
  in PageContent
       { mainContent
       , pageDescription
       , pageTitle = "All Posts"}

post :: IncludeTags -> Post -> PageContent
post includeTags Post{ content, composed, tags, mTitle, description, slug } =
    let
        pageTitle = fromMaybe (firstNWords 5 content) mTitle
        mainContent =
            article_ [ class_ "post full" ] do
                header_ do
                  date composed
                  whenMaybe mTitle \title ->
                    h2_ [ class_ "title" ] $
                      a_ [ href_ (url slug) ] (toHtml title)
                MMark.render content
                when includeTags $ footer_ $
                    p_ (tagLinks tags)
    in PageContent
        { mainContent
        , pageDescription = description
        , pageTitle }

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
archiveEntry includeTags Post{ mSynopsis, composed, content, tags, mTitle, slug } =
   article_ [ class_ ("post " <> if showFullPost then "full" else "summary") ] do
      date composed
      whenMaybe mTitle \title ->
        h2_ [ class_ "title" ] $ a_ [ href_ (url slug) ] (toHtml title)
      whenMaybe mSynopsis \synopsis ->
        p_ [ class_ "synopsis" ] (toHtmlRaw synopsis)
      when showFullPost $
        MMark.render content
      when includeTags $
         p_ (tagLinks tags)
  where
    showFullPost =
      mTitle == Nothing || mSynopsis == Nothing

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
    , pageDescription :: Maybe Text
    , pageTitle :: Text }

page :: PageContent -> Html ()
page PageContent{mainContent, pageDescription, pageTitle} = do
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
            whenMaybe pageDescription \description ->
              meta_ [ name_ "description", content_ description ]
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
                h1_ [ class_ "slightly-bigger semibold" ] $ a_ [ href_ "/" ] "Three dots …"
            main_ mainContent
            footer_ do
              settings

settings :: Html ()
settings =
  section_ do
    h2_ [ class_ "semibold" ] "Appearance"
    p_ do
      label_ [ for_ "colour-scheme-select" ] "Colour scheme:"
      select_ [ id_ "colour-scheme-select", required_ "" {- See above re. defer_ -} ] do
        option_ [ value_ "auto", selected_ "" {- See above re. defer_ -} ] "System setting"
        option_ [ value_ "light" ] "Light"
        option_ [ value_ "dark" ] "Dark"

whenMaybe :: Monoid m => Maybe a -> (a -> m) -> m
whenMaybe mThing f =
  maybe mempty f mThing
